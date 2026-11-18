open! Hermetic
open Kernel
open Address

(* A group counts in [lasting] the holds that last, and keeps in [untils]
   when the others end, on [now]; of those in TIME_WAIT, [time_waits] counts
   by when they end. [lowest] is a port of the dynamic range at or below the
   lowest one nothing holds: every port below it is held; [highest] is one at
   or above the highest free one. [next] is where macOS looks for its next
   TCP port, counting up, and [down] where Linux looks for the next port of a
   connect that binds no address, counting down. *)
module Untils = Map.Make (Mtime)

type group = {
  ip : Eio.Net.Ipaddr.v4v6;
  wildcard : bool;
  reuse : bool;
  listening : bool;
  connected : bool;
  toward : (Eio.Net.Ipaddr.v4v6 * int) option;
  mutable lasting : int;
  mutable untils : int Untils.t;
  mutable time_waits : int Untils.t;
}

type hold = {
  group : group;
  mutable until : Mtime.t option;
  mutable time_wait : bool;
  mutable released : bool;
}

type t = {
  os : os;
  held : (int, group list) Hashtbl.t;
  mutable lowest : int;
  mutable highest : int;
  mutable next : int;
  mutable down : int;
  now : (unit -> Mtime.t) option;
}

let v ~os ?now () =
  {
    os;
    held = Hashtbl.create 64;
    lowest = 49152;
    highest = 65535;
    next = 49152;
    down = 65535;
    now;
  }

(* TCP_TIMEWAIT_LEN on Linux, twice macOS's 15-second MSL, and the
   default tcp_fin_timeout of both. *)
let time_wait t =
  match t.os with
  | Linux -> Mtime.Span.(60 * s)
  | Macos -> Mtime.Span.(30 * s)
  | Windows -> Mtime.Span.zero

let fin_timeout = Mtime.Span.(60 * s)

(* Linux's tcp_tw_reuse_delay: a connect may take a 4-tuple held in
   TIME_WAIT once this has passed since the peer's FIN. *)
let tw_reuse_delay = Mtime.Span.(1 * s)

let freed t port =
  if port >= 49152 && port < t.lowest then t.lowest <- port;
  if port <= 65535 && port > t.highest then t.highest <- port

let incr u m = Untils.update u (fun c -> Some (Option.value c ~default:0 + 1)) m

let decr u m =
  Untils.update u (function Some c when c > 1 -> Some (c - 1) | _ -> None) m

let untils_add u g = g.untils <- incr u g.untils
let untils_remove u g = g.untils <- decr u g.untils

(* Whether a hold of [g] is live, once those whose TIME_WAIT has ended are
   dropped. *)
let live t g =
  (match t.now with
  | None -> ()
  | Some now ->
      let now = now () in
      let rec prune () =
        match Untils.min_binding_opt g.untils with
        | Some (u, _) when not (Mtime.is_earlier now ~than:u) ->
            g.untils <- Untils.remove u g.untils;
            g.time_waits <- Untils.remove u g.time_waits;
            prune ()
        | _ -> ()
      in
      prune ());
  g.lasting > 0 || not (Untils.is_empty g.untils)

(* The groups on [port] that hold it. *)
let at t port =
  match Hashtbl.find_opt t.held port with
  | None -> []
  | Some groups -> (
      match List.filter (live t) groups with
      | [] ->
          Hashtbl.remove t.held port;
          freed t port;
          []
      | live ->
          if List.compare_lengths live groups <> 0 then
            Hashtbl.replace t.held port live;
          live)

let same_toward a b =
  match (a, b) with
  | None, None -> true
  | Some (ip, port), Some (ip', port') -> same ip ip' && port = port'
  | _ -> false

let add t ?(wildcard = false) ?(reuse = false) ?(listening = false)
    ?(connected = false) ?toward (ip, port) =
  let groups = at t port in
  let alike g =
    same g.ip ip && g.wildcard = wildcard && g.reuse = reuse
    && g.listening = listening && g.connected = connected
    && same_toward g.toward toward
  in
  let group =
    match List.find_opt alike groups with
    | Some g -> g
    | None ->
        let g =
          {
            ip;
            wildcard;
            reuse;
            listening;
            connected;
            toward;
            lasting = 0;
            untils = Untils.empty;
            time_waits = Untils.empty;
          }
        in
        Hashtbl.replace t.held port (g :: groups);
        g
  in
  group.lasting <- group.lasting + 1;
  { group; until = None; time_wait = false; released = false }

let forget h u =
  untils_remove u h.group;
  if h.time_wait then h.group.time_waits <- decr u h.group.time_waits

let remove t port h =
  if not h.released then begin
    h.released <- true;
    match h.until with
    | None -> h.group.lasting <- h.group.lasting - 1
    | Some u -> forget h u
  end;
  ignore (at t port : group list)

(* [h] lets go at [until], or later if it already would; [time_wait] when
   it lingers in TIME_WAIT. *)
let set_until h ~time_wait until =
  let set () =
    untils_add until h.group;
    if time_wait then h.group.time_waits <- incr until h.group.time_waits;
    h.until <- Some until;
    h.time_wait <- time_wait
  in
  match h.until with
  | _ when h.released -> ()
  | Some u when Mtime.is_later u ~than:until -> ()
  | Some u ->
      forget h u;
      set ()
  | None ->
      h.group.lasting <- h.group.lasting - 1;
      set ()

(* A bind at [ip] conflicts with a hold at an overlapping address, unless
   both sockets have [SO_REUSEADDR] and the held one does not listen, as
   Linux has it. A client bound at a wildcard address and connected holds
   its source address, and conflicts with a bind at a wildcard address
   too. *)
let in_use_linux t ~reuse (ip, port) =
  let wild = same (unmap ip) (any (unmap ip)) in
  List.exists
    (fun g ->
      (ip_overlaps (ip, port) (g.ip, port) || (g.wildcard && wild))
      && not (reuse && g.reuse && not g.listening))
    (at t port)

(* The IPv4 address at which a socket bound at [ip] takes IPv4 traffic:
   its own, the one an IPv4-mapped address maps, or the wildcard one for
   the dual-stack IPv6 wildcard address. *)
let v4_view (ip : Eio.Net.Ipaddr.v4v6) =
  if String.length (ip :> string) = 4 then Some ip
  else if same ip Eio.Net.Ipaddr.V6.any then
    Some (Eio.Net.Ipaddr.V4.any :> Eio.Net.Ipaddr.v4v6)
  else if is_v4 ip then Some (unmap ip)
  else None

(* macOS looks a bind up as the BSDs do. With [SO_REUSEADDR] only a socket
   that is not connected, at the same address, conflicts; without, any at
   an overlapping one, connected or not. An IPv6 socket looks up the IPv4
   side of the wildcard or an IPv4-mapped address too, where an IPv4
   socket at the IPv4 wildcard address does not conflict with it. *)
let in_use_macos t ~reuse ((ip : Eio.Net.Ipaddr.v4v6), port) =
  let v6 = String.length (ip :> string) = 16 in
  let wild x = same x (any x) in
  let matches g a b =
    if reuse then (not g.connected) && same a b
    else same a b || wild a || wild b
  in
  let conflict g =
    let g6 = String.length (g.ip :> string) = 16 in
    let on_v4 =
      match (v4_view ip, v4_view g.ip) with
      | Some a, Some b when (not v6) || wild ip || is_v4 ip ->
          matches g a b && not (v6 && wild b && not g6)
      | _ -> false
    in
    on_v4 || (v6 && g6 && matches g ip g.ip)
  in
  List.exists conflict (at t port)

(* Windows looks a bind up at its own address only, the IPv4 and IPv6
   ones apart as IPV6_V6ONLY is on: a socket bound there that is not
   connected conflicts, unless both have [SO_REUSEADDR], and the second
   then takes the port over. One with it where the held one has none is
   denied ([denied]). A closed socket holds nothing. *)
let held_windows t (ip, port) =
  List.filter (fun g -> (not g.connected) && same g.ip ip) (at t port)

let in_use_windows t ~reuse a =
  List.exists (fun g -> not (reuse && g.reuse)) (held_windows t a)

let denied t ?(reuse = false) a =
  t.os = Windows && reuse
  && List.exists (fun g -> not g.reuse) (held_windows t a)

let in_use t ?(reuse = false) a =
  match t.os with
  | Linux -> in_use_linux t ~reuse a
  | Macos -> in_use_macos t ~reuse a
  | Windows -> in_use_windows t ~reuse a

let is_free t port = at t port = []

(* The lowest port of the dynamic range nothing holds, at any address. *)
let free t =
  let rec find port =
    if port > 65535 then None
    else if not (is_free t port) then find (port + 1)
    else begin
      t.lowest <- port;
      Some port
    end
  in
  find t.lowest

(* The next port of the dynamic range nothing holds, counting up from the
   last one taken and round, as macOS takes TCP ports. *)
let free_next t =
  let rec find port k =
    if k > 65535 - 49152 then None
    else
      let port = if port > 65535 then 49152 else port in
      if not (is_free t port) then find (port + 1) (k + 1)
      else begin
        t.next <- port + 1;
        Some port
      end
  in
  find t.next 0

(* Whether a connect toward [toward] that binds no address may take a port
   [g] holds, on Linux: one that another such connect holds toward another
   destination, as Linux keeps those unique per 4-tuple, or toward the same
   one once each of its holds is in TIME_WAIT from more than
   [tw_reuse_delay] after the peer's FIN, as Linux's [tcp_tw_reuse] lets
   it on loopback. *)
let shares t ~toward g =
  match g.toward with
  | None -> false
  | Some _ when not (same_toward g.toward (Some toward)) -> true
  | Some _ -> (
      g.lasting = 0
      && Untils.equal Int.equal g.untils g.time_waits
      &&
      match (t.now, Untils.max_binding_opt g.untils) with
      | Some now, Some (u, _) -> (
          match Mtime.sub_span u (time_wait t) with
          | Some fin ->
              let from = Mtime.add_span fin tw_reuse_delay in
              Option.fold ~none:false
                ~some:(fun from -> Mtime.is_later (now ()) ~than:from)
                from
          | None -> false)
      | _ -> false)

(* The port a connect toward [toward] that binds no address takes on
   Linux: the next one down from the last such connect's, round the
   dynamic range, that only connects toward other destinations hold, as
   Linux shares an ephemeral port between them. *)
let free_toward t toward =
  let rec find port n =
    if n = 0 then None
    else
      let port = if port < 49152 then 65535 else port in
      if List.for_all (shares t ~toward) (at t port) then begin
        t.down <- port - 1;
        Some port
      end
      else find (port - 1) (n - 1)
  in
  find t.down (65535 - 49152 + 1)

(* The highest port of the dynamic range nothing holds. *)
let free_down t =
  let rec find port =
    if port < 49152 then None
    else if not (is_free t port) then find (port - 1)
    else begin
      t.highest <- port;
      Some port
    end
  in
  find t.highest

let os t = t.os
let now t = t.now
