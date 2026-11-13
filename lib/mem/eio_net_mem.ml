open! Hermetic

type Eio.Exn.Backend.t +=
  | Closed
  | No_listener
  | Address_in_use
  | No_destination
  | Message_too_long
  | Invalid_port
  | Network_unreachable
  | Address_family
  | Wrong_socket_type
  | Not_connected
  | Address_not_available
  | Name_too_long
  | Invalid_address
  | Broadcast
  | Host_unreachable

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Closed ->
        Format.pp_print_string ppf "Closed (eio-net.mem)";
        true
    | No_listener ->
        Format.pp_print_string ppf "No listener bound (eio-net.mem)";
        true
    | Address_in_use ->
        Format.pp_print_string ppf "Address in use (eio-net.mem)";
        true
    | No_destination ->
        Format.pp_print_string ppf "No destination (eio-net.mem)";
        true
    | Message_too_long ->
        Format.pp_print_string ppf "Message too long (eio-net.mem)";
        true
    | Invalid_port ->
        Format.pp_print_string ppf "Invalid port (eio-net.mem)";
        true
    | Network_unreachable ->
        Format.pp_print_string ppf "Network unreachable (eio-net.mem)";
        true
    | Address_family ->
        Format.pp_print_string ppf "Address family not supported (eio-net.mem)";
        true
    | Wrong_socket_type ->
        Format.pp_print_string ppf "Wrong socket type (eio-net.mem)";
        true
    | Not_connected ->
        Format.pp_print_string ppf "Not connected (eio-net.mem)";
        true
    | Address_not_available ->
        Format.pp_print_string ppf "Address not available (eio-net.mem)";
        true
    | Name_too_long ->
        Format.pp_print_string ppf "File name too long (eio-net.mem)";
        true
    | Invalid_address ->
        Format.pp_print_string ppf "Invalid address (eio-net.mem)";
        true
    | Broadcast ->
        Format.pp_print_string ppf "Broadcast not allowed (eio-net.mem)";
        true
    | Host_unreachable ->
        Format.pp_print_string ppf "No route to host (eio-net.mem)";
        true
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)

(* The kernel mem follows where Linux and macOS differ. *)
type os = Linux | Macos

let kernel = function None | Some `Linux -> Linux | Some `Macos -> Macos

(* The clock a network was given: TCP's timers run on it. *)
type timer = { now : unit -> Mtime.t; sleep_until : Mtime.t -> unit }

let timer clock =
  {
    now = (fun () -> Eio.Time.Mono.now clock);
    sleep_until = (fun t -> Eio.Time.Mono.sleep_until clock t);
  }

let mapped_prefix = "\000\000\000\000\000\000\000\000\000\000\xff\xff"

(* The IPv4 address an IPv4-mapped IPv6 address stands for, or [ip]. *)
let unmap (ip : Eio.Net.Ipaddr.v4v6) =
  let raw = (ip :> string) in
  if String.length raw = 16 && String.equal (String.sub raw 0 12) mapped_prefix
  then Eio.Net.Ipaddr.of_raw (String.sub raw 12 4)
  else ip

let is_v4 ip = String.length (unmap ip :> string) = 4

let any ip =
  Eio.Net.Ipaddr.fold ip
    ~v4:(fun _ -> Eio.Net.Ipaddr.V4.any)
    ~v6:(fun _ -> Eio.Net.Ipaddr.V6.any)

(* The IPv4-mapped IPv6 form of an IPv4 address. *)
let map (ip : Eio.Net.Ipaddr.v4v6) =
  Eio.Net.Ipaddr.of_raw (mapped_prefix ^ (ip :> string))

let same (x : Eio.Net.Ipaddr.v4v6) (y : Eio.Net.Ipaddr.v4v6) =
  String.equal (x :> string) (y :> string)

(* The addresses, best first, at which a socket takes a connection or a
   datagram sent to [ip]: the address itself, in either form if it is IPv4,
   then the wildcard address of its family, and for IPv4 the IPv6 wildcard
   address of a dual-stack socket. A wildcard destination stands for the
   loopback address of its family, as both kernels route it. *)
let reaching ip =
  let ip = unmap ip in
  if is_v4 ip then
    let ip = if same ip (any ip) then Eio.Net.Ipaddr.V4.loopback else ip in
    let v4_any = Eio.Net.Ipaddr.V4.any in
    [ ip; map ip; v4_any; map v4_any; Eio.Net.Ipaddr.V6.any ]
  else
    let ip = if same ip (any ip) then Eio.Net.Ipaddr.V6.loopback else ip in
    [ ip; Eio.Net.Ipaddr.V6.any ]

(* The address a socket that binds none sends from to [ip]: the loopback
   interface's own address of its family, as Linux picks it for any address
   of 127.0.0.0/8. *)
let source_to ip : Eio.Net.Ipaddr.v4v6 =
  if is_v4 ip then (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6)
  else (Eio.Net.Ipaddr.V6.loopback :> Eio.Net.Ipaddr.v4v6)

(* The destination a socket bound at [bound] reaches for [dst], as Linux
   routes a wildcard one: the IPv4 wildcard address stands for the IPv4
   address the socket is bound at, in [dst]'s form, and the IPv6 one from an
   IPv4-mapped address for ::ffff:127.0.0.1. Otherwise {!reaching} takes a
   wildcard address for the loopback address of its family. *)
let toward ~os ~(bound : Eio.Net.Ipaddr.v4v6) (dst : Eio.Net.Ipaddr.v4v6) =
  let b = unmap bound in
  if same dst Eio.Net.Ipaddr.V6.any then
    if os = Linux && String.length (bound :> string) = 16 && is_v4 bound then
      map Eio.Net.Ipaddr.V4.loopback
    else dst
  else if is_v4 dst && same (unmap dst) Eio.Net.Ipaddr.V4.any then
    if is_v4 b && not (same b (any b)) then
      if String.length (dst :> string) = 16 then map b else b
    else dst
  else dst

(* [ip] as a socket bound at [own] sees it: IPv4 to an IPv4 socket, and
   IPv4-mapped to an IPv6 one. *)
let seen_by (own : Eio.Net.Ipaddr.v4v6) ip =
  if String.length (own :> string) = 4 then unmap ip
  else if is_v4 ip then map (unmap ip)
  else ip

(* Two IP addresses of one port overlap when they are equal, one is the
   wildcard address of the other's family, or one is the IPv6 wildcard address,
   which a dual-stack socket holds at IPv4 addresses too, as bind(2) on Linux
   has it. An IPv4-mapped address is the IPv4 address it maps. *)
let ip_overlaps (ip, port) (ip', port') =
  let dual x = same x Eio.Net.Ipaddr.V6.any in
  let x = unmap ip and y = unmap ip' in
  port = port'
  && (dual ip || dual ip'
     || (same (any x) (any y) && (same x y || same x (any x) || same y (any y)))
     )

let address_in_use () = Eio.Exn.create (Eio.Exn.X Address_in_use)
let fail code = raise (Eio.Exn.create (Eio.Exn.X code))
let first_byte (ip : Eio.Net.Ipaddr.v4v6) = Char.code (ip :> string).[0]
let multicast_v4 ip = first_byte ip >= 224 && first_byte ip <= 239

let broadcast (ip : Eio.Net.Ipaddr.v4v6) =
  String.equal (ip :> string) "\255\255\255\255"

(* The broadcast address of loopback's network, 127.0.0.0/8. *)
let lo_broadcast (ip : Eio.Net.Ipaddr.v4v6) =
  String.equal (ip :> string) "\127\255\255\255"

(* A link-local IPv6 address, or a multicast one of interface or link scope,
   which needs a scope that Eio's addresses do not carry. *)
let needs_scope (ip : Eio.Net.Ipaddr.v4v6) =
  let raw = (ip :> string) in
  String.length raw = 16
  && ((raw.[0] = '\xfe' && Char.code raw.[1] land 0xc0 = 0x80)
     || (raw.[0] = '\xff' && Char.code raw.[1] land 0x0f <= 2))

(* The network has the loopback interface only, which reaches 127.0.0.0/8,
   ::1 and the wildcard addresses, which stand for loopback, and routes IPv4
   multicast and broadcast: a host with loopback only has no route to any
   other address, an IPv4-compatible one such as ::127.0.0.1 and an IPv6
   multicast one included. *)
let off_loopback ip =
  let ip = unmap ip in
  if is_v4 ip then
    not
      (first_byte ip = 127
      || same ip (any ip)
      || multicast_v4 ip || broadcast ip)
  else not (same ip Eio.Net.Ipaddr.V6.loopback || same ip (any ip))

(* An address of 127.0.0.0/8 other than 127.0.0.1, which macOS's loopback
   interface does not take. *)
let unanswered (ip : Eio.Net.Ipaddr.v4v6) =
  let ip = unmap ip in
  is_v4 ip
  && first_byte ip = 127
  && (not (same ip (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6)))
  && not (lo_broadcast ip)

(* eio_posix raises bind(2)'s errors from [listen] and [datagram_socket] as a
   raw [Unix.Unix_error], and a [connect]'s as an {!Eio.Io} error. *)
let bind_fail code =
  raise
    (Posix.bind_error
       (match code with
       | Address_in_use -> `In_use
       | Address_not_available -> `Not_available
       | Address_family -> `Family
       | _ -> `Invalid))

(* macOS's loopback interface has 127.0.0.1 and ::1 only, and needs a scope
   for its link-local address. A TCP socket binds no multicast or broadcast
   address, with EAFNOSUPPORT, and a UDP one no broadcast one. *)
let check_local_macos ~tcp ~fail ip =
  let group =
    if is_v4 ip then multicast_v4 ip || broadcast ip
    else (ip :> string).[0] = '\xff'
  in
  if group && tcp then fail Address_family
  else if is_v4 ip && broadcast ip then fail Address_not_available
  else if
    not
      (group
      || same ip (any ip)
      || same ip
           (if is_v4 ip then (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6)
            else Eio.Net.Ipaddr.V6.loopback))
  then fail Address_not_available

(* The network has the loopback interface only: an IPv4 socket binds an
   address of 127.0.0.0/8, the broadcast address or a multicast one, an IPv6
   one ::1, either the wildcard address of its family, and an IPv6 one the
   IPv4-mapped form of an IPv4 address it binds, as on Linux. An IPv6
   address that needs a scope fails with EINVAL, and so does an IPv6
   multicast one for TCP. *)
let check_local ~os ?(tcp = false) ?(fail = fail) ip =
  let ip = unmap ip in
  match os with
  | Macos -> check_local_macos ~tcp ~fail ip
  | Linux ->
      if needs_scope ip then fail Invalid_address;
      let local =
        if is_v4 ip then
          first_byte ip = 127
          || same ip (any ip)
          || multicast_v4 ip || broadcast ip
        else if (ip :> string).[0] = '\xff' then begin
          if tcp then fail Invalid_address;
          true
        end
        else same ip Eio.Net.Ipaddr.V6.loopback || same ip (any ip)
      in
      if not local then fail Address_not_available

(* A path with a NUL past its first byte, which is not an abstract address,
   or on macOS, which has no abstract addresses, any NUL. *)
let embedded_nul ~os path =
  match String.index_opt path '\000' with
  | Some i -> i > 0 || os = Macos
  | None -> false

(* OCaml's [Unix] refuses a path that [sun_path], 108 bytes on Linux and
   104 on macOS, cannot hold with its NUL, and one with a NUL past its first
   byte with ENOENT, as no file has that name: raw from a bind, as
   [Eio.Fs.Not_found] from a [connect] or a [send]. macOS binds no empty
   path, with EINVAL. *)
let check_path ~os ?(raw = false) path =
  if String.length path >= match os with Linux -> 108 | Macos -> 104 then
    if raw then raise (Posix.bind_error (`Name_too_long path))
    else fail Name_too_long
  else if embedded_nul ~os path then
    if raw then raise (Posix.bind_error (`No_file path))
    else raise (Eio.Fs.err (Not_found No_listener))
  else if os = Macos && path = "" then
    if raw then raise (Posix.bind_error `Invalid) else fail Invalid_address

(* The error a socket at [src] meets reaching [dst] across address families,
   IPv6 against IPv4-mapped, as on Linux. *)
let cross_family ~os (src : Eio.Net.Ipaddr.v4v6) (dst : Eio.Net.Ipaddr.v4v6) =
  if String.length (src :> string) <> 16 || String.length (dst :> string) <> 16
  then None
  else if os = Macos then
    if (not (same src Eio.Net.Ipaddr.V6.any)) && is_v4 src <> is_v4 dst then
      Some Address_family
    else None
  else if is_v4 src && not (is_v4 dst) then Some Address_family
  else if
    (not (is_v4 src)) && (not (same src Eio.Net.Ipaddr.V6.any)) && is_v4 dst
  then Some Network_unreachable
  else None

(* The holds sockets of one kind have on each port, kept as they bind and
   close, so a bind checks only the holds on its port. A hold is an address,
   whether its socket has [SO_REUSEADDR], whether it listens, and, once it
   lingers in TIME_WAIT, until when on [now]. Holds alike in all but when
   they end share a group, which counts those that last and keeps when the
   others end, so a port that many connections held costs a bind the groups
   on it, not the connections. On Linux, a connect that binds no address
   holds its port [toward] its destination, and of those in TIME_WAIT,
   [time_waits] counts by when they end. [lowest] is a port of the dynamic
   range (RFC 6335, section 6) at or below the lowest one nothing holds:
   every port below it is held; [highest] is one at or above the highest
   free one. [next] is where macOS looks for its next TCP port, counting up,
   and [down] where Linux looks for the next port of a connect that binds no
   address, counting down. *)
module Ports = struct
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

  let fin_timeout = Mtime.Span.(60 * s)

  (* Linux's tcp_tw_reuse_delay: a connect may take a 4-tuple held in
     TIME_WAIT once this has passed since the peer's FIN. *)
  let tw_reuse_delay = Mtime.Span.(1 * s)

  let freed t port =
    if port >= 49152 && port < t.lowest then t.lowest <- port;
    if port <= 65535 && port > t.highest then t.highest <- port

  let count n = function None -> n | Some m -> m + n

  let untils_add u g =
    g.untils <- Untils.update u (fun c -> Some (count 1 c)) g.untils

  let decr u m =
    Untils.update u (function Some c when c > 1 -> Some (c - 1) | _ -> None) m

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
      if time_wait then
        h.group.time_waits <-
          Untils.update until (fun c -> Some (count 1 c)) h.group.time_waits;
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

  let in_use t ?(reuse = false) a =
    match t.os with
    | Linux -> in_use_linux t ~reuse a
    | Macos -> in_use_macos t ~reuse a

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
end

(* A Unix path no socket binds has no file, as Eio's backends remove a
   listener's file when the switch it listened under finishes. *)
let no_file () = Eio.Fs.err (Not_found No_listener)

(* The Unix-domain paths that hold a socket file, shared by stream and
   datagram sockets as a file system shares them. A socket's file stays once
   it closes, [Stale], as Eio's backends leave a datagram socket's file, until
   it is removed. *)
module Files = struct
  type kind = Listener | Datagram | Client | Stale

  (* A path, and for an abstract address whether its socket is a stream
     one. *)
  type t = {
    mutex : Mutex.t;
    files : (string * bool option, kind) Hashtbl.t;
    mutable autobind : int;
  }

  let v () = { mutex = Mutex.create (); files = Hashtbl.create 8; autobind = 0 }

  (* A path that starts with a NUL is in Linux's abstract namespace, which has
     no files, and a name space per socket type. *)
  let abstract path = String.length path > 0 && path.[0] = '\000'
  let key ~stream path = (path, if abstract path then Some stream else None)

  let is_stream = function
    | Datagram -> false
    | Listener | Client | Stale -> true

  (* What is at [path] for a socket of the type [stream] says: at an abstract
     address, only a socket of that type. *)
  let find t ~stream path =
    Mutex.protect t.mutex (fun () ->
        Hashtbl.find_opt t.files (key ~stream path))

  (* The empty path binds an abstract address no socket holds, as Linux
     autobinds one: a NUL and five hexadecimal digits. *)
  let autobind t =
    Mutex.protect t.mutex @@ fun () ->
    let rec next () =
      let name = Fmt.str "\000%05x" t.autobind in
      t.autobind <- (t.autobind + 1) land 0xfffff;
      let held stream = Hashtbl.mem t.files (key ~stream name) in
      if held true || held false then next () else name
    in
    next ()

  (* [path], or for the empty path the address it autobinds. *)
  let bound t path = if path = "" then autobind t else path

  (* Binds [path] unless a file is there; [take_over] binds over a file, and
     an abstract address has none. *)
  let bind t ?(take_over = false) path kind =
    let k = key ~stream:(is_stream kind) path in
    Mutex.protect t.mutex @@ fun () ->
    ((take_over && not (abstract path)) || not (Hashtbl.mem t.files k))
    && begin
      Hashtbl.replace t.files k kind;
      true
    end

  (* The socket of [kind] at [path] closes and leaves its file, or with
     [remove], or at an abstract address, takes it away. *)
  let leave ?(remove = false) t path kind =
    let k = key ~stream:(is_stream kind) path in
    Mutex.protect t.mutex @@ fun () ->
    match Hashtbl.find_opt t.files k with
    | Some k' when k' = kind ->
        if remove || abstract path then Hashtbl.remove t.files k
        else Hashtbl.replace t.files k Stale
    | Some _ | None -> ()
end

(* Socket options, as the kernel [os] names answers them on a host with
   loopback only: what a socket reads back, defaults included, with the
   kernel's rounding and limits, and the error eio_posix raises where the
   kernel refuses an option for the socket's family or type. Options whose
   effect mem has no model of are kept and read back. The send and receive
   buffers size the queues: [resized] tells the socket's queues. *)
module Sockopt = struct
  module O = Eio.Net.Sockopt

  type family = V4 | V6 | Local
  type kind = Unconnected | Listener | Client | Server | Datagram

  type t = {
    os : os;
    family : family;
    mutable kind : kind;
    mutable v6only : bool;
    mutable debug : bool;
    mutable broadcast : bool;
    mutable reuseaddr : bool;
    mutable keepalive : bool;
    mutable dontroute : bool;
    mutable oobinline : bool;
    mutable reuseport : bool;
    mutable nodelay : bool;
    mutable sndbuf : int option;
    mutable rcvbuf : int option;
    mutable rcvlowat : int;
    mutable sndlowat : int option;
    mutable linger : int option;
    mutable rcvtimeo : float;
    mutable sndtimeo : float;
    mutable keepidle : int option;
    mutable keepintvl : int option;
    mutable keepcnt : int option;
    mutable congestion : string;
    mutable cork : bool;
    mutable user_timeout : int;
    mutable user_mss : int;
    mutable maxseg : int option;
    mutable linger2 : int;
    mutable defer_accept : int;
    mutable syncnt : int;
    mutable window_clamp : int option;
    mutable quickack : bool;
    mutable fastopen : int;
    mutable freebind : bool;
    mutable bind_no_port : bool;
    mutable port_range : int * int;
    mutable ttl : int;
    mutable mtu_discover : [ `Want | `Dont | `Do | `Probe ];
    mutable resized : unit -> unit;
  }

  let v ~os ~family kind =
    {
      os;
      family;
      kind;
      v6only = false;
      debug = false;
      broadcast = false;
      reuseaddr = false;
      keepalive = false;
      dontroute = false;
      oobinline = false;
      reuseport = false;
      nodelay = false;
      sndbuf = None;
      rcvbuf = None;
      rcvlowat = 1;
      sndlowat = None;
      linger = None;
      rcvtimeo = 0.;
      sndtimeo = 0.;
      keepidle = None;
      keepintvl = None;
      keepcnt = None;
      congestion = "cubic";
      cork = false;
      user_timeout = 0;
      user_mss = 0;
      maxseg = None;
      linger2 = 0;
      defer_accept = 0;
      syncnt = 6;
      window_clamp = None;
      quickack = true;
      fastopen = 0;
      freebind = false;
      bind_no_port = false;
      port_range = (0, 0);
      ttl = 64;
      mtu_discover = `Want;
      resized = ignore;
    }

  let family_of_ip (ip : Eio.Net.Ipaddr.v4v6) =
    if String.length (ip :> string) = 4 then V4 else V6

  (* Linux marks an IPv6 socket bound at an address other than the wildcard
     and the IPv4-mapped ones as IPv6 only. *)
  let bound t (ip : Eio.Net.Ipaddr.v4v6) =
    if
      t.os = Linux && t.family = V6
      && (not (is_v4 ip))
      && not (same ip (any ip))
    then t.v6only <- true

  let tcp t = t.family <> Local && t.kind <> Datagram
  let connected t = t.kind = Client || t.kind = Server

  (* The send and receive buffers each kernel gives a socket. *)
  let default_sndbuf t =
    match (t.os, t.family, t.kind) with
    | Linux, (V4 | V6), (Unconnected | Listener) -> 16384
    | Linux, (V4 | V6), (Client | Server) -> 2626560
    | Linux, _, _ -> 229376
    | Macos, (V4 | V6), (Unconnected | Listener) -> 131072
    | Macos, V4, Client -> 146988
    | Macos, V6, Client -> 146808
    | Macos, V4, Server -> 132792
    | Macos, V6, Server -> 132132
    | Macos, (V4 | V6), Datagram -> 9216
    | Macos, Local, Datagram -> 2048
    | Macos, Local, _ -> 8192

  let default_rcvbuf t =
    match (t.os, t.family, t.kind) with
    | Linux, (V4 | V6), (Unconnected | Listener | Client | Server) -> 131072
    | Linux, _, _ -> 229376
    | Macos, (V4 | V6), (Unconnected | Listener) -> 131072
    | Macos, V4, Client -> 408300
    | Macos, V6, Client -> 407800
    | Macos, V4, Server -> 132792
    | Macos, V6, Server -> 132132
    | Macos, (V4 | V6), Datagram -> 786896
    | Macos, Local, Datagram -> 4096
    | Macos, Local, _ -> 8192

  let sndbuf t = Option.value t.sndbuf ~default:(default_sndbuf t)
  let rcvbuf t = Option.value t.rcvbuf ~default:(default_rcvbuf t)

  (* macOS's maximum segment size before a connection, and that of each end
     of one over loopback, whose MTU is 16384. *)
  let default_maxseg t =
    match (t.family, t.kind) with
    | V4, Client -> 16332
    | V6, Client -> 16312
    | V4, Server -> 4024
    | V6, Server -> 4004
    | V6, _ -> 1024
    | _ -> 512

  (* Linux's maximum segment size of a connection over loopback, less the
     12 bytes of the timestamp option from a size the socket set. *)
  let linux_mss t =
    let base =
      match (t.family, t.kind) with
      | V4, Client -> 32741
      | V6, Client -> 32732
      | _ -> 32768
    in
    if t.user_mss > 0 then min base (t.user_mss - 12) else base

  let listener ~os : Eio.Net.Sockaddr.stream -> t = function
    | `Unix _ -> v ~os ~family:Local Listener
    | `Tcp (ip, _) ->
        let t = v ~os ~family:(family_of_ip ip) Listener in
        bound t ip;
        t

  let client ~os : Eio.Net.Sockaddr.stream -> t = function
    | `Unix _ -> v ~os ~family:Local Unconnected
    | `Tcp (ip, _) -> v ~os ~family:(family_of_ip ip) Unconnected

  let datagram ~os : Eio.Net.Sockaddr.datagram -> t = function
    | `Unix _ -> v ~os ~family:Local Datagram
    | `Udp (ip, _) ->
        let t = v ~os ~family:(family_of_ip ip) Datagram in
        bound t ip;
        t

  (* Becomes an end of a connection. *)
  let connect t kind =
    t.kind <- kind;
    match t.os with
    | Linux -> t.maxseg <- Some (linux_mss t)
    | Macos -> t.maxseg <- None

  (* What an accepted socket takes of its listener's options: on Linux a TCP
     one all but [TCP_DEFER_ACCEPT] and [TCP_FASTOPEN], a Unix-domain one
     none; on macOS all but the low-water marks and the timeouts, and over
     TCP the keepalive idle time. *)
  let accepted l =
    let fresh = v ~os:l.os ~family:l.family Server in
    let t =
      match (l.os, l.family) with
      | Linux, Local -> fresh
      | Linux, _ -> { l with kind = Server; defer_accept = 0; fastopen = 0 }
      | Macos, family ->
          {
            l with
            kind = Server;
            rcvlowat = 1;
            sndlowat = None;
            rcvtimeo = 0.;
            sndtimeo = 0.;
            keepidle = (if family = Local then l.keepidle else None);
          }
    in
    connect t Server;
    t.resized <- ignore;
    t

  let error op code = raise (Posix.sockopt_error op code)

  (* An option of Linux's that eio_posix does not find on macOS. *)
  let missing () = raise (Eio.Net.err Invalid_option)

  type level = Socket | Tcp | Ip | Ipv6 | Linux_tcp | Linux_ip

  let level : type a. a Eio.Net.Sockopt.t -> level = function
    | O.TCP_NODELAY | O.TCP_KEEPIDLE | O.TCP_KEEPINTVL | O.TCP_KEEPCNT
    | O.TCP_MAXSEG ->
        Tcp
    | O.TCP_CONGESTION | O.TCP_CORK | O.TCP_USER_TIMEOUT | O.TCP_LINGER2
    | O.TCP_DEFER_ACCEPT | O.TCP_SYNCNT | O.TCP_WINDOW_CLAMP | O.TCP_QUICKACK
    | O.TCP_FASTOPEN ->
        Linux_tcp
    | O.IP_TTL -> Ip
    | O.IP_FREEBIND | O.IP_BIND_ADDRESS_NO_PORT | O.IP_LOCAL_PORT_RANGE
    | O.IP_MTU | O.IP_MTU_DISCOVER ->
        Linux_ip
    | O.IPV6_ONLY -> Ipv6
    | _ -> Socket

  (* The kernel's refusal of an option at a level the socket's family or
     type lacks. On macOS an IP or TCP option on a Unix-domain socket is read
     as the local option of the same number, which [get] answers. *)
  let gate op t level =
    let fail code = error op code in
    let set = op = `Set in
    match (t.os, level, t.family) with
    | _, Socket, _ -> ()
    | Macos, (Linux_tcp | Linux_ip), _ -> missing ()
    | Linux, _, Local -> fail `Not_supported
    | Linux, (Tcp | Linux_tcp), V4 when t.kind = Datagram ->
        fail (if set then `No_option else `Not_supported)
    | Linux, (Tcp | Linux_tcp), V6 when t.kind = Datagram -> fail `No_option
    | Linux, Ipv6, V4 -> fail (if set then `No_option else `Not_supported)
    | Linux, _, _ -> ()
    | Macos, Ipv6, Local -> fail `Not_supported
    | Macos, _, Local -> if set then fail `Not_supported
    | Macos, Tcp, _ when t.kind = Datagram -> fail `Invalid
    | Macos, Ipv6, V4 | Macos, Ip, V6 -> fail `Invalid
    | Macos, _, _ -> ()

  (* What setsockopt(2) takes of a float: whole seconds and microseconds,
     cut as OCaml's Unix cuts them. Linux keeps whole milliseconds, rounded
     up, and a negative time as none; macOS refuses it. *)
  let timeout t f =
    if f < 0. then if t.os = Linux then 0. else error `Set `Domain
    else
      let sec = Float.to_int f in
      let usec = Float.to_int (1e6 *. (f -. Float.of_int sec)) in
      let sec, usec =
        match t.os with
        | Linux ->
            let ms = (usec + 999) / 1000 in
            if ms = 1000 then (sec + 1, 0) else (sec, ms * 1000)
        | Macos -> (sec, usec)
      in
      Float.of_int sec +. (Float.of_int usec /. 1e6)

  (* Linux's [TCP_DEFER_ACCEPT] is a number of SYN-ACK retransmissions, the
     first after a second, each after twice as long, at most 120 seconds. *)
  let secs_to_retrans secs =
    if secs <= 0 then 0
    else
      let rec go res timeout period =
        if secs > period && res < 255 then
          let timeout = min (2 * timeout) 120 in
          go (res + 1) timeout (period + timeout)
        else res
      in
      go 1 1 1

  let retrans_to_secs n =
    if n <= 0 then 0
    else
      let rec go n timeout period =
        if n <= 1 then period
        else
          let timeout = min (2 * timeout) 120 in
          go (n - 1) timeout (period + timeout)
      in
      go n 1 1

  (* Linux takes a buffer size as unsigned, at most [wmem_max] or
     [rmem_max], doubles it for its bookkeeping and keeps a minimum; macOS
     keeps it as it is, at most [kern.ipc.maxsockbuf]. *)
  let buffer t ~min:least n =
    match t.os with
    | Linux ->
        let n = if n < 0 || n > 7500000 then 7500000 else n in
        max (2 * n) least
    | Macos -> if n <= 0 then error `Set `Invalid else min n 8388608

  (* Up to the first NUL and 15 bytes, as Linux reads the name. *)
  let congestion name =
    let name =
      match String.index_opt name '\000' with
      | Some i -> String.sub name 0 i
      | None -> name
    in
    if String.length name > 15 then String.sub name 0 15 else name

  let non_negative name v =
    if v < 0 then Fmt.invalid_arg "%s must be non-negative, got %d" name v

  (* The checks Eio makes before it calls setsockopt(2). *)
  let check : type a. a Eio.Net.Sockopt.t -> a -> unit =
   fun opt v ->
    match opt with
    | O.TCP_KEEPIDLE -> non_negative "O.TCP_KEEPIDLE" v
    | O.TCP_KEEPINTVL -> non_negative "O.TCP_KEEPINTVL" v
    | O.TCP_KEEPCNT -> non_negative "O.TCP_KEEPCNT" v
    | O.TCP_USER_TIMEOUT -> non_negative "O.TCP_USER_TIMEOUT" v
    | O.TCP_MAXSEG -> non_negative "O.TCP_MAXSEG" v
    | O.TCP_LINGER2 -> (
        match v with
        | Some n when n < 0 ->
            Fmt.invalid_arg "O.TCP_LINGER2 must be non-negative, got %d" n
        | _ -> ())
    | O.TCP_DEFER_ACCEPT -> non_negative "O.TCP_DEFER_ACCEPT" v
    | O.TCP_SYNCNT ->
        if v < 1 || v > 255 then
          Fmt.invalid_arg "O.TCP_SYNCNT must be between 1 and 255, got %d" v
    | O.TCP_WINDOW_CLAMP -> non_negative "O.TCP_WINDOW_CLAMP" v
    | O.TCP_FASTOPEN ->
        if v < 0 then
          Fmt.invalid_arg
            "O.TCP_FASTOPEN queue length must be non-negative, got %d" v
    | O.IP_LOCAL_PORT_RANGE ->
        let lower, upper = v in
        if lower < 0 || lower > 65535 then
          Fmt.invalid_arg
            "O.IP_LOCAL_PORT_RANGE lower bound must be 0-65535, got %d" lower;
        if upper < 0 || upper > 65535 then
          Fmt.invalid_arg
            "O.IP_LOCAL_PORT_RANGE upper bound must be 0-65535, got %d" upper;
        if lower <> 0 && upper <> 0 && lower > upper then
          Fmt.invalid_arg
            "O.IP_LOCAL_PORT_RANGE lower bound (%d) must be <= upper bound (%d)"
            lower upper
    | O.IP_TTL ->
        if v < 1 || v > 255 then
          Fmt.invalid_arg "O.IP_TTL must be between 1 and 255, got %d" v
    | O.IP_MTU -> invalid_arg "O.IP_MTU is a read-only socket option"
    | _ -> ()

  let set : type a. t -> a Eio.Net.Sockopt.t -> a -> unit =
   fun t opt v ->
    let opt = Posix.canonical opt in
    check opt v;
    gate `Set t (level opt);
    let invalid () = error `Set `Invalid in
    let linux = t.os = Linux in
    match opt with
    | _ when Posix.extra opt <> None -> error `Set `No_option
    | O.SO_DEBUG ->
        (* Only CAP_NET_ADMIN turns it on, on Linux. *)
        if linux && v then error `Set `Denied else t.debug <- v
    | O.SO_BROADCAST -> t.broadcast <- v
    | O.SO_REUSEADDR -> t.reuseaddr <- v
    | O.SO_KEEPALIVE -> t.keepalive <- v
    | O.SO_DONTROUTE -> t.dontroute <- v
    | O.SO_OOBINLINE -> t.oobinline <- v
    | O.SO_REUSEPORT -> t.reuseport <- v
    | O.SO_SNDBUF ->
        t.sndbuf <- Some (buffer t ~min:4608 v);
        t.resized ()
    | O.SO_RCVBUF ->
        t.rcvbuf <- Some (buffer t ~min:2304 v);
        t.resized ()
    | O.SO_RCVLOWAT -> (
        match t.os with
        | Linux ->
            let v = if v < 0 then 0x7fffffff else v in
            let v =
              if not (tcp t) then v
              else
                min v (match t.rcvbuf with Some b -> b / 2 | None -> 16777216)
            in
            t.rcvlowat <- (if v = 0 then 1 else v)
        | Macos -> if v < 1 then invalid () else t.rcvlowat <- min v (rcvbuf t))
    | O.SO_SNDLOWAT ->
        if linux then error `Set `No_option
        else if v < 1 then invalid ()
        else t.sndlowat <- Some (min v (sndbuf t))
    | O.SO_LINGER -> (
        match v with
        | Some n when (not linux) && (n < 0 || n > 32767) -> error `Set `Domain
        | v -> t.linger <- v)
    | O.SO_RCVTIMEO -> t.rcvtimeo <- timeout t v
    | O.SO_SNDTIMEO -> t.sndtimeo <- timeout t v
    | O.TCP_NODELAY -> t.nodelay <- v
    | O.TCP_KEEPIDLE ->
        if linux then
          if v < 1 || v > 32767 then invalid () else t.keepidle <- Some v
        else t.keepidle <- (if v = 0 then None else Some v)
    | O.TCP_KEEPINTVL ->
        if linux then
          if v < 1 || v > 32767 then invalid () else t.keepintvl <- Some v
        else t.keepintvl <- (if v = 0 then None else Some v)
    | O.TCP_KEEPCNT ->
        if linux then
          if v < 1 || v > 127 then invalid () else t.keepcnt <- Some v
        else t.keepcnt <- (if v = 0 then None else Some v)
    | O.TCP_MAXSEG -> (
        match t.os with
        | Linux ->
            if v <> 0 && (v < 88 || v > 32767) then invalid ()
            else t.user_mss <- v
        | Macos ->
            let current = Option.value t.maxseg ~default:(default_maxseg t) in
            if v <= 0 || v > current || v + 40 < 216 then invalid ()
            else t.maxseg <- Some v)
    | O.TCP_CONGESTION -> (
        match congestion v with
        | "" -> invalid ()
        | ("reno" | "cubic") as name -> t.congestion <- name
        | _ -> error `Set `Unknown)
    | O.TCP_CORK -> t.cork <- v
    | O.TCP_USER_TIMEOUT -> t.user_timeout <- v
    | O.TCP_LINGER2 ->
        t.linger2 <- (match v with None -> 0 | Some n -> min n 120)
    | O.TCP_DEFER_ACCEPT -> t.defer_accept <- secs_to_retrans v
    | O.TCP_SYNCNT -> if v > 127 then invalid () else t.syncnt <- v
    | O.TCP_WINDOW_CLAMP ->
        if v = 0 then
          if t.kind = Unconnected then t.window_clamp <- Some 0 else invalid ()
        else t.window_clamp <- Some (max v 1152)
    | O.TCP_QUICKACK -> t.quickack <- v
    | O.TCP_FASTOPEN ->
        (* The queue is at most [net.core.somaxconn] long. *)
        if connected t then invalid () else t.fastopen <- min v 4096
    | O.IP_FREEBIND -> t.freebind <- v
    | O.IP_BIND_ADDRESS_NO_PORT -> t.bind_no_port <- v
    | O.IP_LOCAL_PORT_RANGE -> t.port_range <- v
    | O.IP_TTL -> t.ttl <- v
    | O.IP_MTU_DISCOVER -> t.mtu_discover <- v
    | O.IPV6_ONLY -> if t.kind = Unconnected then t.v6only <- v else invalid ()
    | _ -> missing ()

  (* macOS's answer to an IP or TCP option on a Unix-domain socket: the local
     option of the same number, LOCAL_PEERCRED for [TCP_NODELAY] and
     LOCAL_PEERPID or LOCAL_PEERUUID for [TCP_MAXSEG] and [IP_TTL], read as
     an integer; mem has no peer process and gives 0. *)
  let local_macos : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt ->
    let fail code = error `Get code in
    match opt with
    | O.TCP_NODELAY -> (
        match t.kind with
        | Datagram -> fail `Invalid
        | Client | Server -> false
        | Unconnected | Listener -> fail `Not_connected)
    | O.TCP_MAXSEG -> if connected t then 0 else fail `Not_connected
    | O.IP_TTL -> if connected t then 0 else fail `Not_connected
    | _ -> fail `Not_supported

  let get : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt ->
    let opt = Posix.canonical opt in
    gate `Get t (level opt);
    let linux = t.os = Linux in
    match (Posix.extra opt, opt) with
    | Some Acceptconn, _ -> if linux then t.kind = Listener else missing ()
    | Some Error, _ -> 0
    | Some Type, _ -> if t.kind = Datagram then 2 else 1
    | None, _ when t.os = Macos && t.family = Local && level opt <> Socket ->
        local_macos t opt
    | None, opt -> (
        match opt with
        | O.SO_DEBUG -> t.debug
        | O.SO_BROADCAST -> t.broadcast
        | O.SO_REUSEADDR -> t.reuseaddr
        | O.SO_KEEPALIVE -> t.keepalive
        | O.SO_DONTROUTE -> t.dontroute
        | O.SO_OOBINLINE -> t.oobinline
        | O.SO_REUSEPORT -> t.reuseport && not (linux && t.family = Local)
        | O.SO_SNDBUF -> sndbuf t
        | O.SO_RCVBUF -> rcvbuf t
        | O.SO_RCVLOWAT -> t.rcvlowat
        | O.SO_SNDLOWAT -> (
            match t.sndlowat with
            | Some n -> n
            | None ->
                if linux || (t.family = Local && t.kind = Datagram) then 1
                else 2048)
        | O.SO_LINGER -> t.linger
        | O.SO_RCVTIMEO -> t.rcvtimeo
        | O.SO_SNDTIMEO -> t.sndtimeo
        | O.TCP_NODELAY -> t.nodelay
        | O.TCP_KEEPIDLE -> Option.value t.keepidle ~default:7200
        | O.TCP_KEEPINTVL -> Option.value t.keepintvl ~default:75
        | O.TCP_KEEPCNT ->
            Option.value t.keepcnt ~default:(if linux then 9 else 8)
        | O.TCP_MAXSEG -> (
            match t.os with
            | Linux -> (
                match t.maxseg with
                | Some n when connected t -> n
                | _ -> if t.user_mss > 0 then t.user_mss else 536)
            | Macos -> Option.value t.maxseg ~default:(default_maxseg t))
        | O.TCP_CONGESTION -> t.congestion
        | O.TCP_CORK -> t.cork
        | O.TCP_USER_TIMEOUT -> t.user_timeout
        | O.TCP_LINGER2 -> Some (if t.linger2 = 0 then 60 else t.linger2)
        | O.TCP_DEFER_ACCEPT -> retrans_to_secs t.defer_accept
        | O.TCP_SYNCNT -> t.syncnt
        | O.TCP_WINDOW_CLAMP -> (
            match t.window_clamp with
            | Some n -> n
            | None -> if connected t then 65536 else 0)
        | O.TCP_QUICKACK -> t.quickack
        | O.TCP_FASTOPEN -> t.fastopen
        | O.IP_FREEBIND -> t.freebind
        | O.IP_BIND_ADDRESS_NO_PORT -> t.bind_no_port
        | O.IP_LOCAL_PORT_RANGE -> t.port_range
        | O.IP_TTL -> t.ttl
        | O.IP_MTU ->
            if not (connected t) then error `Get `Not_connected
            else if t.family = V4 then 65535
            else 65536
        | O.IP_MTU_DISCOVER -> t.mtu_discover
        | O.IPV6_ONLY -> t.v6only
        | _ -> missing ())

  (* What a direction of a connection queues once a buffer is set: on Linux
     over TCP the writer's send buffer, of which it holds at least 32 KiB in
     flight, and half the reader's receive buffer, its window; over a Unix
     path half the writer's send buffer, the rest going to each write's
     overhead. On macOS over TCP the writer's send buffer and what the
     reader's receive buffer grows to as data comes, 128 KiB more than one
     set and 176 KiB when none is, or 16 KiB in flight for a send buffer
     smaller than that; over a Unix path the writer's send buffer. [None]
     when the queue keeps its capacity. *)
  let queue ~local ~writer ~reader =
    match (writer.sndbuf, reader.rcvbuf) with
    | None, None -> None
    | _ when local && writer.sndbuf = None -> None
    | _ -> (
        let snd = sndbuf writer in
        match (writer.os, local) with
        | Linux, false -> Some (max snd 32768 + (rcvbuf reader / 2))
        | Linux, true -> Some (snd / 2)
        | Macos, false ->
            let window =
              match reader.rcvbuf with Some r -> r + 131072 | None -> 180224
            in
            Some (snd + if snd < 16384 then 16384 else window)
        | Macos, true -> Some snd)

  (* When a TCP end whose connection went silent at [silenced] gives up, with
     ETIMEDOUT, if it does: [last] is when it last received a segment, and
     [oldest] when it sent the oldest byte not acknowledged. Keepalive
     probes once the idle time has passed since the last segment, a probe the
     live peer answered included, and gives up after [cnt] more intervals,
     or with [TCP_USER_TIMEOUT] at the first probe that long past the last
     segment. Data not acknowledged for [TCP_USER_TIMEOUT], or for 948.6 s on
     Linux (tcp_retries2 = 15 over loopback) and 18 s on macOS, gives up too.
     On Linux it stops the probes. Measured over loopback, with a peer whose
     segments were all dropped. *)
  let give_up t ~silenced ~last ~oldest =
    let ns = Mtime.to_uint64_ns
    and secs n = Int64.(mul (of_int n) 1_000_000_000L) in
    let at n = Some (Mtime.of_uint64_ns n) in
    let user = Int64.(mul (of_int t.user_timeout) 1_000_000L) in
    let linux = t.os = Linux in
    let keepalive =
      if not t.keepalive then None
      else
        let idle = secs (Option.value t.keepidle ~default:7200) in
        let intvl = secs (Option.value t.keepintvl ~default:75) in
        let cnt = Option.value t.keepcnt ~default:(if linux then 9 else 8) in
        let ceil a b = if a <= 0L then 0L else Int64.(div (add a (pred b)) b) in
        (* The last probe the live peer answered, or the last segment. *)
        let quiet =
          Int64.(
            mul idle (max 0L (pred (ceil (sub (ns silenced) (ns last)) idle))))
        in
        let base = Int64.add (ns last) quiet in
        let probes =
          if user > 0L then max 1L (ceil (Int64.sub user idle) intvl)
          else Int64.of_int cnt
        in
        at Int64.(add base (add idle (mul probes intvl)))
    in
    let retransmit =
      match oldest with
      | None -> None
      | Some u ->
          let span =
            if user > 0L then user
            else if linux then 948_600_000_000L
            else secs 18
          in
          at (Int64.add (ns u) span)
    in
    match (linux, keepalive, retransmit) with
    | true, _, Some _ -> retransmit
    | _, None, r | _, r, None -> r
    | false, Some k, Some r ->
        if Mtime.is_earlier k ~than:r then keepalive else retransmit
end

(* Fibers waiting on a pipe, first come first served, in a doubly-linked
   list as {!Eio.Condition} keeps them, so a cancelled waiter leaves at once.
   A waiter is taken from the list under the pipe's mutex, at most once, and
   woken once the mutex is released. The first waiter is woken with the turn:
   until it has gone again, a newcomer queues behind the waiters rather than
   take the room or bytes that woke it, and a woken waiter that must wait again
   goes back to the head. A waiter cancelled after it was taken passes the turn
   on, so no wake-up is lost. *)
module Waiters = struct
  type node = {
    wake : unit Eio.Promise.u;
    mutable prev : node option;
    mutable next : node option;
    mutable waiting : bool;
  }

  type t = {
    mutable first : node option;
    mutable last : node option;
    mutable turn : bool;
  }

  let v () = { first = None; last = None; turn = false }

  let add q =
    let p, wake = Eio.Promise.create () in
    let n = { wake; prev = q.last; next = None; waiting = true } in
    (match q.last with None -> q.first <- Some n | Some l -> l.next <- Some n);
    q.last <- Some n;
    (p, n)

  let add_first q =
    let p, wake = Eio.Promise.create () in
    let n = { wake; prev = None; next = q.first; waiting = true } in
    (match q.first with None -> q.last <- Some n | Some f -> f.prev <- Some n);
    q.first <- Some n;
    (p, n)

  let unlink q n =
    (match n.prev with None -> q.first <- n.next | Some p -> p.next <- n.next);
    (match n.next with None -> q.last <- n.prev | Some x -> x.prev <- n.prev);
    n.prev <- None;
    n.next <- None;
    n.waiting <- false

  let take q =
    match q.first with
    | None -> []
    | Some n ->
        unlink q n;
        [ n ]

  let rec take_all q =
    q.turn <- false;
    match q.first with
    | None -> []
    | Some n ->
        unlink q n;
        n :: take_all q

  (* A caller that does not hold the turn waits while others wait or hold
     it. *)
  let must_wait q ~turn = (not turn) && (Option.is_some q.first || q.turn)

  (* Wakes the first waiter with the turn, unless a woken one holds it. *)
  let pass q =
    if q.turn then []
    else
      match take q with
      | [] -> []
      | woken ->
          q.turn <- true;
          woken

  (* A caller that held the turn and leaves without waiting again passes it
     on. *)
  let release q ~turn =
    if turn then begin
      q.turn <- false;
      pass q
    end
    else []

  (* Queues a caller, at the head if it held the turn, which it gives up. *)
  let queue q ~turn =
    if turn then begin
      q.turn <- false;
      add_first q
    end
    else add q

  (* Removes a waiter still in the list; [false] if it was taken. *)
  let remove q n =
    n.waiting
    && begin
      unlink q n;
      true
    end

  let wake = List.iter (fun n -> Eio.Promise.resolve n.wake ())
end

(* One direction of a connection: the bytes written and not yet read. Its two
   ends may be used from two domains, so its state is guarded by a mutex. A
   read that finds nothing waits in [readers], and a write that finds the queue
   holding [capacity] bytes waits in [writers]. A write wakes one reader, and
   the next writer if room is left; a read wakes one writer, and the next
   reader if bytes are left. A change of state, a shutdown, a reset or a close,
   wakes every waiter.

   The ends follow TCP: [shut] is the writer's FIN, after which the reader
   reads end-of-file, or a reset when [reset] says one came. A reset is
   reported once, by a read or by a write that [refused] fails, as a socket
   clears its pending error; reads then return end-of-file. [gone] says the
   reader has closed: the first write to it is lost and answered with a reset,
   which [refused] then makes every later write raise; on a Unix-domain
   connection, [local], the first write raises it, as EPIPE. [rcv_shut] says the reader
   shut its receive side: it reads what is queued, then end-of-file, and as
   Linux then sends no window update, what it reads frees no room: [unacked]
   counts it. Once it
   has also shut its send side, [fin_sent], data reaching a TCP reader resets
   the connection, as Linux resets it in FIN-WAIT; a write to a Unix-domain
   reader that shut its receive side raises the reset, as EPIPE.

   [silent] says nothing crosses any more, as on a partition: the reader
   reads the [n] bytes of [Some n], then waits until its own [rcv_shut]; the
   writer fills the queue, then waits until its own [shut]. What the reader
   reads frees no room, and neither end's close or shutdown reaches the
   other. A TCP pipe with a [timer] keeps when its writer last sent a byte
   while the connection was live, [last], when it went silent, [silenced],
   and when the oldest byte sent since then, or lost then, was written,
   [oldest]: when each end gives up follows. *)
module Pipe = struct
  type t = {
    mutex : Mutex.t;
    queued : Cstruct.t Queue.t;
    mutable head : int; (* Bytes of the first queued buffer already read. *)
    mutable shut : bool;
    mutable reset : bool;
    mutable gone : bool;
    mutable refused : bool;
    mutable rcv_shut : bool;
    mutable fin_sent : bool;
    mutable silent : int option;
    timer : timer option;
    mutable last : Mtime.t;
    mutable silenced : Mtime.t;
    mutable oldest : Mtime.t option;
    os : os;
    local : bool;
    mutable capacity : int;
    mutable size : int; (* Bytes queued and not yet read. *)
    mutable unacked : int;
    readers : Waiters.t;
    writers : Waiters.t;
  }

  exception Expired

  let now t = match t.timer with Some c -> c.now () | None -> Mtime.min_stamp

  let v ~os ~local ~timer ~capacity =
    let timer = if local then None else timer in
    let born =
      match timer with Some c -> c.now () | None -> Mtime.min_stamp
    in
    {
      os;
      local;
      timer;
      last = born;
      silenced = born;
      oldest = None;
      mutex = Mutex.create ();
      queued = Queue.create ();
      head = 0;
      shut = false;
      reset = false;
      gone = false;
      refused = false;
      rcv_shut = false;
      fin_sent = false;
      silent = None;
      capacity;
      size = 0;
      unacked = 0;
      readers = Waiters.v ();
      writers = Waiters.v ();
    }

  (* Runs [f] under the mutex; it returns its result and the waiters to wake
     once the mutex is released. *)
  let locked t f =
    let result, woken = Mutex.protect t.mutex (fun () -> f t) in
    Waiters.wake woken;
    result

  (* Waits for the wake-up of [waiter], registered in [q], and at most until
     [until ()], which a time already past makes raise [Expired]: [false] if
     that time came first, and the waiter left without the turn. *)
  let await ?until t q (p, waiter) =
    let leave () =
      locked t (fun _ ->
          if Waiters.remove q waiter then ((), [])
          else begin
            q.turn <- false;
            ((), Waiters.pass q)
          end)
    in
    let deadline =
      match (t.timer, until) with
      | Some c, Some until -> Option.map (fun d -> (c, d)) (until ())
      | _ -> None
    in
    match deadline with
    | Some (c, d) when not (Mtime.is_later d ~than:(c.now ())) ->
        leave ();
        raise Expired
    | _ -> (
        let wait () =
          match deadline with
          | None ->
              Eio.Promise.await p;
              true
          | Some (c, d) ->
              Eio.Fiber.first
                (fun () ->
                  Eio.Promise.await p;
                  true)
                (fun () ->
                  c.sleep_until d;
                  false)
        in
        match wait () with
        | true -> true
        | false ->
            leave ();
            false
        | exception (Eio.Cancel.Cancelled _ as e) ->
            leave ();
            raise e)

  (* Changes the state under the mutex and wakes every waiter. *)
  let update t f =
    locked t (fun t ->
        f t;
        ((), Waiters.take_all t.readers @ Waiters.take_all t.writers))

  (* A socket's buffer changed: the queue holds [capacity] bytes now. *)
  let resize t capacity = update t (fun t -> t.capacity <- capacity)

  (* On macOS a write that fails on a reset leaves it to the next read, as
     it raises EPIPE without clearing the socket's pending error. *)
  let lasting t = t.os = Macos && not t.local

  (* The reset reaches the reader once it has read what is queued. A TCP
     reader that has the writer's end-of-file keeps reading end-of-file on
     Linux, which reports the end of a stream before a reset that came after
     it; macOS reports the reset. A Unix-domain reader reads end-of-file on
     macOS. *)
  let reset t =
    update t (fun t ->
        if Option.is_some t.silent then ()
        else if t.local && t.os = Macos then t.shut <- true
        else if t.local || lasting t || not t.shut then t.reset <- true)

  (* The reset was reported: the stream has ended. *)
  let consume_reset t =
    update t (fun t ->
        if t.reset && not (lasting t) then begin
          t.reset <- false;
          t.shut <- true
        end)

  (* The bytes are copied: the caller owns its buffers again once this returns.
     A write to a reader that has gone is lost, and resets [back], the other
     direction of the connection. *)
  let rec push t bufs n =
    match bufs with
    | b :: bufs when n > 0 ->
        let k = min n (Cstruct.length b) in
        if k > 0 then
          Queue.push (Cstruct.of_string (Cstruct.to_string ~len:k b)) t.queued;
        push t bufs (n - k)
    | _ -> ()

  (* Under the mutex: queues what room is left of [n] bytes. *)
  let queue t ~turn bufs n =
    if turn then t.writers.turn <- false;
    let k = min n (t.capacity - t.size - t.unacked) in
    push t bufs k;
    t.size <- t.size + k;
    k

  (* Under the mutex: the next writer, if room is left. *)
  let room t =
    if t.size + t.unacked < t.capacity then Waiters.pass t.writers else []

  let rec write ?(turn = false) ?until t ~back bufs =
    let n = Cstruct.lenv bufs in
    let result =
      locked t @@ fun t ->
      let leave outcome = (outcome, Waiters.release t.writers ~turn) in
      (* An empty write sends nothing, so on Linux it meets no shutdown or
         closed peer; on macOS it fails where a write would. *)
      if n = 0 && t.os = Linux then leave (`Done 0)
      else if t.shut then leave `Shut
      else if Option.is_some t.silent then
        if n = 0 then leave (`Done 0)
        else if
          Waiters.must_wait t.writers ~turn || t.size + t.unacked >= t.capacity
        then (`Wait (Waiters.queue t.writers ~turn), [])
        else
          let k = queue t ~turn bufs n in
          if k > 0 && Option.is_none t.oldest && Option.is_some t.timer then begin
            t.oldest <- Some (now t);
            (`Sent k, room t)
          end
          else (`Done k, room t)
      else if t.refused then leave `Refused
      else if n = 0 then
        if t.local && t.gone then leave (`Raise (reset_exn ()))
        else leave (`Done 0)
      else if t.gone then begin
        t.refused <- true;
        if t.local then leave (`Raise (reset_exn ())) else leave (`Lost n)
      end
      else if t.rcv_shut && t.local && t.os = Linux then
        leave (`Raise (reset_exn ()))
      else if
        Waiters.must_wait t.writers ~turn || t.size + t.unacked >= t.capacity
      then (`Wait (Waiters.queue t.writers ~turn), [])
      else if t.rcv_shut && t.os = Macos then begin
        (* macOS drops what reaches a receive side shut down, still counted
           in its buffer: a TCP one answers it with a reset. *)
        let k = min n (t.capacity - t.size - t.unacked) in
        t.unacked <- t.unacked + k;
        if t.local then leave (`Done k)
        else begin
          t.refused <- true;
          leave (`Dropped k)
        end
      end
      else if t.rcv_shut && t.fin_sent then begin
        (* Data a full queue held back resets the connection once it is
           sent; a write that waited for room reports the reset. *)
        t.reset <- true;
        t.refused <- true;
        if turn then leave (`Raise (reset_exn ())) else leave (`Lost n)
      end
      else
        let k = queue t ~turn bufs n in
        if k > 0 && Option.is_some t.timer then t.last <- now t;
        (`Done k, Waiters.pass t.readers @ room t)
    in
    match result with
    | `Wait w ->
        let turn = await ?until t t.writers w in
        write ~turn ?until t ~back bufs
    | `Sent n ->
        (* The writer's reads now wait for an acknowledgement too. *)
        update back ignore;
        n
    | `Raise e -> raise e
    | `Shut ->
        (* EPIPE after our own send shutdown; on TCP it reports a pending
           reset first, as Linux reports its pending error. *)
        if not t.local then consume_reset back;
        raise (reset_exn ())
    | `Refused ->
        (* A TCP write reports the pending reset; a Unix-domain one fails
           with EPIPE and leaves it to the next read. *)
        if not t.local then consume_reset back;
        raise (reset_exn ())
    | `Lost n ->
        reset back;
        n
    | `Dropped n ->
        (* The reader's reset: the writer's reads raise it, and so do the
           reader's writes. *)
        update back (fun b -> b.refused <- true);
        reset back;
        n
    | `Done n -> n

  let rec take t dst got =
    match Queue.peek_opt t.queued with
    | Some src when got < Cstruct.length dst ->
        let avail = Cstruct.length src - t.head in
        let n = min avail (Cstruct.length dst - got) in
        Cstruct.blit src t.head dst got n;
        if n = avail then begin
          ignore (Queue.pop t.queued : Cstruct.t);
          t.head <- 0
        end
        else t.head <- t.head + n;
        take t dst (got + n)
    | _ -> got

  (* What is queued drains before a reset or the end of the stream. *)
  let rec read ?(turn = false) ?until t dst =
    let result =
      locked t @@ fun t ->
      if Waiters.must_wait t.readers ~turn then
        (`Wait (Waiters.queue t.readers ~turn), [])
      else if Option.is_some t.silent then
        match t.silent with
        | Some left when left > 0 ->
            if turn then t.readers.turn <- false;
            let n =
              take t (Cstruct.sub dst 0 (min left (Cstruct.length dst))) 0
            in
            t.size <- t.size - n;
            t.unacked <- t.unacked + n;
            t.silent <- Some (left - n);
            (`Read n, if left > n then Waiters.pass t.readers else [])
        | _ when t.rcv_shut ->
            (`Raise End_of_file, Waiters.release t.readers ~turn)
        | _ -> (`Wait (Waiters.queue t.readers ~turn), [])
      else if not (Queue.is_empty t.queued) then begin
        if turn then t.readers.turn <- false;
        let n = take t dst 0 in
        t.size <- t.size - n;
        if t.rcv_shut then t.unacked <- t.unacked + n;
        let next =
          if Queue.is_empty t.queued then [] else Waiters.pass t.readers
        in
        (`Read n, (if t.rcv_shut then [] else Waiters.pass t.writers) @ next)
      end
      else if t.reset then begin
        t.reset <- false;
        t.shut <- true;
        (`Raise (reset_exn ()), Waiters.release t.readers ~turn)
      end
      else if t.shut || t.rcv_shut then
        (`Raise End_of_file, Waiters.release t.readers ~turn)
      else (`Wait (Waiters.queue t.readers ~turn), [])
    in
    match result with
    | `Read n -> n
    | `Raise e -> raise e
    | `Wait w ->
        let turn = await ?until t t.readers w in
        read ~turn ?until t dst

  let shut t = update t (fun t -> t.shut <- true)

  (* The reader shuts its receive side: it still reads what is queued and
     what arrives, as on Linux, and end-of-file once nothing is; what it
     reads frees no room for the writer. macOS drops what is queued. *)
  let shut_receive t =
    update t (fun t ->
        t.rcv_shut <- true;
        if t.os = Macos then begin
          Queue.clear t.queued;
          t.head <- 0;
          t.unacked <- t.unacked + t.size;
          t.size <- 0;
          if Option.is_some t.silent then t.silent <- Some 0
        end)

  (* A TCP reader's FIN advertises the room its dropped bytes took. *)
  let open_window t =
    update t (fun t -> if Option.is_none t.silent then t.unacked <- 0)

  (* The reader reads end-of-file once nothing is queued: macOS refuses to
     shut down its receive side again. *)
  let receive_ended t = Mutex.protect t.mutex (fun () -> t.rcv_shut || t.shut)

  (* The reader shut its send side. *)
  let fin_sent t = update t (fun t -> t.fin_sent <- true)

  (* Nothing crosses from now on. Over TCP what is queued is lost, an end of
     stream or a reset included, as a path that drops every segment loses
     those in flight. AF_UNIX queues at the reader what a returned write
     sent, so a Unix-domain reader still reads it, and a direction that has
     ended keeps its end. *)
  let silence t =
    update t (fun t ->
        if Option.is_none t.silent then
          if not t.local then begin
            t.silent <- Some 0;
            t.silenced <- now t;
            if t.size > 0 && Option.is_some t.timer then
              t.oldest <- Some (now t)
          end
          else if not (t.shut || t.reset || t.gone || t.refused || t.rcv_shut)
          then t.silent <- Some t.size)

  (* A silent TCP pipe with a timer: when it went silent, when its writer last
     sent a byte before, and when it wrote the oldest one not acknowledged. *)
  let timing t =
    Mutex.protect t.mutex (fun () ->
        match (t.silent, t.timer) with
        | Some _, Some _ -> Some (t.silenced, t.last, t.oldest)
        | _ -> None)

  (* The reader closes: bytes it left unread answer the writer with a reset,
     which refuses its writes at once. *)
  let leave t =
    locked t @@ fun t ->
    if Option.is_some t.silent then (false, [])
    else
      let unread = not (Queue.is_empty t.queued) in
      Queue.clear t.queued;
      t.head <- 0;
      t.size <- 0;
      t.gone <- true;
      if unread then t.refused <- true;
      (unread, Waiters.take_all t.readers @ Waiters.take_all t.writers)
end

module Flow = struct
  module Impl = struct
    type tag = [ `Generic ]

    (* [release] is the hook that closes the flow when the switch it was
       opened under finishes, removed once the flow is closed. A close defers
       to the operations in flight, [users], as Eio's [Rcfd] defers a
       descriptor's: [closing] refuses new ones at once, and the last user
       closes the socket, [closed]. [given_up] says the end gave up on its
       silent peer, and whether it reported ETIMEDOUT yet. *)
    type t = {
      to_peer : Pipe.t;
      from_peer : Pipe.t;
      lock : Mutex.t;
      mutable users : int;
      mutable closing : bool;
      mutable closed : bool;
      mutable abort : bool;
      mutable release : Eio.Switch.hook;
      mutable on_close : reset:bool -> abort:bool -> unit;
      mutable on_fin : unit -> unit;
      connection : int;
      opts : Sockopt.t;
      mutable given_up : [ `No | `Pending | `Reported ];
    }

    (* When this end gives up on its silent peer, if it does; the earliest
       time once it has. *)
    let until t () =
      if Mutex.protect t.lock (fun () -> t.given_up <> `No) then
        Some Mtime.min_stamp
      else
        match Pipe.timing t.from_peer with
        | None -> None
        | Some (silenced, sent, _) -> (
            match Pipe.timing t.to_peer with
            | None -> None
            | Some (_, acked, oldest) ->
                Sockopt.give_up t.opts ~silenced
                  ~last:
                    (if Mtime.is_later sent ~than:acked then sent else acked)
                  ~oldest)

    (* The error [op] meets once this end gave up: on Linux ETIMEDOUT once,
       for the first read or write, then end-of-file and EPIPE; on macOS
       EPIPE for each write, and ETIMEDOUT once for the first read, which
       a write leaves pending. The first wakes every waiter of this end. *)
    let gave_up t (op : [ `Read | `Write ]) =
      let first, report =
        Mutex.protect t.lock @@ fun () ->
        let first = t.given_up = `No in
        if first then t.given_up <- `Pending;
        let report =
          t.given_up = `Pending && (op = `Read || t.from_peer.os = Linux)
        in
        if report then t.given_up <- `Reported;
        (first, report)
      in
      if first then begin
        Pipe.update t.from_peer ignore;
        Pipe.update t.to_peer ignore
      end;
      if report then Posix.timed_out (op :> [ `Connect | `Read | `Write ])
      else match op with `Read -> End_of_file | `Write -> reset_exn ()

    (* Runs [f] with the deadline of a silent end, or raises what this end
       meets if it has given up. *)
    let watched t op f =
      if Option.is_none t.from_peer.timer then f None
      else
        let until = until t in
        let expired =
          match until () with
          | Some d -> not (Mtime.is_later d ~than:(Pipe.now t.from_peer))
          | None -> false
        in
        if expired then raise (gave_up t op)
        else
          match f (Some until) with
          | v -> v
          | exception Pipe.Expired -> raise (gave_up t op)

    (* [abort] resets the peer, as a connection a listener drops unaccepted
       is; otherwise the peer reads end-of-file, or a reset if this end left
       bytes unread. *)
    let teardown t =
      let unread = Pipe.leave t.from_peer in
      if t.abort then Pipe.update t.from_peer (fun p -> p.refused <- true);
      if unread || t.abort then Pipe.reset t.to_peer;
      Pipe.shut t.to_peer;
      t.on_close ~reset:(unread || t.abort) ~abort:t.abort

    let leave t =
      let last =
        Mutex.protect t.lock @@ fun () ->
        t.users <- t.users - 1;
        t.users = 0 && t.closing && (not t.closed)
        && begin
          t.closed <- true;
          true
        end
      in
      if last then teardown t

    (* Runs [f] as a user of the open socket. As Eio's own sockets refuse a
       descriptor used after close. *)
    let use t op f =
      Mutex.protect t.lock (fun () ->
          if t.closing then
            Fmt.invalid_arg "eio-net.mem: %s on a closed flow" op;
          t.users <- t.users + 1);
      match f () with
      | v ->
          leave t;
          v
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          leave t;
          Printexc.raise_with_backtrace e bt

    let single_write t bufs =
      use t "write" @@ fun () ->
      watched t `Write @@ fun until ->
      Pipe.write ?until t.to_peer ~back:t.from_peer bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    (* An empty read is end-of-file, as Eio's backends report the 0 that
       read(2) returns for it. *)
    let single_read t buf =
      use t "read" @@ fun () ->
      if Cstruct.length buf = 0 then raise End_of_file
      else watched t `Read @@ fun until -> Pipe.read ?until t.from_peer buf

    (* macOS refuses to shut down the receive side of a socket that reads
       end-of-file, and Eio ignores the ENOTCONN: nothing is shut down. *)
    let shutdown t cmd =
      use t "shutdown" @@ fun () ->
      (* An end that gave up is closed: Eio ignores the ENOTCONN. *)
      if Mutex.protect t.lock (fun () -> t.given_up = `No) then
        match cmd with
        | (`Receive | `All)
          when t.from_peer.os = Macos && Pipe.receive_ended t.from_peer ->
            ()
        | `Send ->
            Pipe.shut t.to_peer;
            Pipe.fin_sent t.from_peer;
            t.on_fin ()
        | `Receive -> Pipe.shut_receive t.from_peer
        | `All ->
            Pipe.shut t.to_peer;
            Pipe.fin_sent t.from_peer;
            Pipe.shut_receive t.from_peer;
            if t.from_peer.os = Macos && not t.from_peer.local then
              Pipe.open_window t.from_peer;
            t.on_fin ()

    let close_with ~abort t =
      let now =
        Mutex.protect t.lock @@ fun () ->
        if t.closing then `Again
        else begin
          t.closing <- true;
          t.abort <- abort;
          if t.users = 0 then begin
            t.closed <- true;
            `Now
          end
          else `Deferred
        end
      in
      if now <> `Again then begin
        Eio.Switch.remove_hook t.release;
        t.release <- Eio.Switch.null_hook
      end;
      if now = `Now then teardown t

    (* A TCP socket that lingers for no time resets its peer as it closes,
       as both kernels abort the connection then. *)
    let close t =
      let abort = (not t.from_peer.local) && t.opts.linger = Some 0 in
      close_with ~abort t

    let read_methods = []

    (* An option set on a silent end moves when it gives up. *)
    let setsockopt t opt v =
      use t "setsockopt" @@ fun () ->
      Sockopt.set t.opts opt v;
      if Option.is_some (Pipe.timing t.from_peer) then begin
        Pipe.update t.from_peer ignore;
        Pipe.update t.to_peer ignore
      end

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun t opt -> use t "getsockopt" @@ fun () -> Sockopt.get t.opts opt
  end

  type (_, _, _) Eio.Resource.pi +=
    | Connection : ('t, 't -> int, [> `Generic ]) Eio.Resource.pi
    | Silence : ('t, 't -> unit, [> `Generic ]) Eio.Resource.pi

  let handler =
    Eio.Resource.handler
      (H (Connection, fun (t : Impl.t) -> t.connection)
      :: H
           ( Silence,
             fun (t : Impl.t) ->
               Pipe.silence t.to_peer;
               Pipe.silence t.from_peer )
      :: Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module Impl)))

  type t = Impl.t * Impl.tag Eio.Net.stream_socket_ty Eio.Resource.t

  let v ~connection ~opts ~from_peer ~to_peer =
    let impl =
      Impl.
        {
          from_peer;
          to_peer;
          lock = Mutex.create ();
          users = 0;
          closing = false;
          closed = false;
          abort = false;
          release = Eio.Switch.null_hook;
          on_close = (fun ~reset:_ ~abort:_ -> ());
          on_fin = ignore;
          connection;
          opts;
          given_up = `No;
        }
    in
    (impl, Eio.Resource.T (impl, handler))

  let connections = Atomic.make 0

  (* What a direction of a connection queues when the network sets no
     capacity: at least what Linux's default buffers hold on loopback. A TCP
     send buffer grows to 4 MiB (tcp_wmem), and a writer queues about
     2.6 MiB to a reader that does not read; an AF_UNIX one queues 208 or
     224 KiB of the writer's buffer, each write counted with its overhead,
     at most about 250 KiB. *)
  let default_capacity ~os ~local =
    match os with
    | Linux -> if local then 256 * 1024 else 4 * 1024 * 1024
    | Macos -> if local then 8 * 1024 else 1024 * 1024

  (* What a direction queues: [capacity] until a socket sets a buffer, and at
     most a [capacity] the test gave, [bound]. *)
  let queue ~local ~capacity ~bound ~writer ~reader =
    match Sockopt.queue ~local ~writer ~reader with
    | None -> capacity
    | Some q -> if bound then min q capacity else q

  (* A connection to itself: what it writes, it reads. *)
  let self ~os ~timer ~capacity ~bound ~(opts : Sockopt.t) =
    let connection = Atomic.fetch_and_add connections 1 in
    let size () =
      queue ~local:false ~capacity ~bound ~writer:opts ~reader:opts
    in
    let pipe = Pipe.v ~os ~local:false ~timer ~capacity:(size ()) in
    opts.resized <- (fun () -> Pipe.resize pipe (size ()));
    v ~connection ~opts ~from_peer:pipe ~to_peer:pipe

  let pair ~os ~local ~timer ~capacity ~bound ~client:(c : Sockopt.t)
      ~server:(s : Sockopt.t) =
    let connection = Atomic.fetch_and_add connections 1 in
    let size writer reader = queue ~local ~capacity ~bound ~writer ~reader in
    let to_client = Pipe.v ~os ~local ~timer ~capacity:(size s c) in
    let to_server = Pipe.v ~os ~local ~timer ~capacity:(size c s) in
    let resized () =
      Pipe.resize to_client (size s c);
      Pipe.resize to_server (size c s)
    in
    c.resized <- resized;
    s.resized <- resized;
    let client =
      v ~connection ~opts:c ~from_peer:to_client ~to_peer:to_server
    in
    let server =
      v ~connection ~opts:s ~from_peer:to_server ~to_peer:to_client
    in
    (client, server)

  let connection (Eio.Resource.T (t, ops) : _ Eio.Net.stream_socket) =
    match Eio.Resource.get_opt ops Connection with
    | Some f -> Some (f t)
    | None -> None

  let silence (Eio.Resource.T (t, ops) : _ Eio.Net.stream_socket) =
    match Eio.Resource.get_opt ops Silence with
    | Some f -> f t
    | None ->
        invalid_arg "eio-net.mem: silence: not a connection of eio-net.mem"

  (* Closes [flow] when [sw] finishes, unless it is closed first. *)
  let close_with sw ((impl, flow) : t) =
    impl.Impl.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Flow.close flow);
    flow

  let close ((_, flow) : t) = Eio.Flow.close flow
  let on_close ((impl, _) : t) f = impl.Impl.on_close <- f
  let on_fin ((impl, _) : t) f = impl.Impl.on_fin <- f
  let abort ((impl, _) : t) = Impl.close_with ~abort:true impl
end

module Listening_socket = struct
  type tag = [ `Generic ]

  type hub = {
    listeners : (Eio.Net.Sockaddr.stream, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Eio.Mutex.t;
  }

  (* The connections waiting to be accepted, at most [backlog] of them, and
     [closed], guarded by [lock] so a connect queues one only while the
     listener is open, and a close resets every one it queued. A close
     defers to the accepts in flight, [users], as Eio's [Rcfd] defers a
     descriptor's: [closing] refuses new ones at once, and the last one
     closes the listener. *)
  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.stream;
    lock : Mutex.t;
    incoming : (Flow.t * Eio.Net.Sockaddr.stream) Queue.t;
    backlog : int;
    acceptable : Eio.Condition.t;
    room : Eio.Condition.t;
    mutable users : int;
    mutable closing : bool;
    mutable closed : bool;
    mutable hooks : Eio.Switch.hook list;
    reuse : bool;
    hold : Ports.hold option;
    opts : Sockopt.t;
  }

  (* Linux's accept queue takes one connection more than the backlog, which
     it caps at [net.core.somaxconn], 4096 by default, as it takes a negative
     one. *)
  let queue_length ~os backlog =
    match os with
    | Linux -> if backlog < 0 || backlog > 4096 then 4097 else backlog + 1
    | Macos -> if backlog <= 0 || backlog > 128 then 128 else backlog

  let is_closed t = Mutex.protect t.lock (fun () -> t.closed)

  (* A Unix-domain path is taken over with [reuse_addr], as Eio's POSIX backend
     removes the socket file there before it binds; the listener that held it
     stays open, unreachable. *)
  let bind hub ~reuse_addr ~backlog addr =
    let addr =
      match addr with
      | `Tcp (ip, 0) ->
          let free =
            match hub.ports.os with
            | Linux -> Ports.free hub.ports
            | Macos -> Ports.free_next hub.ports
          in
          Option.map (fun port -> `Tcp (ip, port)) free
      | `Unix path -> Some (`Unix (Files.bound hub.files path))
      | addr -> Some addr
    in
    match addr with
    | None -> Error `No_port
    | Some addr ->
        let bound =
          match addr with
          | `Unix path ->
              Files.bind hub.files ~take_over:reuse_addr path Files.Listener
          | `Tcp a -> not (Ports.in_use hub.ports ~reuse:reuse_addr a)
        in
        if not bound then Error `In_use
        else
          let hold =
            match addr with
            | `Tcp a ->
                Some (Ports.add hub.ports ~reuse:reuse_addr ~listening:true a)
            | `Unix _ -> None
          in
          let t =
            {
              hub;
              addr;
              lock = Mutex.create ();
              incoming = Queue.create ();
              backlog = queue_length ~os:hub.ports.os backlog;
              acceptable = Eio.Condition.create ();
              room = Eio.Condition.create ();
              users = 0;
              closing = false;
              closed = false;
              hooks = [];
              reuse = reuse_addr;
              hold;
              opts = Sockopt.listener ~os:hub.ports.os addr;
            }
          in
          Hashtbl.replace hub.listeners addr t;
          Ok t

  (* The refusals are raised once the mutex is released, which an exception
     inside it would poison. *)
  let v hub ~reuse_addr ~backlog addr =
    (match addr with
    | `Tcp (ip, _) -> check_local ~os:hub.ports.os ~tcp:true ~fail:bind_fail ip
    | `Unix path -> check_path ~os:hub.ports.os ~raw:true path);
    match
      Eio.Mutex.use_rw ~protect:true hub.mutex (fun () ->
          bind hub ~reuse_addr ~backlog addr)
    with
    | Ok t -> t
    (* A port of zero with none of the range free is bind(2)'s EADDRINUSE, as
       an address a socket holds is. *)
    | Error (`In_use | `No_port) -> bind_fail Address_in_use

  (* The connections waiting to be accepted are reset with the listener, as
     a kernel resets them. A connect that queues one after this is
     refused ([Network.connect]). *)
  let shut t =
    (* A path taken over is its new listener's to free. A close in a
       cancelled fiber frees the address all the same: taking the mutex is
       not cancelled. *)
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        match Hashtbl.find_opt t.hub.listeners t.addr with
        | Some held when held == t -> (
            Hashtbl.remove t.hub.listeners t.addr;
            match (t.addr, t.hold) with
            | `Tcp (_, port), Some h -> Ports.remove t.hub.ports port h
            | `Tcp _, None -> ()
            | `Unix path, _ -> Files.leave t.hub.files path Listener)
        | Some _ | None -> ());
    let queued =
      Mutex.protect t.lock @@ fun () ->
      let queued = List.of_seq (Queue.to_seq t.incoming) in
      Queue.clear t.incoming;
      queued
    in
    List.iter (fun (flow, _) -> Flow.abort flow) queued;
    Eio.Condition.broadcast t.room

  let leave t =
    let last =
      Mutex.protect t.lock @@ fun () ->
      t.users <- t.users - 1;
      t.users = 0 && t.closing && (not t.closed)
      && begin
        t.closed <- true;
        true
      end
    in
    if last then shut t

  let accept t ~sw =
    Mutex.protect t.lock (fun () ->
        if t.closing then invalid_arg "accept on closed in-memory listener";
        t.users <- t.users + 1);
    match
      Eio.Condition.loop_no_mutex t.acceptable @@ fun () ->
      Mutex.protect t.lock (fun () -> Queue.take_opt t.incoming)
    with
    | flow, client_addr ->
        leave t;
        Eio.Condition.broadcast t.room;
        (Flow.close_with sw flow, client_addr)
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        leave t;
        Printexc.raise_with_backtrace e bt

  (* Queues a connection, waiting while [backlog] do on Linux; macOS resets
     it at once. *)
  let offer t conn =
    let queued =
      Eio.Condition.loop_no_mutex t.room @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if t.closed then Some `Closed
      else if Queue.length t.incoming < t.backlog then begin
        Queue.push conn t.incoming;
        Some `Queued
      end
      else if t.hub.ports.os = Macos then Some `Full
      else None
    in
    if queued = `Queued then Eio.Condition.broadcast t.acceptable;
    queued

  let close t =
    let now =
      Mutex.protect t.lock @@ fun () ->
      if t.closing then `Again
      else begin
        t.closing <- true;
        if t.users = 0 then begin
          t.closed <- true;
          `Now
        end
        else `Deferred
      end
    in
    if now <> `Again then begin
      List.iter Eio.Switch.remove_hook t.hooks;
      t.hooks <- []
    end;
    if now = `Now then shut t

  (* As Eio's own sockets refuse a descriptor used after close. *)
  let listening_addr t =
    if Mutex.protect t.lock (fun () -> t.closing) then
      invalid_arg "eio-net.mem: listening_addr on a closed listener";
    t.addr

  let open_opts t =
    if Mutex.protect t.lock (fun () -> t.closing) then
      invalid_arg "eio-net.mem: socket option on a closed listener";
    t.opts

  let setsockopt t opt v = Sockopt.set (open_opts t) opt v

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt -> Sockopt.get (open_opts t) opt
end

(* A datagram socket bound at [addr]: the datagrams sent to it and not yet
   received, each with the address it came from. Like a pipe, its queue is
   guarded by a mutex and holds whatever is sent, so a datagram is never
   dropped or reordered on the way, and a send never waits. *)
module Datagram_socket = struct
  type tag = [ `Generic ]

  (* [room] is broadcast whenever a queued datagram goes, so a Unix-domain
     send waiting for room looks again. *)
  type hub = {
    os : os;
    sockets : (Eio.Net.Sockaddr.datagram, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Mutex.t;
    room : Eio.Condition.t;
  }

  (* [rmem] is the memory the datagrams of [queued] take, and those read but
     not yet freed, [unfreed], and [wmem] that of the Unix-domain datagrams
     this socket sent that are still queued, as Linux counts them; [batch] is
     the datagrams left of those queued when a receive last found none taken.
     All are guarded by [lock] but [wmem], which is atomic: no socket takes
     another's lock while it holds its own, as one sending to itself or two
     sending to each other would. *)
  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : datagram Queue.t;
    mutable rmem : int;
    mutable unfreed : int;
    mutable batch : int;
    wmem : int Atomic.t;
    readable : Eio.Condition.t;
    mutable closed : bool;
    mutable send_shut : bool;
    mutable recv_shut : bool;
    mutable release : Eio.Switch.hook;
    hold : Ports.hold option;
    opts : Sockopt.t;
  }

  and datagram = {
    src : Eio.Net.Sockaddr.datagram;
    data : string;
    size : int;
    sender : t option;
  }

  (* The memory a UDP datagram of [n] bytes takes in a receive queue over
     loopback, as Linux allocates it: a head of 896 bytes or a power of two
     that holds the payload and its headers, up to 16 KiB, then a small head
     and the payload in pages, and 256 bytes of [sk_buff]. *)
  let udp_size ~v4 n =
    let head = n + if v4 then 444 else 456 in
    let rec pow2 p = if p >= head then p else pow2 (2 * p) in
    256
    + if head <= 896 then 896 else if head <= 16384 then pow2 1024 else 896 + n

  (* The memory a Unix-domain datagram of [n] bytes takes from its sender's
     buffer until it is received, as Linux allocates it: up to 16000 bytes in
     a head of a power of two that holds them, rounded up to 64, and 384
     bytes of shared information; past that, the rest in pages, 17 at most;
     and 256 bytes of [sk_buff]. *)
  let unix_size n =
    let paged =
      if n <= 16000 then 0
      else min ((n - 16000 + 4095) / 4096 * 4096) (17 * 4096)
    in
    let head = ((n - paged + 63) / 64 * 64) + 384 in
    let rec pow2 p = if p >= head then p else pow2 (2 * p) in
    256 + pow2 512 + paged

  (* A Unix-domain socket whose queue holds more than [max_dgram_qlen]
     datagrams makes a send to it wait, but its own send to itself. *)
  let max_dgram_qlen = 10

  (* What a datagram of [n] bytes takes of a macOS receive buffer: its
     length, and at least 128 bytes on a Unix path, which fits what one Mac
     holds of 1, 500, 1000 and 2000 bytes. *)
  let macos_size ~local n = if local then max n 128 else n

  (* A port of zero with none of the range free is bind(2)'s EADDRINUSE. *)
  let free_port hub =
    match Ports.free hub.ports with
    | Some port -> port
    | None -> bind_fail Address_in_use

  let v hub ~reuse addr =
    (match addr with
    | `Udp (ip, _) -> check_local ~os:hub.os ~fail:bind_fail ip
    | `Unix path -> check_path ~os:hub.os ~raw:true path
    | `UdpV4 | `UdpV6 -> ());
    Mutex.protect hub.mutex @@ fun () ->
    let addr : Eio.Net.Sockaddr.datagram =
      match addr with
      (* An unbound socket is bound where a kernel binds it on its first
         send: the wildcard address of its family, at an ephemeral port. *)
      | `UdpV4 -> `Udp (Eio.Net.Ipaddr.V4.any, free_port hub)
      | `UdpV6 -> `Udp (Eio.Net.Ipaddr.V6.any, free_port hub)
      | `Udp (ip, 0) -> `Udp (ip, free_port hub)
      | `Unix "" -> `Unix (Files.autobind hub.files)
      | (`Udp _ | `Unix _) as addr -> addr
    in
    let in_use =
      match addr with
      | `Unix path -> not (Files.bind hub.files path Files.Datagram)
      | `Udp a -> Ports.in_use hub.ports ~reuse a
    in
    if in_use then bind_fail Address_in_use;
    let hold =
      match addr with
      | `Udp a -> Some (Ports.add hub.ports ~reuse a)
      | `Unix _ -> None
    in
    let t =
      {
        hub;
        addr;
        lock = Mutex.create ();
        queued = Queue.create ();
        rmem = 0;
        unfreed = 0;
        batch = 0;
        wmem = Atomic.make 0;
        readable = Eio.Condition.create ();
        closed = false;
        send_shut = false;
        recv_shut = false;
        release = Eio.Switch.null_hook;
        hold;
        opts = Sockopt.datagram ~os:hub.os addr;
      }
    in
    (* The latest bound of sockets sharing an address takes its datagrams,
       as on Linux. *)
    Hashtbl.add hub.sockets addr t;
    t

  (* The socket a datagram sent to [addr] reaches ([reaching]). *)
  let find hub (addr : Eio.Net.Sockaddr.datagram) =
    Mutex.protect hub.mutex @@ fun () ->
    match addr with
    | `Udp (ip, port) ->
        List.find_map
          (fun ip -> Hashtbl.find_opt hub.sockets (`Udp (ip, port)))
          (reaching ip)
    | `Unix _ -> Hashtbl.find_opt hub.sockets addr

  (* The longest datagram [t] sends to [dst]: UDP's payload over IPv4 or
     IPv6, or on a Unix path 212960 bytes, what Linux sends with a send
     buffer of 212992 bytes, and less, its send buffer less 32, for a smaller
     one. *)
  let max_length t : Eio.Net.Sockaddr.datagram -> int = function
    | `Unix _ -> min 212960 (Sockopt.sndbuf t.opts - 32)
    | `Udp (ip, _) -> if is_v4 ip then 65507 else 65527

  (* A send a socket refuses, in the order Linux checks: a Unix-domain socket
     refuses a UDP address or the empty path with EINVAL, then a datagram too long. An IPv4
     socket refuses more than 65535 bytes, then a path (EINVAL, or
     EAFNOSUPPORT for one that fills a [sockaddr_in], 14 bytes or more), an
     IPv6 address, port 0, an address its loopback address cannot route to,
     then a datagram too long for IPv4. An IPv6 socket refuses a path, then
     another family or an unreachable network, port 0, a loopback route, and
     a datagram too long; it sends to an IPv4 address the IPv4 way, from an
     IPv6 address other than the wildcard unreachable. *)
  (* macOS checks the length first: net.inet.udp.maxdgram and
     net.local.dgram.maxdgram. A Unix-domain socket refuses an IP address,
     an IPv4 one a path, and an IPv6 one an IPv4 address. A destination at a
     wildcard address has no route. An IPv4 socket bound at a wildcard
     address sends nothing to an IPv6 address, and one bound at 127.0.0.1
     finds no route to it. Between IPv6 and IPv4-mapped addresses, a socket
     bound at ::1 finds the IPv4 side not available, and one bound at an
     IPv4-mapped address other than the wildcard has no route to the IPv6
     side. *)
  let refusal_macos t (dst : Eio.Net.Sockaddr.datagram) len =
    let wild x = same (unmap x) (any (unmap x)) in
    if len > Sockopt.sndbuf t.opts then Some Message_too_long
    else
      match (t.addr, dst) with
      | `Unix _, `Udp _ -> Some Address_family
      | `Unix _, `Unix _ -> None
      | `Udp (b, _), `Unix _ ->
          Some
            (if String.length (b :> string) = 4 then Address_not_available
             else Invalid_address)
      | `Udp (b, _), `Udp (d, _) when String.length (b :> string) = 4 ->
          if String.length (d :> string) = 16 then
            Some (if wild b then Invalid_address else Host_unreachable)
          else if wild d then Some Host_unreachable
          else if off_loopback d then Some Network_unreachable
          else None
      | `Udp (b, _), `Udp (d, _) ->
          if String.length (d :> string) = 4 then Some Invalid_address
          else if wild d then Some Host_unreachable
          else if is_v4 d && not (is_v4 b || wild b) then
            Some Address_not_available
          else if (not (is_v4 d)) && is_v4 b && not (wild b) then
            Some Host_unreachable
          else if off_loopback d then Some Network_unreachable
          else None

  let refusal t dst bufs =
    let len = Cstruct.lenv bufs in
    if t.hub.os = Macos then refusal_macos t dst len
    else
      let too_long = len > max_length t dst in
      (* Without SO_BROADCAST a broadcast is refused, and a socket bound at
       a wildcard address has no route to send one, nor a multicast one. *)
      let routed b d port =
        let b = unmap b and d4 = unmap d in
        let wildcard = same b (any b) in
        if port = 0 then Some Invalid_port
        else if lo_broadcast d4 || (broadcast d4 && not wildcard) then
          Some Broadcast
        else if wildcard && (broadcast d4 || multicast_v4 d4) then
          Some Network_unreachable
        else if off_loopback d then Some Network_unreachable
        else if too_long then Some Message_too_long
        else None
      in
      match (t.addr, dst) with
      | `Unix _, `Udp _ -> Some Invalid_address
      | `Unix _, `Unix "" -> Some Invalid_address
      | `Unix _, `Unix _ -> if too_long then Some Message_too_long else None
      | `Udp (b, _), dst when String.length (b :> string) = 4 -> (
          if len > 0xffff then Some Message_too_long
          else
            match dst with
            | `Unix path ->
                Some
                  (if String.length path >= 14 then Address_family
                   else Invalid_address)
            | `Udp (d, _) when String.length (d :> string) = 16 ->
                Some Address_family
            | `Udp (d, port) -> routed b d port)
      | `Udp _, `Unix _ -> Some Invalid_address
      | `Udp (b, _), `Udp (d, port) -> (
          let d16 = if String.length (d :> string) = 4 then map d else d in
          match cross_family ~os:Linux b d16 with
          (* No route reaches an IPv6 address off loopback: Linux says so
           before it finds the families apart. *)
          | Some Address_family when off_loopback d -> Some Network_unreachable
          | Some code -> Some code
          | None -> routed b d port)

  (* The address a datagram from [t] to [dst] comes from, as [peer] sees it: a
     wildcard sender sends from the loopback address of the destination's
     family. *)
  let source t ~dst peer : Eio.Net.Sockaddr.datagram =
    match (t.addr, dst, peer.addr) with
    | `Udp (ip, port), `Udp (d, _), `Udp (own, _) ->
        let ip = unmap ip in
        let ip = if same ip (any ip) then source_to d else ip in
        (* On macOS a socket bound at an IPv4-mapped address takes IPv4
           datagrams as an IPv4 socket does. *)
        let own =
          if t.hub.os = Macos && String.length (own :> string) = 16 && is_v4 own
          then unmap own
          else own
        in
        `Udp (seen_by own ip, port)
    | addr, _, _ -> addr

  (* A datagram to an address no socket binds is lost, as UDP loses it, and
     the sender is not told; at a Unix path, the send finds no file, or one
     that no socket holds any more and refuses it. After a send shutdown a
     send fails with EPIPE, as does one to a Unix-domain socket that shut its
     receive side; Linux checks a Unix-domain socket's shutdown before its
     destination, and a UDP socket's after. *)
  (* Queues the datagram at [peer], unless [peer] refuses it (a Unix-domain
     socket that shut its receive side) or has closed. A UDP datagram that
     finds the receive buffer full is lost, as Linux drops it. A Unix-domain
     one waits while the sender's buffer is full, or [peer] holds more than
     [max_dgram_qlen], as on Linux: [`Wait] says to look again. *)
  let deliver_macos t ~local ~dst peer data =
    let size = macos_size ~local (String.length data) in
    let delivered =
      Mutex.protect peer.lock @@ fun () ->
      if peer.closed then `Lost
      else if peer.rmem + size > Sockopt.rcvbuf peer.opts then
        if local then `Full else `Lost
      else begin
        peer.rmem <- peer.rmem + size;
        Queue.push
          { src = source t ~dst peer; data; size; sender = None }
          peer.queued;
        `Delivered
      end
    in
    match delivered with
    | `Delivered ->
        Eio.Condition.broadcast peer.readable;
        `Done
    | `Lost -> `Done
    | `Full -> raise (Posix.no_buffer_space ())

  (* On macOS a datagram that finds its receiver's buffer full is lost over
     UDP, and refused with ENOBUFS on a Unix path, where Linux waits. *)
  let deliver t ~local ~dst peer bufs =
    let data = Cstruct.to_string (Cstruct.concat bufs) in
    if t.hub.os = Macos then deliver_macos t ~local ~dst peer data
    else
      let n = String.length data in
      let delivered =
        if local && Atomic.get t.wmem >= Sockopt.sndbuf t.opts then `Wait
        else
          Mutex.protect peer.lock @@ fun () ->
          if local && peer.recv_shut then `Refused
          else if peer.closed then `Lost
          else if local then
            if peer != t && Queue.length peer.queued > max_dgram_qlen then `Wait
            else begin
              let size = unix_size n in
              ignore (Atomic.fetch_and_add t.wmem size);
              Queue.push
                { src = source t ~dst peer; data; size; sender = Some t }
                peer.queued;
              `Delivered
            end
          else
            let v4 =
              match dst with `Udp (d, _) -> is_v4 d | `Unix _ -> false
            in
            let size = udp_size ~v4 n in
            if peer.rmem + size > Sockopt.rcvbuf peer.opts then `Lost
            else begin
              peer.rmem <- peer.rmem + size;
              Queue.push
                { src = source t ~dst peer; data; size; sender = None }
                peer.queued;
              `Delivered
            end
      in
      match delivered with
      | `Delivered ->
          Eio.Condition.broadcast peer.readable;
          `Done
      | `Lost -> `Done
      | `Wait -> `Wait
      | `Refused -> raise (reset_exn ())

  (* A datagram leaves its socket's queue: its memory is free. *)
  let freed t d =
    t.rmem <- t.rmem - d.size;
    Option.iter
      (fun s -> ignore (Atomic.fetch_and_add s.wmem (-d.size)))
      d.sender

  (* A datagram a receive takes. A UDP one's memory is freed with that of
     the others read before, once they reach a quarter of the buffer or the
     receive takes the last of its batch, as Linux frees it. *)
  let taken t d =
    match d.sender with
    | Some _ -> freed t d
    | None when t.hub.os = Macos -> freed t d
    | None ->
        t.unfreed <- t.unfreed + d.size;
        if t.unfreed >= Sockopt.rcvbuf t.opts / 4 || t.batch = 0 then begin
          t.rmem <- t.rmem - t.unfreed;
          t.unfreed <- 0
        end

  (* The destination of a send, or the error a send without one raises; a
     send shutdown first for a Unix-domain socket, after for a UDP one. *)
  let destination t ~local dst =
    let shut () =
      if Mutex.protect t.lock (fun () -> t.send_shut) then raise (reset_exn ())
    in
    if local then shut ();
    let dst =
      match dst with
      | Some dst -> dst
      | None ->
          let code =
            if local && t.hub.os = Linux then Not_connected else No_destination
          in
          raise (Eio.Exn.create (Eio.Exn.X code))
    in
    shut ();
    dst

  let send t ?dst bufs =
    if t.closed then invalid_arg "eio-net.mem: send on a closed datagram socket";
    let os = t.hub.os in
    (match dst with Some (`Unix path) -> check_path ~os path | _ -> ());
    let local = match t.addr with `Unix _ -> true | `Udp _ -> false in
    let dst =
      match (destination t ~local dst, t.addr) with
      | `Udp (d, port), `Udp (bound, _) when os = Linux ->
          `Udp (toward ~os ~bound d, port)
      | dst, _ -> dst
    in
    Option.iter
      (function
        | Broadcast -> raise (Eio.Fs.err (Permission_denied Broadcast))
        | code -> raise (Eio.Exn.create (Eio.Exn.X code)))
      (refusal t dst bufs);
    match dst with
    | `Udp (d, _) when os = Macos && unanswered d ->
        (* Sent, and lost: macOS's loopback interface takes 127.0.0.1 only. *)
        ()
    | _ -> (
        (* A send that waits looks for its destination again, as Linux does, and
           a Unix-domain one fails once its socket shuts its sending. *)
        Eio.Condition.loop_no_mutex t.hub.room
        @@ fun () ->
        if local && Mutex.protect t.lock (fun () -> t.send_shut) then
          raise (reset_exn ());
        let file =
          match dst with
          | `Unix path -> Files.find t.hub.files ~stream:false path
          | `Udp _ -> None
        in
        let refused () =
          raise (Eio.Net.err (Connection_failure (Refused No_listener)))
        in
        match (file, find t.hub dst) with
        | Some (Files.Listener | Client), _ ->
            raise (Eio.Exn.create (Eio.Exn.X Wrong_socket_type))
        | Some Stale, _ -> refused ()
        | None, None -> (
            match dst with
            | `Unix path when Files.abstract path -> refused ()
            | `Unix _ -> raise (no_file ())
            | `Udp _ -> Some ())
        | Some Datagram, None -> Some ()
        | (None | Some Datagram), Some peer -> (
            match deliver t ~local ~dst peer bufs with
            | `Done -> Some ()
            | `Wait -> None))

  (* A datagram longer than [buf] is cut to it, as a socket cuts it. *)
  let recv t buf =
    let got =
      Eio.Condition.loop_no_mutex t.readable @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if t.closed then
        invalid_arg "eio-net.mem: recv on a closed datagram socket"
      else if t.batch = 0 then t.batch <- Queue.length t.queued;
      match Queue.take_opt t.queued with
      | None -> None
      | Some d ->
          t.batch <- t.batch - 1;
          taken t d;
          let n = min (String.length d.data) (Cstruct.length buf) in
          Cstruct.blit_from_string d.data 0 buf 0 n;
          Some (d.src, n)
    in
    Eio.Condition.broadcast t.hub.room;
    got

  let close t =
    let was_open =
      Mutex.protect t.lock @@ fun () ->
      let was_open = not t.closed in
      t.closed <- true;
      Queue.iter (freed t) t.queued;
      Queue.clear t.queued;
      was_open
    in
    if was_open then begin
      Eio.Condition.broadcast t.hub.room;
      Eio.Switch.remove_hook t.release;
      t.release <- Eio.Switch.null_hook;
      Mutex.protect t.hub.mutex (fun () ->
          let others =
            List.filter
              (fun s -> s != t)
              (Hashtbl.find_all t.hub.sockets t.addr)
          in
          while Hashtbl.mem t.hub.sockets t.addr do
            Hashtbl.remove t.hub.sockets t.addr
          done;
          List.iter (Hashtbl.add t.hub.sockets t.addr) (List.rev others);
          match (t.addr, t.hold) with
          | `Unix path, _ -> Files.leave t.hub.files path Datagram
          | `Udp (_, port), Some h -> Ports.remove t.hub.ports port h
          | `Udp _, None -> ());
      Eio.Condition.broadcast t.readable
    end

  (* A send waiting for room on this socket, or from it, looks again. macOS
     refuses to shut down a socket that is not connected, and Eio ignores the
     ENOTCONN: nothing is shut down. *)
  let shutdown t cmd =
    ( Mutex.protect t.lock @@ fun () ->
      if t.closed then
        invalid_arg "eio-net.mem: shutdown on a closed datagram socket";
      match cmd with
      | (`Send | `Receive | `All) when t.hub.os = Macos -> ()
      | `Send -> t.send_shut <- true
      | `Receive -> t.recv_shut <- true
      | `All ->
          t.send_shut <- true;
          t.recv_shut <- true );
    Eio.Condition.broadcast t.hub.room

  let open_opts t =
    if Mutex.protect t.lock (fun () -> t.closed) then
      invalid_arg "eio-net.mem: socket option on a closed datagram socket";
    t.opts

  let setsockopt t opt v = Sockopt.set (open_opts t) opt v

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt -> Sockopt.get (open_opts t) opt
end

(* Host and service resolution as glibc's getaddrinfo(3) reads numbers, on
   a dual-stack host with no services database and no name server. *)
module Resolve = struct
  let is_space = function
    | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
    | _ -> false

  let is_digit c = c >= '0' && c <= '9'
  let ulong_max = "18446744073709551615"

  (* [service] read as strtoul(3) reads it in base 10, the whole string or
     nothing: leading spaces, a sign, digits, its value modulo 2^64 or
     ULONG_MAX past it. The empty string reads as 0. *)
  let strtoul service =
    let n = String.length service in
    let rec skip i =
      if i < n && is_space service.[i] then skip (i + 1) else i
    in
    let i = skip 0 in
    let neg, i =
      if i < n && (service.[i] = '+' || service.[i] = '-') then
        (service.[i] = '-', i + 1)
      else (false, i)
    in
    let rec digits j =
      if j < n && is_digit service.[j] then digits (j + 1) else j
    in
    let j = digits i in
    if n = 0 then Some (false, false, 0L)
    else if j = i || j <> n then None
    else
      let ds = String.sub service i (j - i) in
      let rec strip k =
        if k < String.length ds - 1 && ds.[k] = '0' then strip (k + 1) else k
      in
      let ds = String.sub ds (strip 0) (String.length ds - strip 0) in
      let overflow =
        String.length ds > 20
        || (String.length ds = 20 && String.compare ds ulong_max > 0)
      in
      (* The low 32 bits are all a port needs. *)
      let low =
        String.fold_left
          (fun acc c ->
            Int64.(logand (add (mul acc 10L) (of_int (Char.code c - 48))))
              0xffff_ffffL)
          0L ds
      in
      Some (neg, overflow, low)

  (* glibc keeps strtoul's result in an int: a negative one is no number, and
     the port is its low 16 bits. *)
  let port service =
    match strtoul service with
    | None -> None
    | Some (_, true, _) -> None
    | Some (neg, false, low) ->
        let v =
          if neg then Int64.(logand (sub 0x1_0000_0000L low) 0xffff_ffffL)
          else low
        in
        if Int64.compare v 0x8000_0000L >= 0 then None
        else Some (Int64.to_int v land 0xffff)

  (* A part of an inet_aton(3) address: hexadecimal after 0x, octal after 0,
     decimal otherwise. A bare 0x is no number. *)
  let part s =
    let n = String.length s in
    let value base digits =
      if digits = "" then None
      else
        String.fold_left
          (fun acc c ->
            match acc with
            | None -> None
            | Some v ->
                let d =
                  match c with
                  | '0' .. '9' -> Char.code c - 48
                  | 'a' .. 'f' -> Char.code c - 87
                  | 'A' .. 'F' -> Char.code c - 55
                  | _ -> 99
                in
                if d >= base then None
                else
                  let v = Int64.(add (mul v (of_int base)) (of_int d)) in
                  if Int64.compare v 0xffff_ffffL > 0 then None else Some v)
          (Some 0L) digits
    in
    if n >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
      value 16 (String.sub s 2 (n - 2))
    else if n >= 1 && s.[0] = '0' then value 8 s
    else value 10 s

  (* macOS's port: strtoul(3)'s value, which must be at most 65535, so a
     negative one is no port but -0. *)
  let port_macos service =
    let n = String.length service in
    let rec skip i =
      if i < n && is_space service.[i] then skip (i + 1) else i
    in
    let i = skip 0 in
    let neg, i =
      if i < n && (service.[i] = '+' || service.[i] = '-') then
        (service.[i] = '-', i + 1)
      else (false, i)
    in
    let rec digits j =
      if j < n && is_digit service.[j] then digits (j + 1) else j
    in
    let j = digits i in
    if n = 0 then Some 0
    else if j = i || j <> n then None
    else
      let rec strip k =
        if k < j - 1 && service.[k] = '0' then strip (k + 1) else k
      in
      let k = strip i in
      if j - k > 5 then None
      else
        let v = int_of_string (String.sub service k (j - k)) in
        if neg then if v = 0 then Some 0 else None
        else if v <= 65535 then Some v
        else None

  (* An IPv4 address as inet_aton(3) reads it: one to four parts, the last
     filling the bytes left. macOS's reads a bare 0x as 0 but in the last
     part. *)
  let inet_aton ~os s =
    let parts = String.split_on_char '.' s in
    let last = List.length parts - 1 in
    let parts =
      List.mapi
        (fun i p ->
          if os = Macos && i < last && String.lowercase_ascii p = "0x" then
            Some 0L
          else part p)
        parts
    in
    if List.mem None parts then None
    else
      let parts = List.map Option.get parts in
      let k = List.length parts in
      let rec fold acc = function
        | [] -> Some acc
        | [ last ] ->
            let bits = 8 * (5 - k) in
            if k > 4 || Int64.compare last (Int64.shift_left 1L bits) >= 0 then
              None
            else Some Int64.(logor (shift_left acc bits) last)
        | x :: rest ->
            if Int64.compare x 255L > 0 then None
            else fold Int64.(logor (shift_left acc 8) x) rest
      in
      match fold 0L parts with
      | None -> None
      | Some v ->
          Some
            (Eio.Net.Ipaddr.of_raw
               (String.init 4 (fun i ->
                    Char.chr
                      (Int64.to_int
                         Int64.(
                           logand (shift_right_logical v (8 * (3 - i))) 255L)))))

  (* /etc/hosts names the loopback addresses "localhost", in any case. With
     no name server, "localhost." is another name. *)
  let localhost name = String.lowercase_ascii name = "localhost"

  (* An IPv6 address, in glibc's form, with no brackets. *)
  let ipv6 s =
    if String.length s > 0 && s.[0] = '[' then None
    else
      match Ipaddr.V6.of_string s with
      | Ok ip -> Some (Eio.Net.Ipaddr.of_raw (Ipaddr.V6.to_octets ip))
      | Error _ -> None

  (* A scope glibc takes after an IPv6 address: a number of 32 bits, or the
     name of an interface, which only the loopback one has, for an address
     that needs a scope. *)
  let scope ~os ip s =
    (os = Macos && s <> "")
    ||
    let value =
      String.fold_left
        (fun v c ->
          if Int64.compare v 0xffff_ffffL > 0 then v
          else Int64.(add (mul v 10L) (of_int (Char.code c - 48))))
        0L s
    in
    s <> "" && String.for_all is_digit s
    && Int64.compare value 0xffff_ffffL <= 0
    || (s = "lo" && needs_scope ip)

  let number ~os name =
    match inet_aton ~os name with
    | Some ip -> Some ip
    | None -> (
        match String.index_opt name '%' with
        | None -> ipv6 name
        | Some i -> (
            match ipv6 (String.sub name 0 i) with
            | Some ip
              when scope ~os ip
                     (String.sub name (i + 1) (String.length name - i - 1)) ->
                Some ip
            | Some _ | None -> None))

  (* The addresses of a host: a number as it reads, the loopback addresses
     for "localhost" and the empty name, IPv6 first as glibc and macOS order
     them on a dual-stack host, and nothing for any other name. *)
  let host ~os name : Eio.Net.Ipaddr.v4v6 list =
    let loopback =
      [
        (Eio.Net.Ipaddr.V6.loopback :> Eio.Net.Ipaddr.v4v6);
        (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6);
      ]
    in
    if name = "" || localhost name then loopback
    else Option.to_list (number ~os name)
end

(* The two ends of a TCP connection, and when each lets go of its address.
   An end holds it while open. Once closed, the end whose FIN went first, by
   a shutdown of its send side or by its close, holds it in TIME_WAIT for 60
   seconds from the peer's FIN, as Linux does, and the other lets go at once.
   Closed with no FIN from its peer, it waits for one in FIN_WAIT2 for 60
   seconds, Linux's tcp_fin_timeout.
   After a reset, and without a clock, each lets go once both have closed.
   An end its listener dropped unaccepted lets go at once, as Linux destroys
   it. *)
module Ends = struct
  type side = [ `Client | `Server ]

  type decision =
    [ `Held | `Release | `Until of Mtime.t | `Time_wait of Mtime.t ]

  type t = {
    lock : Mutex.t;
    now : (unit -> Mtime.t) option;
    time_wait : Mtime.Span.t;
    mutable seq : int;
    fin : (int * Mtime.t) option array;
    closed : Mtime.t option array;
    dropped : bool array;
    mutable reset : bool;
  }

  let v (ports : Ports.t) =
    {
      lock = Mutex.create ();
      now = ports.now;
      time_wait = Ports.time_wait ports;
      seq = 0;
      fin = [| None; None |];
      closed = [| None; None |];
      dropped = [| false; false |];
      reset = false;
    }

  let index : side -> int = function `Client -> 0 | `Server -> 1
  let now t = match t.now with Some now -> now () | None -> Mtime.min_stamp

  let decide t i : decision =
    let o = 1 - i in
    match t.closed.(i) with
    | None -> `Held
    | Some _ when t.dropped.(i) -> `Release
    | Some closed -> (
        let both = Option.is_some t.closed.(o) in
        if Option.is_none t.now || t.reset then if both then `Release else `Held
        else
          let after span t = Option.value ~default:t (Mtime.add_span t span) in
          let orphan = after Ports.fin_timeout closed in
          match (t.fin.(i), t.fin.(o)) with
          | Some (mine, _), Some (theirs, _) when theirs < mine -> `Release
          | _, Some (_, peer) when Mtime.is_later peer ~than:orphan -> `Release
          | _, Some (_, peer) ->
              let u = after t.time_wait peer in
              if Mtime.is_later u ~than:closed then `Time_wait u else `Release
          | _, None -> `Until orphan)

  (* Changes the state, and gives what the client and the server now do. *)
  let update t f =
    Mutex.protect t.lock @@ fun () ->
    f ();
    (decide t 0, decide t 1)

  let send_fin t i =
    if Option.is_none t.fin.(i) then begin
      t.seq <- t.seq + 1;
      t.fin.(i) <- Some (t.seq, now t)
    end

  let fin t side = update t (fun () -> send_fin t (index side))

  let close t side ~reset ~abort =
    update t (fun () ->
        let i = index side in
        send_fin t i;
        t.closed.(i) <- Some (now t);
        if abort then t.dropped.(i) <- true;
        if reset then t.reset <- true)

  let reset t = Mutex.protect t.lock (fun () -> t.reset <- true)
end

module Network = struct
  type tag = [ `Generic ]

  type t = {
    sw : Eio.Switch.t;
    os : os;
    hub : Listening_socket.hub;
    datagrams : Datagram_socket.hub;
    capacity : int -> int;
        (* What a connection queues each way, from the kernel's default. *)
    bounded : bool;
        (* The test gave [capacity]: it bounds what buffers set give too. *)
    sleep : (float -> unit) option;
    timer : timer option;
  }

  let listening_handler = Eio.Net.Pi.listening_socket (module Listening_socket)

  (* One listener per address: there is no kernel to spread connections
     across several, so [reuse_port] is refused. A listener is closed by
     whichever of [sw] and the network's switch finishes first. *)
  let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
    if reuse_port then
      invalid_arg "eio-net.mem does not support SO_REUSEPORT listeners";
    Eio.Switch.check sw;
    (* eio_posix removes a listener's file when [sw] finishes, and fails to
       for a path with a NUL, as there is none. *)
    (match addr with
    | `Unix path when embedded_nul ~os:Linux path ->
        Eio.Switch.on_release sw (fun () ->
            raise (Posix.bind_error (`Unlink path)))
    | _ -> ());
    let listener = Listening_socket.v t.hub ~reuse_addr ~backlog addr in
    (* The file a listener leaves goes when the listen's switch finishes, as
       eio_linux removes it then, closed by hand or not; eio_posix keeps the
       file of a listener closed by hand. This runs after the close the hooks
       below make. *)
    (match listener.addr with
    | `Unix path ->
        Eio.Switch.on_release sw (fun () ->
            Files.leave ~remove:true t.hub.files path Stale)
    | `Tcp _ -> ());
    let socket = Eio.Resource.T (listener, listening_handler) in
    let close () = Eio.Resource.close socket in
    listener.hooks <-
      List.map
        (fun sw -> Eio.Switch.on_release_cancellable sw close)
        (if sw == t.sw then [ sw ] else [ sw; t.sw ]);
    socket

  (* The options of a [connect], set in order before it binds and connects,
     as eio_posix sets them. *)
  let rec set_options opts : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (opt, v) :: rest ->
        Sockopt.set opts opt v;
        set_options opts rest

  (* Holds the address of a client that connects to [ip] at [dport] in the
     listeners' ports, until [release]: the loopback address of its
     destination's family, in the destination's form, or the one it is bound
     at. Its port is, on Linux, one free toward the destination if it binds
     no address, and the highest port nothing holds if bound at port 0, as
     listeners count up from the bottom; on macOS, the next port nothing
     holds. *)
  let hold t ?bind ((ip : Eio.Net.Ipaddr.v4v6), dport) =
    let toward = (List.hd (reaching ip), dport) in
    let mapped = String.length (ip :> string) = 16 && is_v4 ip in
    let reached = if mapped then map (source_to ip) else source_to ip in
    let bound, wildcard, port =
      match bind with
      | None -> (reached, false, 0)
      | Some (b, port) -> (b, same (unmap b) (any (unmap b)), port)
    in
    let ip = if wildcard then reached else bound in
    Eio.Mutex.use_rw ~protect:true t.hub.mutex @@ fun () ->
    let ports = t.hub.Listening_socket.ports in
    (* The bind comes first, at the address it names. *)
    if port <> 0 && Ports.in_use ports (bound, port) then Error `In_use
    else
      let toward =
        match (bind, t.os) with None, Linux -> Some toward | _ -> None
      in
      let port =
        match (toward, t.os) with
        | _ when port <> 0 -> Some port
        | Some toward, _ -> Ports.free_toward ports toward
        | None, Linux -> Ports.free_down ports
        | None, Macos -> Ports.free_next ports
      in
      match port with
      | None -> Error `No_port
      | Some port ->
          Ok
            ( (ip, port),
              Ports.add ports ~wildcard ~connected:true ?toward (ip, port) )

  (* Lets go of [h] now, or at [until]. Not cancelled, even while it waits
     for the mutex: a release that gave up would hold the port for good. *)
  let release_port t ?until ?(time_wait = false) port h =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        let ports = t.hub.Listening_socket.ports in
        match until with
        | Some until -> Ports.set_until h ~time_wait until
        | None -> Ports.remove ports port h)

  (* A connect's socket has its destination's family, and fails to bind an
     address of another as Linux's bind(2) does: an IPv4 socket with
     EAFNOSUPPORT, an IPv6 or Unix-domain one with EINVAL. IPv6 against
     IPv4-mapped fails as on Linux. *)
  let check_bind ~os (bind_to : Eio.Net.Sockaddr.stream option)
      (addr : Eio.Net.Sockaddr.stream) =
    match (bind_to, addr) with
    | None, _ | Some (`Unix _), `Unix _ -> ()
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string)
           && os = Macos ->
        Option.iter fail (cross_family ~os b d)
    | Some _, _ when os = Macos -> fail Address_family
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string) -> (
        match cross_family ~os b d with
        (* Linux finds no route, or no scope, before the families apart. *)
        | Some Address_family when first_byte d = 0xff ->
            fail Network_unreachable
        | Some Address_family when needs_scope d -> fail Invalid_address
        | Some Address_family when off_loopback d -> fail Network_unreachable
        | code -> Option.iter fail code)
    | Some (`Tcp _), `Tcp (d, _) when String.length (d :> string) = 16 ->
        fail Invalid_address
    | Some (`Tcp _), `Unix _ -> fail Invalid_address
    | Some _, `Tcp _ -> fail Address_family

  (* The client's address as the listener at [listener] sees it: an IPv4
     client of an IPv6 listener is IPv4-mapped. A Unix-domain client binds no
     path. *)
  let peer_addr (listener : Eio.Net.Sockaddr.stream) (ip, port) :
      Eio.Net.Sockaddr.stream =
    match listener with
    | `Unix _ -> `Unix ""
    | `Tcp (lip, _) -> `Tcp (seen_by lip ip, port)

  (* A connect for a client that holds [local], if it is a TCP one. *)
  let connect_held t ~sw ~bind_to ~opts
      ~(release : ?until:Mtime.t -> ?time_wait:bool -> unit -> unit) addr local
      =
    let listener =
      Eio.Mutex.use_ro t.hub.mutex (fun () ->
          match addr with
          | `Tcp (ip, port) ->
              List.find_map
                (fun ip -> Hashtbl.find_opt t.hub.listeners (`Tcp (ip, port)))
                (reaching ip)
          | `Unix _ -> Hashtbl.find_opt t.hub.listeners addr)
    in
    let refused () =
      raise (Eio.Net.err (Connection_failure (Refused No_listener)))
    in
    (* A connect reaches no broadcast or multicast address, nor one off
       loopback, and a link-local one needs a scope, as on Linux. *)
    (match addr with
    | `Tcp (d, _) ->
        let d' = unmap d in
        if
          (is_v4 d' && (multicast_v4 d' || broadcast d' || lo_broadcast d'))
          || ((not (is_v4 d')) && first_byte d' = 0xff)
        then fail (if t.os = Macos then Address_family else Network_unreachable);
        if needs_scope d' then
          fail (if t.os = Macos then Host_unreachable else Invalid_address);
        if off_loopback d then fail Network_unreachable
    | `Unix _ -> ());
    (* A connect bound at its own destination opens to itself, as Linux's
       simultaneous open does. *)
    let to_itself =
      match (addr, bind_to, local) with
      | `Tcp (d, port), Some (`Tcp _), Some (ip, port') ->
          port = port' && same (unmap ip) (unmap (List.hd (reaching d)))
      | _ -> false
    in
    match (listener, addr) with
    | _, `Tcp _ when to_itself && t.os = Macos -> fail Invalid_address
    | _, `Tcp (d, _) when t.os = Macos && unanswered d -> (
        (* macOS's loopback interface takes 127.0.0.1 only: a connect to
           another address of 127.0.0.0/8 waits for a handshake that never
           comes, and times out after net.inet.tcp.keepinit. *)
        match t.sleep with
        | Some sleep ->
            sleep 75.;
            raise (Posix.timed_out `Connect)
        | None -> Eio.Fiber.await_cancel ())
    | None, `Tcp _ when to_itself ->
        let capacity =
          t.capacity (Flow.default_capacity ~os:t.os ~local:false)
        in
        Sockopt.connect opts Client;
        let flow =
          Flow.self ~os:t.os ~timer:t.timer ~capacity ~bound:t.bounded ~opts
        in
        let ends = Ends.v t.hub.Listening_socket.ports in
        (* Its FIN reaches itself: it holds its address in TIME_WAIT. *)
        Flow.on_close flow (fun ~reset ~abort:_ ->
            ignore (Ends.close ends `Client ~reset ~abort:false);
            match Ends.close ends `Server ~reset ~abort:false with
            | `Time_wait u, _ -> release ~until:u ~time_wait:true ()
            | `Until u, _ -> release ~until:u ()
            | (`Release | `Held), _ -> release ());
        Flow.close_with sw flow
    | None, `Unix "" -> fail Invalid_address
    | None, `Unix path -> (
        match Files.find t.hub.files ~stream:true path with
        | Some Datagram -> raise (Eio.Exn.create (Eio.Exn.X Wrong_socket_type))
        | Some (Stale | Client | Listener) -> refused ()
        | None when Files.abstract path -> refused ()
        | None -> raise (no_file ()))
    | None, `Tcp _ -> refused ()
    | Some listener, _ when Listening_socket.is_closed listener -> refused ()
    | Some listener, _ -> (
        let unix = match addr with `Unix _ -> true | `Tcp _ -> false in
        let capacity =
          t.capacity (Flow.default_capacity ~os:t.os ~local:unix)
        in
        Sockopt.connect opts Client;
        let client, server =
          Flow.pair ~os:t.os ~local:unix ~timer:t.timer ~capacity
            ~bound:t.bounded ~client:opts
            ~server:(Sockopt.accepted listener.opts)
        in
        (* The accepted socket holds the listener's port at the address the
           client reached, with the listener's [SO_REUSEADDR], and as one
           bound at a wildcard address if the listener is, as Linux's
           does. *)
        let child =
          match addr with
          | `Unix _ -> None
          | `Tcp (ip, port) ->
              let reached = List.hd (reaching ip) in
              (* macOS's accepted socket has its listener's family. *)
              let reached =
                match (t.os, listener.addr) with
                | Macos, `Tcp (lip, _) -> seen_by lip reached
                | _ -> reached
              in
              Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
                  Some
                    ( port,
                      Ports.add t.hub.Listening_socket.ports
                        ~reuse:listener.reuse ~connected:true
                        ~wildcard:
                          (match listener.addr with
                          | `Tcp (l, _) -> same (unmap l) (any (unmap l))
                          | `Unix _ -> false)
                        (reached, port) ))
        in
        let ends = Ends.v t.hub.Listening_socket.ports in
        let settle (client, server) =
          let apply f = function
            | `Held -> ()
            | `Release -> f None false
            | `Until u -> f (Some u) false
            | `Time_wait u -> f (Some u) true
          in
          apply (fun until time_wait -> release ?until ~time_wait ()) client;
          Option.iter
            (fun (port, h) ->
              apply
                (fun until time_wait -> release_port t ?until ~time_wait port h)
                server)
            child
        in
        Flow.on_close client (fun ~reset ~abort ->
            settle (Ends.close ends `Client ~reset ~abort));
        Flow.on_close server (fun ~reset ~abort ->
            settle (Ends.close ends `Server ~reset ~abort));
        Flow.on_fin client (fun () -> settle (Ends.fin ends `Client));
        Flow.on_fin server (fun () -> settle (Ends.fin ends `Server));
        let fail () =
          Ends.reset ends;
          Flow.close server;
          Flow.close client
        in
        let peer =
          match (local, bind_to) with
          | Some a, _ -> peer_addr listener.addr a
          | None, Some (`Unix path) -> `Unix path
          | None, _ -> `Unix ""
        in
        (* The switch takes the client before the listener sees it, so a
           finished switch queues nothing. *)
        let flow = Flow.close_with sw client in
        match Listening_socket.offer listener (server, peer) with
        | `Queued -> flow
        | `Closed ->
            (* The listener closed while the connection waited for room. *)
            fail ();
            refused ()
        | `Full ->
            fail ();
            raise (Eio.Net.err (Connection_reset No_listener))
        | exception e ->
            fail ();
            raise e)

  (* A connection is a pair of queues. A TCP client holds its address until
     its flow closes; one bound at an address a socket holds finds it in
     use. *)
  let connect t ~bind_to ~options ~sw addr =
    let os = t.os in
    let opts = Sockopt.client ~os addr in
    set_options opts options;
    Eio.Switch.check sw;
    (* macOS binds before it looks at the destination. *)
    (match (os, bind_to) with
    | Macos, Some (`Unix path) -> check_path ~os path
    | _ -> ());
    (match addr with `Unix path -> check_path ~os path | `Tcp _ -> ());
    let addr =
      match (addr, bind_to) with
      | `Tcp (d, port), Some (`Tcp (bound, _)) ->
          `Tcp (toward ~os ~bound d, port)
      | addr, _ -> addr
    in
    check_bind ~os bind_to addr;
    (match bind_to with
    | Some (`Tcp (ip, _)) ->
        check_local ~os ~tcp:true ip;
        Sockopt.bound opts ip
    | Some (`Unix path) -> check_path ~os path
    | None -> ());
    let bind_to =
      match bind_to with
      | Some (`Unix path) -> Some (`Unix (Files.bound t.hub.files path))
      | bind_to -> bind_to
    in
    let held =
      match (addr, bind_to) with
      | `Unix _, Some (`Unix path) ->
          if Files.bind t.hub.files path Client then `Path path
          else raise (address_in_use ())
      | `Unix _, _ -> `Nothing
      | `Tcp (ip, dport), bind -> (
          let bind = match bind with Some (`Tcp b) -> Some b | _ -> None in
          match hold t ?bind (ip, dport) with
          | Ok (local, h) -> `Port (local, h)
          (* With no port left, connect(2) fails with EADDRNOTAVAIL, and a
             bind(2) at port 0 before it with EADDRINUSE. *)
          | Error `No_port when Option.is_none bind ->
              fail Address_not_available
          | Error (`In_use | `No_port) -> raise (address_in_use ()))
    in
    let local =
      match held with `Port (a, _) -> Some a | `Path _ | `Nothing -> None
    in
    (* Released by the connection's ends; a bound path leaves its file. *)
    let release ?until ?time_wait () =
      match held with
      | `Port ((_, port), h) -> release_port t ?until ?time_wait port h
      | `Path path -> Files.leave t.hub.files path Client
      | `Nothing -> ()
    in
    match connect_held t ~sw ~bind_to ~opts ~release addr local with
    | flow -> flow
    | exception e ->
        (* eio_posix and eio_linux keep a failed connect's socket on [sw], so
           the address it was bound at stays bound until [sw] finishes, as on
           Linux; macOS frees a TCP one at once. *)
        (match bind_to with
        | None -> release ()
        | Some (`Tcp _) when os = Macos -> release ()
        | Some _ -> (
            try Eio.Switch.on_release sw (fun () -> release ())
            with Invalid_argument _ -> ()));
        raise e

  let datagram_handler = Eio.Net.Pi.datagram_socket (module Datagram_socket)

  (* One socket per address: there is no kernel to spread datagrams across
     several, so [reuse_port] is refused. *)
  let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
    if reuse_port then
      invalid_arg "eio-net.mem does not support SO_REUSEPORT datagram sockets";
    let datagram = Datagram_socket.v t.datagrams ~reuse:reuse_addr addr in
    let socket = Eio.Resource.T (datagram, datagram_handler) in
    datagram.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Resource.close socket);
    socket

  (* Each address once for streams and once for datagrams, as getaddrinfo(3)
     lists one per socket type. The service is read first, as glibc reads it,
     and then the host: a number, "localhost" or the loopback addresses.
     Neither host nor service names nothing. *)
  let getaddrinfo t ~service node =
    let port =
      match t.os with
      | Linux -> Resolve.port service
      | Macos -> Resolve.port_macos service
    in
    match port with
    | _ when node = "" && service = "" -> []
    | None when t.os = Macos ->
        raise (Eio.Net.err (Address_lookup_failed NONAME))
    | None -> raise (Eio.Net.err (Address_lookup_failed SERVICE))
    | Some port ->
        List.concat_map
          (fun ip ->
            match t.os with
            | Linux -> [ `Tcp (ip, port); `Udp (ip, port) ]
            | Macos -> [ `Udp (ip, port); `Tcp (ip, port) ])
          (Resolve.host ~os:t.os node)

  (* The loopback addresses are "localhost", as /etc/hosts names them, and
     any other a number, as glibc gives one no name server knows; a service
     is a number, as there is no services database. There is no host name to
     give a Unix address. *)
  (* An IPv4-compatible address, ::a.b.c.d past ::1, which glibc's
     getnameinfo names as its IPv4 address, and inet_ntop(3) writes dotted
     unless its first 16 of those bits are zero. *)
  let compatible (ip : Eio.Net.Ipaddr.v4v6) =
    let s = (ip :> string) in
    if
      String.length s = 16
      && String.for_all (( = ) '\000') (String.sub s 0 12)
      && Int32.unsigned_compare (String.get_int32_be s 12) 1l > 0
    then
      Some
        ( Eio.Net.Ipaddr.of_raw (String.sub s 12 4),
          s.[12] <> '\000' || s.[13] <> '\000' )
    else None

  let getnameinfo t = function
    | `Unix _ when t.os = Macos ->
        (* macOS's getnameinfo(3) names no Unix address, and OCaml's [Unix]
           raises [Not_found] then. *)
        raise Not_found
    | `Tcp (ip, port) | `Udp (ip, port) ->
        let lo = Eio.Net.Ipaddr.V4.loopback in
        let host =
          match compatible ip with
          | Some (v4, _) when same v4 lo -> "localhost"
          | Some (v4, true) -> Fmt.str "::%a" Eio.Net.Ipaddr.pp v4
          | _ when same (unmap ip) lo || same ip Eio.Net.Ipaddr.V6.loopback ->
              "localhost"
          | _ -> Fmt.str "%a" Eio.Net.Ipaddr.pp ip
        in
        (host, string_of_int port)
    | `Unix path ->
        (* A C string: an abstract address's ends at its first NUL. *)
        let service =
          match String.index_opt path '\000' with
          | Some i -> String.sub path 0 i
          | None -> path
        in
        ("localhost", service)
end

let handler = Eio.Net.Pi.network (module Network)

let capacity_rule = function
  | None -> Fun.id
  | Some c when c >= 1 -> Fun.const c
  | Some c -> Fmt.invalid_arg "eio-net.mem: the capacity is %d" c

let connection = Flow.connection

let pipe ?os ?capacity () =
  let os = kernel os in
  let end_ kind =
    let opts = Sockopt.v ~os ~family:V4 Unconnected in
    Sockopt.connect opts kind;
    opts
  in
  let (_, client), (_, server) =
    Flow.pair ~os ~local:false ~timer:None
      ~capacity:
        (capacity_rule capacity (Flow.default_capacity ~os ~local:false))
      ~bound:(Option.is_some capacity) ~client:(end_ Client)
      ~server:(end_ Server)
  in
  (client, server)

let network ?os ~capacity ~bounded ?clock ~sw () =
  let os = kernel os in
  let files = Files.v () in
  let now = Option.map (fun clock () -> Eio.Time.Mono.now clock) clock in
  let sleep = Option.map (fun clock d -> Eio.Time.Mono.sleep clock d) clock in
  let timer = Option.map timer clock in
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      ports = Ports.v ~os ?now ();
      files;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    {
      Datagram_socket.os;
      sockets = Hashtbl.create 8;
      ports = Ports.v ~os ();
      files;
      mutex = Mutex.create ();
      room = Eio.Condition.create ();
    }
  in
  Eio.Resource.T
    ( Network.{ sw; os; hub; datagrams; capacity; bounded; sleep; timer },
      handler )

let v ?os ?capacity ?clock ~sw () =
  network ?os ~capacity:(capacity_rule capacity)
    ~bounded:(Option.is_some capacity) ?clock ~sw ()

module Private = struct
  let v ?os ~capacity ?clock ~sw () =
    network ?os ~capacity ~bounded:true ?clock ~sw ()

  let silence = Flow.silence
end
