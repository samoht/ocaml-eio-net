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
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)
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
let toward ~(bound : Eio.Net.Ipaddr.v4v6) (dst : Eio.Net.Ipaddr.v4v6) =
  let b = unmap bound in
  if same dst Eio.Net.Ipaddr.V6.any then
    if String.length (bound :> string) = 16 && is_v4 bound then
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

(* eio_posix raises bind(2)'s errors from [listen] and [datagram_socket] as a
   raw [Unix.Unix_error], and a [connect]'s as an {!Eio.Io} error. *)
let bind_fail code =
  raise
    (Posix.bind_error
       (match code with
       | Address_in_use -> `In_use
       | Address_not_available -> `Not_available
       | _ -> `Invalid))

(* The network has the loopback interface only: an IPv4 socket binds an
   address of 127.0.0.0/8, the broadcast address or a multicast one, an IPv6
   one ::1, either the wildcard address of its family, and an IPv6 one the
   IPv4-mapped form of an IPv4 address it binds, as on Linux. An IPv6
   address that needs a scope fails with EINVAL, and so does an IPv6
   multicast one for TCP. *)
let check_local ?(tcp = false) ?(fail = fail) ip =
  let ip = unmap ip in
  if needs_scope ip then fail Invalid_address;
  let local =
    if is_v4 ip then
      first_byte ip = 127 || same ip (any ip) || multicast_v4 ip || broadcast ip
    else if (ip :> string).[0] = '\xff' then begin
      if tcp then fail Invalid_address;
      true
    end
    else same ip Eio.Net.Ipaddr.V6.loopback || same ip (any ip)
  in
  if not local then fail Address_not_available

(* A path with a NUL past its first byte, which is not an abstract address. *)
let embedded_nul path =
  match String.index_opt path '\000' with Some i -> i > 0 | None -> false

(* OCaml's [Unix] refuses a path that [sun_path], 108 bytes on Linux, cannot
   hold with its NUL, and one with a NUL past its first byte with ENOENT, as
   no file has that name: raw from a bind, as [Eio.Fs.Not_found] from a
   [connect] or a [send]. *)
let check_path ?(raw = false) path =
  if String.length path >= 108 then
    if raw then raise (Posix.bind_error (`Name_too_long path))
    else fail Name_too_long
  else if embedded_nul path then
    if raw then raise (Posix.bind_error (`No_file path))
    else raise (Eio.Fs.err (Not_found No_listener))

(* The error a socket at [src] meets reaching [dst] across address families,
   IPv6 against IPv4-mapped, as on Linux. *)
let cross_family (src : Eio.Net.Ipaddr.v4v6) (dst : Eio.Net.Ipaddr.v4v6) =
  if String.length (src :> string) <> 16 || String.length (dst :> string) <> 16
  then None
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
   on it, not the connections. [lowest] is a port of the dynamic range (RFC
   6335, section 6) at or below the lowest one nothing holds: every port
   below it is held; [highest] is one at or above the highest free one. *)
module Ports = struct
  module Untils = Map.Make (Mtime)

  type group = {
    ip : Eio.Net.Ipaddr.v4v6;
    wildcard : bool;
    reuse : bool;
    listening : bool;
    mutable lasting : int;
    mutable untils : int Untils.t;
  }

  type hold = {
    group : group;
    mutable until : Mtime.t option;
    mutable released : bool;
  }

  type t = {
    held : (int, group list) Hashtbl.t;
    mutable lowest : int;
    mutable highest : int;
    now : (unit -> Mtime.t) option;
  }

  let v ?now () =
    { held = Hashtbl.create 64; lowest = 49152; highest = 65535; now }

  (* TCP_TIMEWAIT_LEN on Linux, and its default tcp_fin_timeout. *)
  let time_wait = Mtime.Span.(60 * s)
  let fin_timeout = Mtime.Span.(60 * s)

  let freed t port =
    if port >= 49152 && port < t.lowest then t.lowest <- port;
    if port <= 65535 && port > t.highest then t.highest <- port

  let count n = function None -> n | Some m -> m + n

  let untils_add u g =
    g.untils <- Untils.update u (fun c -> Some (count 1 c)) g.untils

  let untils_remove u g =
    g.untils <-
      Untils.update u
        (function Some c when c > 1 -> Some (c - 1) | _ -> None)
        g.untils

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

  let add t ?(wildcard = false) ?(reuse = false) ?(listening = false) (ip, port)
      =
    let groups = at t port in
    let alike g =
      same g.ip ip && g.wildcard = wildcard && g.reuse = reuse
      && g.listening = listening
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
              lasting = 0;
              untils = Untils.empty;
            }
          in
          Hashtbl.replace t.held port (g :: groups);
          g
    in
    group.lasting <- group.lasting + 1;
    { group; until = None; released = false }

  let remove t port h =
    if not h.released then begin
      h.released <- true;
      match h.until with
      | None -> h.group.lasting <- h.group.lasting - 1
      | Some u -> untils_remove u h.group
    end;
    ignore (at t port : group list)

  (* [h] lets go at [until], or later if it already would. *)
  let set_until h until =
    match h.until with
    | _ when h.released -> ()
    | Some u when Mtime.is_later u ~than:until -> ()
    | Some u ->
        untils_remove u h.group;
        untils_add until h.group;
        h.until <- Some until
    | None ->
        h.group.lasting <- h.group.lasting - 1;
        untils_add until h.group;
        h.until <- Some until

  (* A bind at [ip] conflicts with a hold at an overlapping address, unless
     both sockets have [SO_REUSEADDR] and the held one does not listen, as
     Linux has it. A client bound at a wildcard address and connected holds
     its source address, and conflicts with a bind at a wildcard address
     too. *)
  let in_use t ?(reuse = false) (ip, port) =
    let wild = same (unmap ip) (any (unmap ip)) in
    List.exists
      (fun g ->
        (ip_overlaps (ip, port) (g.ip, port) || (g.wildcard && wild))
        && not (reuse && g.reuse && not g.listening))
      (at t port)

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

(* A stream option is ignored only where the queues already provide what it
   asks for, so ignoring it yields the requested behaviour rather than
   withholding it: [TCP_NODELAY] and [TCP_QUICKACK]. Every other option would
   change what a caller observes and is refused by name, as is every option on
   a datagram socket. *)
module Sockopt = struct
  let refuse opt =
    Fmt.invalid_arg "eio-net.mem does not support socket option %a"
      Eio.Net.Sockopt.pp opt

  let stream : type a. a Eio.Net.Sockopt.t -> unit = function
    | Eio.Net.Sockopt.TCP_NODELAY -> ()
    | Eio.Net.Sockopt.TCP_QUICKACK -> ()
    | opt -> refuse opt

  let datagram : type a. a Eio.Net.Sockopt.t -> unit = refuse
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
   reader that shut its receive side raises the reset, as EPIPE. *)
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
    local : bool;
    capacity : int;
    mutable size : int; (* Bytes queued and not yet read. *)
    mutable unacked : int;
    readers : Waiters.t;
    writers : Waiters.t;
  }

  let v ~local ~capacity =
    {
      local;
      mutex = Mutex.create ();
      queued = Queue.create ();
      head = 0;
      shut = false;
      reset = false;
      gone = false;
      refused = false;
      rcv_shut = false;
      fin_sent = false;
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

  (* Waits for the wake-up of [waiter], registered in [q]. *)
  let await t q (p, waiter) =
    match Eio.Promise.await p with
    | () -> ()
    | exception (Eio.Cancel.Cancelled _ as e) ->
        locked t (fun _ ->
            if Waiters.remove q waiter then ((), [])
            else begin
              q.turn <- false;
              ((), Waiters.pass q)
            end);
        raise e

  (* Changes the state under the mutex and wakes every waiter. *)
  let update t f =
    locked t (fun t ->
        f t;
        ((), Waiters.take_all t.readers @ Waiters.take_all t.writers))

  (* The reset reaches the reader once it has read what is queued. A TCP
     reader that has the writer's end-of-file keeps reading end-of-file, as
     Linux reports the end of a stream before a reset that came after it. *)
  let reset t =
    update t (fun t -> if t.local || not t.shut then t.reset <- true)

  (* The reset was reported: the stream has ended. *)
  let consume_reset t =
    update t (fun t ->
        if t.reset then begin
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

  let rec write ?(turn = false) t ~back bufs =
    let n = Cstruct.lenv bufs in
    let result =
      locked t @@ fun t ->
      let leave outcome = (outcome, Waiters.release t.writers ~turn) in
      (* An empty write sends nothing, so it meets no shutdown or closed
         peer, as on Linux. *)
      if n = 0 then leave (`Done 0)
      else if t.shut then leave `Shut
      else if t.refused then leave `Refused
      else if t.gone then begin
        t.refused <- true;
        if t.local then leave (`Raise (reset_exn ())) else leave (`Lost n)
      end
      else if t.rcv_shut && t.local then leave (`Raise (reset_exn ()))
      else if
        Waiters.must_wait t.writers ~turn || t.size + t.unacked >= t.capacity
      then (`Wait (Waiters.queue t.writers ~turn), [])
      else if t.rcv_shut && t.fin_sent then begin
        (* Data a full queue held back resets the connection once it is
           sent; a write that waited for room reports the reset. *)
        t.reset <- true;
        t.refused <- true;
        if turn then leave (`Raise (reset_exn ())) else leave (`Lost n)
      end
      else begin
        if turn then t.writers.turn <- false;
        let k = min n (t.capacity - t.size - t.unacked) in
        push t bufs k;
        t.size <- t.size + k;
        let next =
          if t.size + t.unacked < t.capacity then Waiters.pass t.writers else []
        in
        (`Done k, Waiters.pass t.readers @ next)
      end
    in
    match result with
    | `Wait w ->
        await t t.writers w;
        write ~turn:true t ~back bufs
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
  let rec read ?(turn = false) t dst =
    let result =
      locked t @@ fun t ->
      if Waiters.must_wait t.readers ~turn then
        (`Wait (Waiters.queue t.readers ~turn), [])
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
        await t t.readers w;
        read ~turn:true t dst

  let shut t = update t (fun t -> t.shut <- true)

  (* The reader shuts its receive side: it still reads what is queued and
     what arrives, as on Linux, and end-of-file once nothing is; what it
     reads frees no room for the writer. *)
  let shut_receive t = update t (fun t -> t.rcv_shut <- true)

  (* The reader shut its send side. *)
  let fin_sent t = update t (fun t -> t.fin_sent <- true)

  (* The reader closes: bytes it left unread answer the writer with a reset,
     which refuses its writes at once. *)
  let leave t =
    locked t @@ fun t ->
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
       closes the socket, [closed]. *)
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
    }

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
      use t "write" @@ fun () -> Pipe.write t.to_peer ~back:t.from_peer bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    (* An empty read is end-of-file, as Eio's backends report the 0 that
       read(2) returns for it. *)
    let single_read t buf =
      use t "read" @@ fun () ->
      if Cstruct.length buf = 0 then raise End_of_file
      else Pipe.read t.from_peer buf

    let shutdown t cmd =
      use t "shutdown" @@ fun () ->
      match cmd with
      | `Send ->
          Pipe.shut t.to_peer;
          Pipe.fin_sent t.from_peer;
          t.on_fin ()
      | `Receive -> Pipe.shut_receive t.from_peer
      | `All ->
          Pipe.shut t.to_peer;
          Pipe.fin_sent t.from_peer;
          Pipe.shut_receive t.from_peer;
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

    let close t = close_with ~abort:false t
    let read_methods = []
    let setsockopt _ opt _ = Sockopt.stream opt

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
  end

  type (_, _, _) Eio.Resource.pi +=
    | Connection : ('t, 't -> int, [> `Generic ]) Eio.Resource.pi

  let handler =
    Eio.Resource.handler
      (H (Connection, fun (t : Impl.t) -> t.connection)
      :: Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module Impl)))

  type t = Impl.t * Impl.tag Eio.Net.stream_socket_ty Eio.Resource.t

  let v ~connection ~from_peer ~to_peer =
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
  let default_capacity ~local = if local then 256 * 1024 else 4 * 1024 * 1024

  (* A connection to itself: what it writes, it reads. *)
  let self ~capacity =
    let connection = Atomic.fetch_and_add connections 1 in
    let pipe = Pipe.v ~local:false ~capacity in
    v ~connection ~from_peer:pipe ~to_peer:pipe

  let pair ~local ~capacity =
    let connection = Atomic.fetch_and_add connections 1 in
    let to_client = Pipe.v ~local ~capacity in
    let to_server = Pipe.v ~local ~capacity in
    let client = v ~connection ~from_peer:to_client ~to_peer:to_server in
    let server = v ~connection ~from_peer:to_server ~to_peer:to_client in
    (client, server)

  let connection (Eio.Resource.T (t, ops) : _ Eio.Net.stream_socket) =
    match Eio.Resource.get_opt ops Connection with
    | Some f -> Some (f t)
    | None -> None

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
  }

  (* Linux's accept queue takes one connection more than the backlog, which
     it caps at [net.core.somaxconn], 4096 by default, as it takes a negative
     one. *)
  let queue_length backlog =
    if backlog < 0 || backlog > 4096 then 4097 else backlog + 1

  let is_closed t = Mutex.protect t.lock (fun () -> t.closed)

  (* A Unix-domain path is taken over with [reuse_addr], as Eio's POSIX backend
     removes the socket file there before it binds; the listener that held it
     stays open, unreachable. *)
  let bind hub ~reuse_addr ~backlog addr =
    let addr =
      match addr with
      | `Tcp (ip, 0) ->
          Option.map (fun port -> `Tcp (ip, port)) (Ports.free hub.ports)
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
              backlog = queue_length backlog;
              acceptable = Eio.Condition.create ();
              room = Eio.Condition.create ();
              users = 0;
              closing = false;
              closed = false;
              hooks = [];
              reuse = reuse_addr;
              hold;
            }
          in
          Hashtbl.replace hub.listeners addr t;
          Ok t

  (* The refusals are raised once the mutex is released, which an exception
     inside it would poison. *)
  let v hub ~reuse_addr ~backlog addr =
    (match addr with
    | `Tcp (ip, _) -> check_local ~tcp:true ~fail:bind_fail ip
    | `Unix path -> check_path ~raw:true path);
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

  (* Queues a connection, waiting while [backlog] do; [false] if the
     listener is closed. *)
  let offer t conn =
    let queued =
      Eio.Condition.loop_no_mutex t.room @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if t.closed then Some false
      else if Queue.length t.incoming < t.backlog then begin
        Queue.push conn t.incoming;
        Some true
      end
      else None
    in
    if queued then Eio.Condition.broadcast t.acceptable;
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

  let setsockopt _ opt _ = Sockopt.stream opt

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
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
    sockets : (Eio.Net.Sockaddr.datagram, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Mutex.t;
    room : Eio.Condition.t;
  }

  (* [rmem] is the memory the datagrams of [queued] take, and [wmem] that of
     the Unix-domain datagrams this socket sent that are still queued, as
     Linux counts them; both are guarded by [lock], [wmem] by the sender's. *)
  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : datagram Queue.t;
    mutable rmem : int;
    mutable wmem : int;
    readable : Eio.Condition.t;
    mutable closed : bool;
    mutable send_shut : bool;
    mutable recv_shut : bool;
    mutable release : Eio.Switch.hook;
    hold : Ports.hold option;
  }

  and datagram = {
    src : Eio.Net.Sockaddr.datagram;
    data : string;
    size : int;
    sender : t option;
  }

  (* Linux's default receive and send buffers, [rmem_default] and
     [wmem_default], on the kernel the tests record. *)
  let buffer = 229376

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
     datagrams makes a send to it wait. *)
  let max_dgram_qlen = 10

  (* A port of zero with none of the range free is bind(2)'s EADDRINUSE. *)
  let free_port hub =
    match Ports.free hub.ports with
    | Some port -> port
    | None -> bind_fail Address_in_use

  let v hub ~reuse addr =
    (match addr with
    | `Udp (ip, _) -> check_local ~fail:bind_fail ip
    | `Unix path -> check_path ~raw:true path
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
        wmem = 0;
        readable = Eio.Condition.create ();
        closed = false;
        send_shut = false;
        recv_shut = false;
        release = Eio.Switch.null_hook;
        hold;
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

  (* The longest datagram a socket sends to [dst]: UDP's payload over IPv4 or
     IPv6, or what Linux's default send buffer (212992 bytes, less 32) lets a
     Unix-domain socket send. *)
  let max_length : Eio.Net.Sockaddr.datagram -> int = function
    | `Unix _ -> 212960
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
  let refusal t dst bufs =
    let len = Cstruct.lenv bufs in
    let too_long = len > max_length dst in
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
        match cross_family b d16 with
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
  let deliver t ~local ~dst peer bufs =
    let data = Cstruct.to_string (Cstruct.concat bufs) in
    let n = String.length data in
    let delivered =
      if local && Mutex.protect t.lock (fun () -> t.wmem >= buffer) then `Wait
      else
        Mutex.protect peer.lock @@ fun () ->
        if local && peer.recv_shut then `Refused
        else if peer.closed then `Lost
        else if local then
          if Queue.length peer.queued > max_dgram_qlen then `Wait
          else begin
            let size = unix_size n in
            Mutex.protect t.lock (fun () -> t.wmem <- t.wmem + size);
            Queue.push
              { src = source t ~dst peer; data; size; sender = Some t }
              peer.queued;
            `Delivered
          end
        else
          let v4 = match dst with `Udp (d, _) -> is_v4 d | `Unix _ -> false in
          let size = udp_size ~v4 n in
          if peer.rmem + size > buffer then `Lost
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
      (fun s ->
        if s == t then s.wmem <- s.wmem - d.size
        else Mutex.protect s.lock (fun () -> s.wmem <- s.wmem - d.size))
      d.sender

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
          let code = if local then Not_connected else No_destination in
          raise (Eio.Exn.create (Eio.Exn.X code))
    in
    shut ();
    dst

  let send t ?dst bufs =
    if t.closed then invalid_arg "eio-net.mem: send on a closed datagram socket";
    (match dst with Some (`Unix path) -> check_path path | _ -> ());
    let local = match t.addr with `Unix _ -> true | `Udp _ -> false in
    let dst =
      match (destination t ~local dst, t.addr) with
      | `Udp (d, port), `Udp (bound, _) -> `Udp (toward ~bound d, port)
      | dst, _ -> dst
    in
    Option.iter
      (function
        | Broadcast -> raise (Eio.Fs.err (Permission_denied Broadcast))
        | code -> raise (Eio.Exn.create (Eio.Exn.X code)))
      (refusal t dst bufs);
    (* A send that waits looks for its destination again, as Linux does, and
       a Unix-domain one fails once its socket shuts its sending. *)
    Eio.Condition.loop_no_mutex t.hub.room @@ fun () ->
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
        | `Wait -> None)

  (* A datagram longer than [buf] is cut to it, as a socket cuts it. *)
  let recv t buf =
    let got =
      Eio.Condition.loop_no_mutex t.readable @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if t.closed then
        invalid_arg "eio-net.mem: recv on a closed datagram socket"
      else
        match Queue.take_opt t.queued with
        | None -> None
        | Some d ->
            freed t d;
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

  (* A send waiting for room on this socket, or from it, looks again. *)
  let shutdown t cmd =
    ( Mutex.protect t.lock @@ fun () ->
      if t.closed then
        invalid_arg "eio-net.mem: shutdown on a closed datagram socket";
      match cmd with
      | `Send -> t.send_shut <- true
      | `Receive -> t.recv_shut <- true
      | `All ->
          t.send_shut <- true;
          t.recv_shut <- true );
    Eio.Condition.broadcast t.hub.room

  let setsockopt _ opt _ = Sockopt.datagram opt

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
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
    if n = 0 then Some (false, false, 0)
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
          (fun acc c -> ((acc * 10) + Char.code c - 48) land 0xffff_ffff)
          0 ds
      in
      Some (neg, overflow, low)

  (* glibc keeps strtoul's result in an int: a negative one is no number, and
     the port is its low 16 bits. *)
  let port service =
    match strtoul service with
    | None -> None
    | Some (_, true, _) -> None
    | Some (neg, false, low) ->
        let v = if neg then (0x1_0000_0000 - low) land 0xffff_ffff else low in
        if v >= 0x8000_0000 then None else Some (v land 0xffff)

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
                  let v = (v * base) + d in
                  if v > 0xffff_ffff then None else Some v)
          (Some 0) digits
    in
    if n >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
      value 16 (String.sub s 2 (n - 2))
    else if n >= 1 && s.[0] = '0' then value 8 s
    else value 10 s

  (* An IPv4 address as inet_aton(3) reads it: one to four parts, the last
     filling the bytes left. *)
  let inet_aton s =
    let parts = List.map part (String.split_on_char '.' s) in
    if List.mem None parts then None
    else
      let parts = List.map Option.get parts in
      let k = List.length parts in
      let rec fold acc = function
        | [] -> Some acc
        | [ last ] ->
            let bits = 8 * (5 - k) in
            if k > 4 || last >= 1 lsl bits then None
            else Some ((acc lsl bits) lor last)
        | x :: rest -> if x > 255 then None else fold ((acc lsl 8) lor x) rest
      in
      match fold 0 parts with
      | None -> None
      | Some v ->
          Some
            (Eio.Net.Ipaddr.of_raw
               (String.init 4 (fun i ->
                    Char.chr ((v lsr (8 * (3 - i))) land 255))))

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
  let scope ip s =
    let value =
      String.fold_left
        (fun v c -> if v > 0xffff_ffff then v else (v * 10) + Char.code c - 48)
        0 s
    in
    (s <> "" && String.for_all is_digit s && value <= 0xffff_ffff)
    || (s = "lo" && needs_scope ip)

  let number name =
    match inet_aton name with
    | Some ip -> Some ip
    | None -> (
        match String.index_opt name '%' with
        | None -> ipv6 name
        | Some i -> (
            match ipv6 (String.sub name 0 i) with
            | Some ip
              when scope ip
                     (String.sub name (i + 1) (String.length name - i - 1)) ->
                Some ip
            | Some _ | None -> None))

  (* The addresses of a host: a number as it reads, the loopback addresses
     for "localhost" and the empty name, IPv6 first as glibc and macOS order
     them on a dual-stack host, and nothing for any other name. *)
  let host name : Eio.Net.Ipaddr.v4v6 list =
    let loopback =
      [
        (Eio.Net.Ipaddr.V6.loopback :> Eio.Net.Ipaddr.v4v6);
        (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6);
      ]
    in
    if name = "" || localhost name then loopback
    else Option.to_list (number name)
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
  type decision = [ `Held | `Release | `Until of Mtime.t ]

  type t = {
    lock : Mutex.t;
    now : (unit -> Mtime.t) option;
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
              let u = after Ports.time_wait peer in
              if Mtime.is_later u ~than:closed then `Until u else `Release
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
    hub : Listening_socket.hub;
    datagrams : Datagram_socket.hub;
    capacity : int option;
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
    | `Unix path when embedded_nul path ->
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

  let rec check_options : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (opt, _) :: rest ->
        Sockopt.stream opt;
        check_options rest

  (* Holds the address of a client that connects to [ip] in the listeners'
     ports, until [release]: the loopback address of its destination's
     family, in the destination's form, or the one it is bound at, at a free port counted
     down from the top of the dynamic range as listeners count up from its
     bottom. *)
  let hold t ?bind (ip : Eio.Net.Ipaddr.v4v6) =
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
      match if port = 0 then Ports.free_down ports else Some port with
      | None -> Error `No_port
      | Some port -> Ok ((ip, port), Ports.add ports ~wildcard (ip, port))

  (* Lets go of [h] now, or at [until]. Not cancelled, even while it waits
     for the mutex: a release that gave up would hold the port for good. *)
  let release_port t ?until port h =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        let ports = t.hub.Listening_socket.ports in
        match until with
        | Some until -> Ports.set_until h until
        | None -> Ports.remove ports port h)

  (* A connect's socket has its destination's family, and fails to bind an
     address of another as Linux's bind(2) does: an IPv4 socket with
     EAFNOSUPPORT, an IPv6 or Unix-domain one with EINVAL. IPv6 against
     IPv4-mapped fails as on Linux. *)
  let check_bind (bind_to : Eio.Net.Sockaddr.stream option)
      (addr : Eio.Net.Sockaddr.stream) =
    match (bind_to, addr) with
    | None, _ | Some (`Unix _), `Unix _ -> ()
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string) -> (
        match cross_family b d with
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
  let connect_held t ~sw ~bind_to ~(release : ?until:Mtime.t -> unit -> unit)
      addr local =
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
        then fail Network_unreachable;
        if needs_scope d' then fail Invalid_address;
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
    | None, `Tcp _ when to_itself ->
        let capacity =
          Option.value t.capacity ~default:(Flow.default_capacity ~local:false)
        in
        let flow = Flow.self ~capacity in
        let ends = Ends.v t.hub.Listening_socket.ports in
        (* Its FIN reaches itself: it holds its address in TIME_WAIT. *)
        Flow.on_close flow (fun ~reset ~abort:_ ->
            ignore (Ends.close ends `Client ~reset ~abort:false);
            match Ends.close ends `Server ~reset ~abort:false with
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
          Option.value t.capacity ~default:(Flow.default_capacity ~local:unix)
        in
        let client, server = Flow.pair ~local:unix ~capacity in
        (* The accepted socket holds the listener's port at the address the
           client reached, with the listener's [SO_REUSEADDR], as Linux's
           does. *)
        let child =
          match addr with
          | `Unix _ -> None
          | `Tcp (ip, port) ->
              let reached = List.hd (reaching ip) in
              Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
                  Some
                    ( port,
                      Ports.add t.hub.Listening_socket.ports
                        ~reuse:listener.reuse (reached, port) ))
        in
        let ends = Ends.v t.hub.Listening_socket.ports in
        let settle (client, server) =
          let apply f = function
            | `Held -> ()
            | `Release -> f None
            | `Until u -> f (Some u)
          in
          apply (fun until -> release ?until ()) client;
          Option.iter
            (fun (port, h) ->
              apply (fun until -> release_port t ?until port h) server)
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
        | true -> flow
        | false ->
            (* The listener closed while the connection waited for room. *)
            fail ();
            refused ()
        | exception e ->
            fail ();
            raise e)

  (* A connection is a pair of queues. A TCP client holds its address until
     its flow closes; one bound at an address a socket holds finds it in
     use. *)
  let connect t ~bind_to ~options ~sw addr =
    check_options options;
    Eio.Switch.check sw;
    (match addr with `Unix path -> check_path path | `Tcp _ -> ());
    let addr =
      match (addr, bind_to) with
      | `Tcp (d, port), Some (`Tcp (bound, _)) -> `Tcp (toward ~bound d, port)
      | addr, _ -> addr
    in
    check_bind bind_to addr;
    let bind_to =
      match bind_to with
      | Some (`Unix path) -> Some (`Unix (Files.bound t.hub.files path))
      | bind_to -> bind_to
    in
    (match bind_to with
    | Some (`Tcp (ip, _)) -> check_local ~tcp:true ip
    | Some (`Unix path) -> check_path path
    | None -> ());
    let held =
      match (addr, bind_to) with
      | `Unix _, Some (`Unix path) ->
          if Files.bind t.hub.files path Client then `Path path
          else raise (address_in_use ())
      | `Unix _, _ -> `Nothing
      | `Tcp (ip, _), bind -> (
          let bind = match bind with Some (`Tcp b) -> Some b | _ -> None in
          match hold t ?bind ip with
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
    let release ?until () =
      match held with
      | `Port ((_, port), h) -> release_port t ?until port h
      | `Path path -> Files.leave t.hub.files path Client
      | `Nothing -> ()
    in
    match connect_held t ~sw ~bind_to ~release addr local with
    | flow -> flow
    | exception e ->
        (* eio_posix and eio_linux keep a failed connect's socket on [sw], so
           the address it was bound at stays bound until [sw] finishes, as on
           Linux. *)
        (match bind_to with
        | None -> release ()
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
  let getaddrinfo _ ~service node =
    match Resolve.port service with
    | _ when node = "" && service = "" -> []
    | None -> raise (Eio.Net.err (Address_lookup_failed SERVICE))
    | Some port ->
        List.concat_map
          (fun ip -> [ `Tcp (ip, port); `Udp (ip, port) ])
          (Resolve.host node)

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

  let getnameinfo _ = function
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

let check_capacity = function
  | None -> None
  | Some c when c >= 1 -> Some c
  | Some c -> Fmt.invalid_arg "eio-net.mem: the capacity is %d" c

let connection = Flow.connection

let pipe ?capacity () =
  let capacity =
    Option.value (check_capacity capacity)
      ~default:(Flow.default_capacity ~local:false)
  in
  let (_, client), (_, server) = Flow.pair ~local:false ~capacity in
  (client, server)

let v ?capacity ?clock ~sw () =
  let capacity = check_capacity capacity in
  let files = Files.v () in
  let now = Option.map (fun clock () -> Eio.Time.Mono.now clock) clock in
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      ports = Ports.v ?now ();
      files;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    {
      Datagram_socket.sockets = Hashtbl.create 8;
      ports = Ports.v ();
      files;
      mutex = Mutex.create ();
      room = Eio.Condition.create ();
    }
  in
  Eio.Resource.T (Network.{ sw; hub; datagrams; capacity }, handler)
