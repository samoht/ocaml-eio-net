open! Hermetic
open Kernel
open Address

(* Socket options, as the kernel [os] names answers them on a host with
   loopback only: what a socket reads back, defaults included, with the
   kernel's rounding and limits, and the error eio_posix raises where the
   kernel refuses an option for the socket's family or type. Options whose
   effect mem has no model of are kept and read back. The send and receive
   buffers size the queues: [resized] tells the socket's queues. *)
module O = Eio.Net.Sockopt

type family = Validation.family = V4 | V6 | Local

type kind = Validation.kind =
  | Unconnected
  | Listener
  | Client
  | Server
  | Datagram

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
  if t.os = Linux && t.family = V6 && (not (is_v4 ip)) && not (same ip (any ip))
  then t.v6only <- true

let tcp t = t.family <> Local && t.kind <> Datagram
let connected t = Validation.connected t.kind

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

let view (t : t) : Validation.socket =
  let maxseg =
    Option.value t.maxseg ~default:(Validation.default_maxseg t.family t.kind)
  in
  { os = t.os; family = t.family; kind = t.kind; maxseg }

(* What setsockopt(2) takes of a float: whole seconds and microseconds,
   cut as OCaml's Unix cuts them. Linux keeps whole milliseconds, rounded
   up, and a negative time as none. *)
let timeout t f =
  if f < 0. then 0.
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
  | Macos -> min n 8388608

(* The low-water mark of receives the kernel keeps: Linux takes a negative
   one as the largest, and caps a TCP one at half the receive buffer;
   macOS at the receive buffer. *)
let rcvlowat t v =
  match t.os with
  | Linux ->
      let v = if v < 0 then 0x7fffffff else v in
      let v =
        if not (tcp t) then v
        else min v (match t.rcvbuf with Some b -> b / 2 | None -> 16777216)
      in
      if v = 0 then 1 else v
  | Macos -> min v (rcvbuf t)

(* What the kernel keeps of a value it takes, and what setting it does: the
   buffers resize the queues. *)
let store : type a. t -> a O.t -> a -> unit =
 fun t opt v ->
  let linux = t.os = Linux in
  match opt with
  | O.SO_DEBUG -> t.debug <- v
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
  | O.SO_RCVLOWAT -> t.rcvlowat <- rcvlowat t v
  | O.SO_SNDLOWAT -> t.sndlowat <- Some (min v (sndbuf t))
  | O.SO_LINGER -> t.linger <- v
  | O.SO_RCVTIMEO -> t.rcvtimeo <- timeout t v
  | O.SO_SNDTIMEO -> t.sndtimeo <- timeout t v
  | O.TCP_NODELAY -> t.nodelay <- v
  | O.TCP_KEEPIDLE -> t.keepidle <- (if linux || v <> 0 then Some v else None)
  | O.TCP_KEEPINTVL -> t.keepintvl <- (if linux || v <> 0 then Some v else None)
  | O.TCP_KEEPCNT -> t.keepcnt <- (if linux || v <> 0 then Some v else None)
  | O.TCP_MAXSEG -> if linux then t.user_mss <- v else t.maxseg <- Some v
  | O.TCP_CONGESTION -> t.congestion <- Validation.congestion v
  | O.TCP_CORK -> t.cork <- v
  | O.TCP_USER_TIMEOUT -> t.user_timeout <- v
  | O.TCP_LINGER2 ->
      t.linger2 <- (match v with None -> 0 | Some n -> min n 120)
  | O.TCP_DEFER_ACCEPT -> t.defer_accept <- secs_to_retrans v
  | O.TCP_SYNCNT -> t.syncnt <- v
  | O.TCP_WINDOW_CLAMP ->
      t.window_clamp <- Some (if v = 0 then 0 else max v 1152)
  | O.TCP_QUICKACK -> t.quickack <- v
  | O.TCP_FASTOPEN ->
      (* The queue is at most [net.core.somaxconn] long. *)
      t.fastopen <- min v 4096
  | O.IP_FREEBIND -> t.freebind <- v
  | O.IP_BIND_ADDRESS_NO_PORT -> t.bind_no_port <- v
  | O.IP_LOCAL_PORT_RANGE -> t.port_range <- v
  | O.IP_TTL -> t.ttl <- v
  | O.IP_MTU_DISCOVER -> t.mtu_discover <- v
  | O.IPV6_ONLY -> t.v6only <- v
  | _ -> ()

let set t opt v =
  let opt = Posix.canonical opt in
  match Validation.set (view t) opt v with
  | Error r -> raise (Validation.exn `Set r)
  | Ok () -> store t opt v

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

(* The low-water mark of sends macOS reads back by default. *)
let sndlowat t =
  match t.sndlowat with
  | Some n -> n
  | None ->
      if t.os = Linux || (t.family = Local && t.kind = Datagram) then 1
      else 2048

(* The maximum segment size the kernel reads back. *)
let maxseg t =
  match (t.os, t.maxseg) with
  | Linux, Some n when connected t -> n
  | Linux, _ -> if t.user_mss > 0 then t.user_mss else 536
  | Macos, maxseg ->
      Option.value maxseg ~default:(Validation.default_maxseg t.family t.kind)

let get : type a. t -> a Eio.Net.Sockopt.t -> a =
 fun t opt ->
  let opt = Posix.canonical opt in
  Result.iter_error
    (fun r -> raise (Validation.exn `Get r))
    (Validation.get (view t) opt);
  let linux = t.os = Linux in
  match (Posix.extra opt, opt) with
  | Some Acceptconn, _ -> if linux then t.kind = Listener else missing ()
  | Some Error, _ -> 0
  | Some Type, _ -> if t.kind = Datagram then 2 else 1
  | None, _
    when t.os = Macos && t.family = Local && not (Validation.socket_level opt)
    ->
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
      | O.SO_SNDLOWAT -> sndlowat t
      | O.SO_LINGER -> t.linger
      | O.SO_RCVTIMEO -> t.rcvtimeo
      | O.SO_SNDTIMEO -> t.sndtimeo
      | O.TCP_NODELAY -> t.nodelay
      | O.TCP_KEEPIDLE -> Option.value t.keepidle ~default:7200
      | O.TCP_KEEPINTVL -> Option.value t.keepintvl ~default:75
      | O.TCP_KEEPCNT ->
          Option.value t.keepcnt ~default:(if linux then 9 else 8)
      | O.TCP_MAXSEG -> maxseg t
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

let linger t = t.linger
let on_resize t f = t.resized <- f
