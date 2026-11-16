open! Hermetic
open Kernel
module O = Eio.Net.Sockopt

type family = V4 | V6 | Local
type role = Client | Server
type stream = Unconnected | Listening | Connected of role
type kind = Stream of stream | Datagram
type socket = { os : os; family : family; kind : kind; maxseg : int }

type errno =
  [ `No_option
  | `Not_supported
  | `Invalid
  | `Denied
  | `Unknown
  | `Not_connected
  | `Domain ]

type refusal = Eio_check of string | Missing | Errno of errno

let connected = function
  | Stream (Connected _) -> true
  | Stream (Unconnected | Listening) | Datagram -> false

let default_maxseg family kind =
  match (family, kind) with
  | V4, Stream (Connected Client) -> 16332
  | V6, Stream (Connected Client) -> 16312
  | V4, Stream (Connected Server) -> 4024
  | V6, Stream (Connected Server) -> 4004
  | V6, _ -> 1024
  | _ -> 512

type level = Socket | Tcp | Ip | Ipv6 | Linux_tcp | Linux_ip

let level : type a. a O.t -> level = function
  | O.TCP_NODELAY | O.TCP_KEEPIDLE | O.TCP_KEEPINTVL | O.TCP_KEEPCNT
  | O.TCP_MAXSEG ->
      Tcp
  | O.TCP_CONGESTION | O.TCP_CORK | O.TCP_USER_TIMEOUT | O.TCP_LINGER2
  | O.TCP_DEFER_ACCEPT | O.TCP_SYNCNT | O.TCP_WINDOW_CLAMP | O.TCP_QUICKACK
  | O.TCP_FASTOPEN ->
      Linux_tcp
  | O.IP_TTL -> Ip
  | O.IP_FREEBIND | O.IP_BIND_ADDRESS_NO_PORT | O.IP_LOCAL_PORT_RANGE | O.IP_MTU
  | O.IP_MTU_DISCOVER ->
      Linux_ip
  | O.IPV6_ONLY -> Ipv6
  | _ -> Socket

let socket_level opt = level opt = Socket

let non_negative name v =
  if v < 0 then Some (Fmt.str "%s must be non-negative, got %d" name v)
  else None

let port_range (lower, upper) =
  if lower < 0 || lower > 65535 then
    Some
      (Fmt.str "O.IP_LOCAL_PORT_RANGE lower bound must be 0-65535, got %d" lower)
  else if upper < 0 || upper > 65535 then
    Some
      (Fmt.str "O.IP_LOCAL_PORT_RANGE upper bound must be 0-65535, got %d" upper)
  else if lower <> 0 && upper <> 0 && lower > upper then
    Some
      (Fmt.str
         "O.IP_LOCAL_PORT_RANGE lower bound (%d) must be <= upper bound (%d)"
         lower upper)
  else None

(* The checks Eio makes before it calls setsockopt(2), whatever the
   kernel. *)
let eio_check : type a. a O.t -> a -> string option =
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
          Some (Fmt.str "O.TCP_LINGER2 must be non-negative, got %d" n)
      | _ -> None)
  | O.TCP_DEFER_ACCEPT -> non_negative "O.TCP_DEFER_ACCEPT" v
  | O.TCP_SYNCNT ->
      if v < 1 || v > 255 then
        Some (Fmt.str "O.TCP_SYNCNT must be between 1 and 255, got %d" v)
      else None
  | O.TCP_WINDOW_CLAMP -> non_negative "O.TCP_WINDOW_CLAMP" v
  | O.TCP_FASTOPEN ->
      if v < 0 then
        Some
          (Fmt.str "O.TCP_FASTOPEN queue length must be non-negative, got %d" v)
      else None
  | O.IP_LOCAL_PORT_RANGE -> port_range v
  | O.IP_TTL ->
      if v < 1 || v > 255 then
        Some (Fmt.str "O.IP_TTL must be between 1 and 255, got %d" v)
      else None
  | O.IP_MTU -> Some "O.IP_MTU is a read-only socket option"
  | _ -> None

let errno e = Some (Errno e)
let invalid = errno `Invalid

(* Linux refuses an option at a level the socket's family or type lacks. *)
let gate_linux ~op s level =
  let set = op = `Set in
  match (level, s.family) with
  | Socket, _ -> None
  | _, Local -> errno `Not_supported
  | (Tcp | Linux_tcp), V4 when s.kind = Datagram ->
      errno (if set then `No_option else `Not_supported)
  | (Tcp | Linux_tcp), V6 when s.kind = Datagram -> errno `No_option
  | Ipv6, V4 -> errno (if set then `No_option else `Not_supported)
  | _ -> None

(* eio_posix has no Linux-only option on macOS. macOS reads an IP or TCP
   option on a Unix-domain socket as the local option of the same number,
   which a get answers and a set may not change. *)
let gate_macos ~op s level =
  match (level, s.family) with
  | Socket, _ -> None
  | (Linux_tcp | Linux_ip), _ -> Some Missing
  | Ipv6, Local -> errno `Not_supported
  | _, Local -> if op = `Set then errno `Not_supported else None
  | Tcp, _ when s.kind = Datagram -> invalid
  | Ipv6, V4 | Ip, V6 -> invalid
  | _ -> None

(* Up to the first NUL and 15 bytes, as Linux reads the name. *)
let congestion name =
  let name =
    match String.index_opt name '\000' with
    | Some i -> String.sub name 0 i
    | None -> name
  in
  if String.length name > 15 then String.sub name 0 15 else name

(* The values Linux refuses. Only CAP_NET_ADMIN turns [SO_DEBUG] on. *)
let linux : type a. socket -> a O.t -> a -> refusal option =
 fun s opt v ->
  match opt with
  | O.SO_DEBUG -> if v then errno `Denied else None
  | O.SO_SNDLOWAT -> errno `No_option
  | O.TCP_KEEPIDLE -> if v < 1 || v > 32767 then invalid else None
  | O.TCP_KEEPINTVL -> if v < 1 || v > 32767 then invalid else None
  | O.TCP_KEEPCNT -> if v < 1 || v > 127 then invalid else None
  | O.TCP_MAXSEG -> if v <> 0 && (v < 88 || v > 32767) then invalid else None
  | O.TCP_CONGESTION -> (
      match congestion v with
      | "" -> invalid
      | "reno" | "cubic" -> None
      | _ -> errno `Unknown)
  | O.TCP_SYNCNT -> if v > 127 then invalid else None
  | O.TCP_WINDOW_CLAMP ->
      if v = 0 && s.kind <> Stream Unconnected then invalid else None
  | O.TCP_FASTOPEN -> if connected s.kind then invalid else None
  | O.IPV6_ONLY -> if s.kind <> Stream Unconnected then invalid else None
  | O.SO_BROADCAST | O.SO_REUSEADDR | O.SO_KEEPALIVE | O.SO_DONTROUTE
  | O.SO_OOBINLINE | O.SO_REUSEPORT | O.TCP_NODELAY | O.TCP_CORK
  | O.TCP_QUICKACK | O.IP_FREEBIND | O.IP_BIND_ADDRESS_NO_PORT ->
      None
  | O.SO_SNDBUF | O.SO_RCVBUF | O.SO_RCVLOWAT | O.TCP_USER_TIMEOUT
  | O.TCP_DEFER_ACCEPT | O.IP_TTL ->
      None
  | O.SO_LINGER | O.TCP_LINGER2 -> None
  | O.SO_RCVTIMEO | O.SO_SNDTIMEO -> None
  | O.IP_LOCAL_PORT_RANGE -> None
  | O.IP_MTU_DISCOVER -> None
  | _ -> Some Missing

(* The values macOS refuses. *)
let macos : type a. socket -> a O.t -> a -> refusal option =
 fun s opt v ->
  match opt with
  | O.SO_RCVLOWAT -> if v < 1 then invalid else None
  | O.SO_SNDLOWAT -> if v < 1 then invalid else None
  | O.SO_SNDBUF -> if v <= 0 then invalid else None
  | O.SO_RCVBUF -> if v <= 0 then invalid else None
  | O.SO_LINGER -> (
      match v with Some n when n < 0 || n > 32767 -> errno `Domain | _ -> None)
  | O.SO_RCVTIMEO -> if v < 0. then errno `Domain else None
  | O.SO_SNDTIMEO -> if v < 0. then errno `Domain else None
  | O.TCP_MAXSEG ->
      if v <= 0 || v > s.maxseg || v + 40 < 216 then invalid else None
  | O.IPV6_ONLY -> if s.kind <> Stream Unconnected then invalid else None
  | O.SO_DEBUG | O.SO_BROADCAST | O.SO_REUSEADDR | O.SO_KEEPALIVE
  | O.SO_DONTROUTE | O.SO_OOBINLINE | O.SO_REUSEPORT | O.TCP_NODELAY ->
      None
  | O.TCP_KEEPIDLE | O.TCP_KEEPINTVL | O.TCP_KEEPCNT | O.IP_TTL -> None
  | _ -> Some Missing

let refusal : type a. socket -> a O.t -> a -> refusal option =
 fun s opt v ->
  match eio_check opt v with
  | Some msg -> Some (Eio_check msg)
  | None -> (
      let gate = match s.os with Linux -> gate_linux | Macos -> gate_macos in
      match gate ~op:`Set s (level opt) with
      | Some r -> Some r
      | None when Posix.extra opt <> None -> errno `No_option
      | None -> (
          match s.os with Linux -> linux s opt v | Macos -> macos s opt v))

let set s opt v =
  match refusal s (Posix.canonical opt) v with
  | None -> Ok ()
  | Some r -> Error r

let get s opt =
  let gate = match s.os with Linux -> gate_linux | Macos -> gate_macos in
  match gate ~op:`Get s (level (Posix.canonical opt)) with
  | None -> Ok ()
  | Some r -> Error r

let exn op = function
  | Eio_check msg -> Invalid_argument msg
  | Missing -> Eio.Net.err Invalid_option
  | Errno e -> Posix.sockopt_error op e
