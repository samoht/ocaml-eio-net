open! Hermetic
open Kernel
open Code

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
