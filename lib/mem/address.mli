(** IP addresses and Unix paths on a host with the loopback interface only: how
    a destination is reached, which addresses a socket binds, and the errors
    each kernel gives for those it refuses. *)

open Kernel

val unmap : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6
(** [unmap ip] is the IPv4 address an IPv4-mapped [ip] stands for, or [ip]. *)

val is_v4 : Eio.Net.Ipaddr.v4v6 -> bool
(** [is_v4 ip] is [true] if [ip] is IPv4 or IPv4-mapped. *)

val any : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6
(** [any ip] is the wildcard address of [ip]'s family. *)

val map : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6
(** [map ip] is the IPv4-mapped form of the IPv4 address [ip]. *)

val same : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6 -> bool
(** [same x y] is [true] if [x] and [y] are the same address in the same form.
*)

val reaching : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6 list
(** [reaching ip] is the addresses, best first, at which a socket takes a
    connection or a datagram sent to [ip]: [ip] itself, in either form if it is
    IPv4, then the wildcard address of its family, and for IPv4 the IPv6
    wildcard address of a dual-stack socket. A wildcard destination stands for
    the loopback address of its family, as both kernels route it. *)

val source_to : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6
(** [source_to ip] is the address a socket that binds none sends from to [ip]:
    the loopback address of its family. *)

val toward :
  os:os ->
  bound:Eio.Net.Ipaddr.v4v6 ->
  Eio.Net.Ipaddr.v4v6 ->
  Eio.Net.Ipaddr.v4v6
(** [toward ~os ~bound dst] is the destination a socket bound at [bound] reaches
    for [dst], as Linux routes a wildcard one: the IPv4 wildcard address stands
    for the IPv4 address the socket is bound at, in [dst]'s form, and the IPv6
    one from an IPv4-mapped address for ::ffff:127.0.0.1. *)

val seen_by : Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6 -> Eio.Net.Ipaddr.v4v6
(** [seen_by own ip] is [ip] as a socket bound at [own] sees it: IPv4 to an IPv4
    socket, IPv4-mapped to an IPv6 one. *)

val ip_overlaps : Eio.Net.Ipaddr.v4v6 * int -> Eio.Net.Ipaddr.v4v6 * int -> bool
(** [ip_overlaps a b] is [true] if [a] and [b] are of one port and their
    addresses are equal, one is the wildcard address of the other's family, or
    one is the IPv6 wildcard address, which a dual-stack socket holds at IPv4
    addresses too, as bind(2) has it on Linux. *)

val first_byte : Eio.Net.Ipaddr.v4v6 -> int
(** [first_byte ip] is the first byte of [ip]'s raw form. *)

val multicast_v4 : Eio.Net.Ipaddr.v4v6 -> bool
(** [multicast_v4 ip] is [true] for an address of 224.0.0.0/4. *)

val broadcast : Eio.Net.Ipaddr.v4v6 -> bool
(** [broadcast ip] is [true] for 255.255.255.255. *)

val lo_broadcast : Eio.Net.Ipaddr.v4v6 -> bool
(** [lo_broadcast ip] is [true] for 127.255.255.255. *)

val needs_scope : Eio.Net.Ipaddr.v4v6 -> bool
(** [needs_scope ip] is [true] for a link-local IPv6 address, or a multicast one
    of interface or link scope, which Eio's addresses cannot scope. *)

val off_loopback : Eio.Net.Ipaddr.v4v6 -> bool
(** [off_loopback ip] is [true] for an address the loopback interface does not
    reach: any but 127.0.0.0/8, [::1], the wildcard addresses, IPv4 multicast
    and broadcast, in either form. *)

val unanswered : Eio.Net.Ipaddr.v4v6 -> bool
(** [unanswered ip] is [true] for an address of 127.0.0.0/8 other than 127.0.0.1
    and the broadcast one, which macOS's loopback does not take. *)

val bind_fail : Eio.Exn.Backend.t -> 'a
(** [bind_fail code] raises the raw [Unix.Unix_error] eio_posix raises from
    [listen] and [datagram_socket] for a bind that fails with [code]. *)

val check_local :
  os:os ->
  ?tcp:bool ->
  ?fail:(Eio.Exn.Backend.t -> unit) ->
  Eio.Net.Ipaddr.v4v6 ->
  unit
(** [check_local ~os ~tcp ~fail ip] calls [fail] with the code of the error a
    socket meets binding [ip], a TCP one if [tcp], if the loopback interface
    does not have it. [fail] defaults to {!Code.fail}. *)

val embedded_nul : os:os -> string -> bool
(** [embedded_nul ~os path] is [true] for a path with a NUL past its first byte,
    or under macOS a NUL anywhere. *)

val check_path : os:os -> ?raw:bool -> string -> unit
(** [check_path ~os ~raw path] raises the error a socket meets at [path] if
    [sun_path] cannot hold it, or it names no file: raw from a bind if [raw], an
    {!Eio.Io} error otherwise. *)

val cross_family :
  os:os ->
  Eio.Net.Ipaddr.v4v6 ->
  Eio.Net.Ipaddr.v4v6 ->
  Eio.Exn.Backend.t option
(** [cross_family ~os src dst] is the error a socket at [src] meets reaching
    [dst] across IPv6 and IPv4-mapped addresses, if any. *)
