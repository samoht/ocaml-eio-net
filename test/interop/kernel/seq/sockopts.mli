(** The scenarios of socket options: each option's default, a set and the value
    read back, on every kind of socket; options a listener passes to the
    connections it accepts; options given to a connect; edge values; and what
    the buffer sizes change in the queues. *)

val unix_sndbuf : ?sndbuf:int -> ?rcvbuf:int -> int -> Common.net -> string
(** [unix_sndbuf ~sndbuf ~rcvbuf size net] sends Unix-domain datagrams of [size]
    bytes to 60 receivers in turn until a send waits, and says how many the
    sender's buffer took. *)

val udp_rmem :
  ?rcvbuf:int -> [ `Send of int | `Recv of int ] list -> Common.net -> string
(** [udp_rmem ~rcvbuf steps net] sends and receives UDP datagrams of 1000 bytes
    over loopback as [steps] say, and says how many the receiver then holds. *)

val scenarios : (string * (Common.net -> string)) list
(** [scenarios] is the socket options' scenarios, each by its name. *)
