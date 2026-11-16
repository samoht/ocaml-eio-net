(** Socket options, as the chosen kernel answers them on a host with loopback
    only: what a socket reads back, defaults included, with the kernel's
    rounding and limits, and the error eio_posix raises where the kernel refuses
    an option. The send and receive buffers size a connection's queues, and the
    keepalive options and [TCP_USER_TIMEOUT] say when an end of a silent
    connection gives up. *)

(** A socket's address family. *)
type family = V4 | V6 | Local

(** What a socket is. *)
type kind =
  | Unconnected  (** A stream socket that neither listens nor is connected. *)
  | Listener  (** A listener. *)
  | Client  (** The end of a connection that connected. *)
  | Server  (** The end of a connection a listener accepted. *)
  | Datagram  (** A datagram socket. *)

type t
(** A socket's options. *)

val v : os:Kernel.os -> family:family -> kind -> t
(** [v ~os ~family kind] has the kernel's defaults. *)

val listener : os:Kernel.os -> Eio.Net.Sockaddr.stream -> t
(** [listener ~os addr] is those of a listener bound at [addr]. *)

val client : os:Kernel.os -> Eio.Net.Sockaddr.stream -> t
(** [client ~os dst] is those of a socket that connects to [dst]. *)

val datagram : os:Kernel.os -> Eio.Net.Sockaddr.datagram -> t
(** [datagram ~os addr] is those of a datagram socket bound at [addr]. *)

val bound : t -> Eio.Net.Ipaddr.v4v6 -> unit
(** [bound t ip] says the socket binds [ip]: Linux makes an IPv6 socket bound at
    an address other than the wildcard and the IPv4-mapped ones IPv6 only. *)

val connect : t -> kind -> unit
(** [connect t kind] makes the socket the end [kind] of a connection. *)

val accepted : t -> t
(** [accepted l] is those of a socket the listener of options [l] accepts: those
    the kernel passes on, the defaults otherwise. *)

val set : t -> 'a Eio.Net.Sockopt.t -> 'a -> unit
(** [set t opt v] sets [opt] as the kernel does, or raises the error eio_posix
    raises. *)

val get : t -> 'a Eio.Net.Sockopt.t -> 'a
(** [get t opt] is what the kernel reads back, or raises the error eio_posix
    raises. *)

val sndbuf : t -> int
(** [sndbuf t] is the send buffer [SO_SNDBUF] reads back. *)

val rcvbuf : t -> int
(** [rcvbuf t] is the receive buffer [SO_RCVBUF] reads back. *)

val linger : t -> int option
(** [linger t] is the [SO_LINGER] time. *)

val on_resize : t -> (unit -> unit) -> unit
(** [on_resize t f] calls [f] whenever a buffer is set. *)

val queue : local:bool -> writer:t -> reader:t -> int option
(** [queue ~local ~writer ~reader] is what a direction of a connection from
    [writer] to [reader] queues once a buffer is set, over AF_UNIX if [local],
    or [None] when neither is. *)

val give_up :
  t ->
  silenced:Mtime.t ->
  last:Mtime.t ->
  oldest:Mtime.t option ->
  Mtime.t option
(** [give_up t ~silenced ~last ~oldest] is when a TCP end whose connection went
    silent at [silenced] gives up, if it does: [last] is when it last received a
    segment, and [oldest] when it sent the oldest byte not acknowledged. *)
