(** Whether a kernel takes a socket option for a socket, and the error it gives
    otherwise: the value's type and range, the state of the socket in which the
    option is allowed, and the errno each kernel returns, as eio_posix raises
    it. A pure function of the kernel, the socket and the value, apart from what
    setting the option does ({!Sockopt}). *)

(** A socket's address family. *)
type family = V4 | V6 | Local

(** What a socket is. *)
type kind =
  | Unconnected  (** A stream socket that neither listens nor is connected. *)
  | Listener  (** A listener. *)
  | Client  (** The end of a connection that connected. *)
  | Server  (** The end of a connection a listener accepted. *)
  | Datagram  (** A datagram socket. *)

type socket = {
  os : Kernel.os;
  family : family;
  kind : kind;
  maxseg : int;  (** The maximum segment size macOS reads back now. *)
}
(** What the kernel looks at when it validates an option. *)

type errno =
  [ `No_option
  | `Not_supported
  | `Invalid
  | `Denied
  | `Unknown
  | `Not_connected
  | `Domain ]
(** The errors setsockopt(2) and getsockopt(2) fail with: [ENOPROTOOPT],
    [EOPNOTSUPP], [EINVAL], [EACCES], [ENOENT], [ENOTCONN] and [EDOM]. *)

(** Why an option is refused. *)
type refusal =
  | Eio_check of string
      (** Eio's own check refuses the value before any system call, with
          [Invalid_argument] and this message. *)
  | Missing  (** eio_posix has no such option on the kernel. *)
  | Errno of errno  (** The kernel refuses it with this error. *)

val connected : kind -> bool
(** [connected k] is [true] for an end of a connection. *)

val default_maxseg : family -> kind -> int
(** [default_maxseg family kind] is macOS's maximum segment size before a
    connection, and that of each end of one over loopback, whose MTU is 16384.
*)

val socket_level : 'a Eio.Net.Sockopt.t -> bool
(** [socket_level opt] is [true] for an option of the socket level,
    [SOL_SOCKET], which every socket takes. *)

val congestion : string -> string
(** [congestion name] is the congestion control algorithm Linux reads in [name]:
    up to its first NUL and 15 bytes. *)

val set : socket -> 'a Eio.Net.Sockopt.t -> 'a -> (unit, refusal) result
(** [set s opt v] is [Ok ()] if the kernel of [s] takes [v] for [opt] on [s],
    and why not otherwise. Eio's checks come first, then the level of the option
    against the socket's family and type, then the value. *)

val get : socket -> 'a Eio.Net.Sockopt.t -> (unit, refusal) result
(** [get s opt] is [Ok ()] if the kernel of [s] reads [opt] at its level on [s],
    and why not otherwise. A get may fail later for a reason of its own, such as
    [IP_MTU] on a socket that is not connected. *)

val exn : [ `Get | `Set ] -> refusal -> exn
(** [exn op r] is the exception eio_posix raises for [r] from [op]. *)
