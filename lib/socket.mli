(** A classifier of failed socket creations: it tells whether the system has no
    descriptor or memory left for a new socket, does not support it, or refuses
    it.

    Eio raises a backend's own error when a call such as
    {!Eio.Net.datagram_socket} cannot create its socket: eio_posix lets
    [socket(2)]'s [Unix.Unix_error] through as it is. A client that must outlive
    one failed creation, such as a resolver whose queries run beside other work,
    reads it with {!classify} rather than match a backend's exception.

    A library that creates sockets for its caller takes the classifier as an
    argument, {!classify} by default where it links eio-net, so linking a
    library never changes how another's failures are read. *)

type answer =
  [ `Descriptors
    (** The process or the system has no descriptor left: EMFILE or ENFILE. *)
  | `Memory
    (** The system has no buffer space or memory for the socket: ENOBUFS or
        ENOMEM. *)
  | `Unsupported
    (** The address family, protocol or socket type is not supported:
        EAFNOSUPPORT, EPROTONOSUPPORT or EPROTOTYPE, or EINVAL, which Linux
        returns for an unknown protocol or a family it does not have. *)
  | `Denied  (** The process may not create the socket: EACCES or EPERM. *) ]
(** The type for the answers from the system that refuse a new socket, as POSIX
    names the errors of [socket()]. *)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed socket creation
    carries from the system, and [None] if it carries none, as for a
    cancellation. It reads the system's error raw, as eio_posix raises
    [socket(2)]'s, or wrapped: [`Denied] for [Eio.Fs.Permission_denied _], as
    Eio names EACCES and EPERM, and the other answers from the
    [Eio_unix.Unix_error] every Eio backend raises for an error Eio does not
    name. *)
