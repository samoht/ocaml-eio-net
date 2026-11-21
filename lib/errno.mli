(** The answers carried by the [Eio_unix.Unix_error] of a failed accept, connect
    or socket creation, which every Eio backend raises for an error Eio does not
    name, and by the raw [Unix.Unix_error] of a socket creation. *)

val accept : exn -> [> `Aborted | `Descriptors ] option
(** [accept exn] is the answer of the Unix error [exn] carries from a failed
    accept, as {!Eio_net.Accept.classify} reads it, and [None] if [exn] carries
    none. *)

val connect : exn -> [> `Unreachable ] option
(** [connect exn] is the answer of the Unix error [exn] carries from a failed
    connect, as {!Eio_net.Connect.classify} reads it, and [None] if [exn]
    carries none. *)

val socket : exn -> [> `Descriptors | `Memory | `Unsupported | `Denied ] option
(** [socket exn] is the answer of the Unix error [exn] carries from a failed
    socket creation, raw as eio_posix raises [socket(2)]'s or wrapped, as
    {!Eio_net.Socket.classify} reads it, and [None] if [exn] carries none. *)
