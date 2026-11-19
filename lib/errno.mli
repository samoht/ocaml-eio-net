(** The answers carried by the [Eio_unix.Unix_error] of a failed accept or
    connect, which every Eio backend raises for an error Eio does not name. *)

val accept : exn -> [> `Aborted | `Descriptors ] option
(** [accept exn] is the answer of the Unix error [exn] carries from a failed
    accept, as {!Eio_net.Accept.classify} reads it, and [None] if [exn] carries
    none. *)

val connect : exn -> [> `Unreachable ] option
(** [connect exn] is the answer of the Unix error [exn] carries from a failed
    connect, as {!Eio_net.Connect.classify} reads it, and [None] if [exn]
    carries none. *)
