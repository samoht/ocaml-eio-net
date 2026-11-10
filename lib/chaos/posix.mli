(** The errors eio_posix raises for failed connects and accepts. *)

val timeout : unit -> exn
(** [timeout ()] is the error of a connect whose handshake timed out. *)

val unreachable : unit -> exn
(** [unreachable ()] is the error of a connect to a host with no route, which
    the kernel refuses at once. *)

val emfile : unit -> exn
(** [emfile ()] is the error of an accept with no descriptor left. *)
