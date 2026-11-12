(** The errors eio_posix raises for failed connects and accepts. *)

val timeout : unit -> exn
(** [timeout ()] is the error of a connect whose handshake timed out. *)

val unreachable : unit -> exn
(** [unreachable ()] is the error of a connect to a host with no route, which
    the kernel refuses at once. *)

val emfile : unit -> exn
(** [emfile ()] is the error of an accept with no descriptor left. *)

val systhread : 'a Effect.t -> (unit -> 'a) option
(** [systhread e] is [Some run] iff [e] is the effect of
    [Eio_unix.run_in_systhread]: [run ()] calls its function in the calling
    thread and is the effect's answer. *)
