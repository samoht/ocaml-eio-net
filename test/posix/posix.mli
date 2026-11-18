(** eio_posix, where it is installed: everywhere but Windows. *)

val run : ((Eio_unix.Stdenv.base -> unit) -> unit) option
(** [run] is [Some Eio_posix.run] where eio_posix is installed, and [None]
    elsewhere. *)
