(** The Eio backend the recorder runs on: eio_windows on Windows, eio_posix
    elsewhere. *)

val name : string
(** [name] is the backend's opam package. *)

val run : (Eio_unix.Stdenv.base -> 'a) -> 'a
(** [run main] runs [main] in the backend's event loop. *)
