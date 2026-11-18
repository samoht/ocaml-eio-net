(** The kernel the network follows where Linux, macOS and Windows differ. *)

(** A kernel. *)
type os = Linux | Macos | Windows

val of_arg : [< `Linux | `Macos | `Windows ] option -> os
(** [of_arg os] is the kernel [?os] names, Linux by default. *)
