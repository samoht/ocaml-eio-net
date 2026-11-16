(** The kernel the network follows where Linux and macOS differ. *)

(** A kernel. *)
type os = Linux | Macos

val of_arg : [< `Linux | `Macos ] option -> os
(** [of_arg os] is the kernel [?os] names, Linux by default. *)
