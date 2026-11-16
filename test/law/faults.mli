(** The faults the law's tests inject. *)

val all : Fault.t list
(** [all] is every fault, each constructor once, with the arguments the law's
    rows use. *)
