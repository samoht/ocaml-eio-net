(** The validation of socket options against the kernel's traces. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks each setsockopt the traces record against the validation of
    each kernel. *)
