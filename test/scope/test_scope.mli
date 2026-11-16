(** The scope table of eio-net.mem against the network. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that every option has a row and that each option's class
    holds under each kernel. *)
