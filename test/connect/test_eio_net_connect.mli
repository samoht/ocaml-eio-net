(** Cases for {!Eio_net_connect}: the portable answer, the registered
    classifier, and the refusal of a second registration. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the named group of test cases, for {!Alcotest.run}. *)
