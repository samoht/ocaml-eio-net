(** Tests of eio-net.chaos against review 4's programs. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program and says what the check does with it. *)
