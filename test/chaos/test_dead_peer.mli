(** Tests of eio-net.chaos against a broadcaster whose subscriber goes silent.
*)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program and says what the check does with it. *)
