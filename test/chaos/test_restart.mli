(** Tests of the restart that follows each run with a fault. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program and says what the check does with it. *)
