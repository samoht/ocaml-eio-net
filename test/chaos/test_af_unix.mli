(** Tests of eio-net.chaos against programs on AF_UNIX. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program and says what the check does with it. *)
