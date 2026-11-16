(** Tests of eio-net.chaos against its probes. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program under test, correct and with its bugs, and says
    which tier of the check catches each bug. *)
