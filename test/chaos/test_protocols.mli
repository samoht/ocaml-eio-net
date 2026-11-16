(** Tests of eio-net.chaos against programs that real stacks run. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program, correct and with its bugs, and says which tier of
    the check catches each bug. *)
