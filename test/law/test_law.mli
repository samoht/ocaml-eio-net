(** The wrapper law, checked against what each operation does. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that every operation has a row of the law, that each row's
    class holds of a wrapper over eio-net.mem, that the faults an intercepted
    row lists, and only those, change what it does, and that a fault leaves no
    socket open once its run closed those it holds. *)
