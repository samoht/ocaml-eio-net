(** The wrapper law, checked against what each operation does. *)

val faults : Fault.t list
(** [faults] is every fault, each constructor once, with the arguments the law's
    rows use. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that every operation has a row of the law, that each row's
    class holds of a wrapper over eio-net.mem, that the faults an intercepted
    row lists, and only those, change what it does, and that a fault leaves no
    socket open once its run closed those it holds. *)
