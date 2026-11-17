(** How eio-net.mem's silent TCP connections fail against the kernels' measured
    ones. *)

val suite : string * unit Alcotest.test_case list
(** One test per kernel and per case of traces/silent.csv. *)
