(** Tests of the guards of [Eio_net_chaos.check]. *)

val suite : string * unit Alcotest.test_case list
(** [suite] covers the hang and time limit reports, the budget, determinism,
    replay and the refusals of a stale or unknown replay and of a workload with
    no operation, a nondeterministic one or one that fails alone. *)
