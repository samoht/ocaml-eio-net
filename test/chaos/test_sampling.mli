(** Tests of how the sampled tier of eio-net.chaos picks its scenarios. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs a workload whose bug only the sampled tier reaches, from
    candidates in a periodic order. *)
