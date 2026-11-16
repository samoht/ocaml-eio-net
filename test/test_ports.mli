(** Tests of the ports and addresses of the in-memory Eio network backend. *)

val suite : string * unit Alcotest.test_case list
(** [suite] covers addresses in use, TIME_WAIT and FIN_WAIT2, the addresses no
    interface has, source addresses and the choice of ephemeral ports. *)
