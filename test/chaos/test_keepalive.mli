(** Tests of eio-net.chaos against a client that counts on TCP to notice a
    server gone silent. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each program and says what the check does with it. *)
