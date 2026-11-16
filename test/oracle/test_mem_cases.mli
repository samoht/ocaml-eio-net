(** Socket cases beyond gVisor's tests, run against eio-net.mem and real
    sockets. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs each case. *)
