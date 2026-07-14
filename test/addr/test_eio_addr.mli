(** Cases for the {!Eio_addr} bridge: the crossing between Ipaddr addresses and
    the raw octets Eio names them by. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the named group of test cases, for {!Alcotest.run}. *)
