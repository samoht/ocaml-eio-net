(** Tests of the datagram sockets of the in-memory Eio network backend. *)

val suite : string * unit Alcotest.test_case list
(** [suite] covers datagram delivery, order and loss, addresses in use and
    autobinding, the order of a send's errors and the receive queues. *)
