(** Tests for the fault wrappers of eio-net.chaos. *)

val suite : string * unit Alcotest.test_case list
(** [suite] shows each fault on flows, datagram sockets and networks: short,
    stalled, reset and ended reads, torn writes, faulted accepts and connects,
    lost, duplicated, reordered and garbled datagrams, what a cancelled receive
    or a close keeps, and the wrappers shared by several domains. *)
