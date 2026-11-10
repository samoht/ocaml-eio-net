(** eio-net.mem replays the kernel's traces. *)

val suite : string * unit Alcotest.test_case list
(** One test per transport, and one that every listed exception is needed. *)
