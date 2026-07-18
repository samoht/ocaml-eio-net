(** Tests for Unix-specific Eio network transports and socket helpers. *)

val suite : string * unit Alcotest.test_case list
(** [suite] exercises connected datagram frames, batching, and retry behavior.
*)
