(** Tests for the in-memory Eio network backend. *)

val suite : string * unit Alcotest.test_case list
(** [suite] covers stream round-trips and local host resolution. *)
