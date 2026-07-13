(** Tests for the addressed datagram channel. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks source retention, the configured default destination, the
    refusal of oversized datagrams, and datagram address equality. *)
