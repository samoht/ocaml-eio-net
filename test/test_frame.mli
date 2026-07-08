(** Tests for the generic frame channel. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that an in-memory frame-channel pair preserves frame
    boundaries and ordering in both directions. *)
