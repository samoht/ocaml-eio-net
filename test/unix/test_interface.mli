(** Tests for the host's interface addresses. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks the listed addresses against the loopback's known ones and
    against bind(2). *)
