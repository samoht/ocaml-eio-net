(** Length-prefixed framing cases. *)

val suite : string * unit Alcotest.test_case list
(** Exact bytes, fragmentation, end of input and hostile lengths. *)
