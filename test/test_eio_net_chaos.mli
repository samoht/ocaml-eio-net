(** Tests for the adversarial/chaos network layer. *)

val suite : string * unit Alcotest.test_case list
(** [suite] shows each fault kind injecting deterministically: dribbled,
    coalesced, stalled, reset, and ended flow reads; torn writes; dropped,
    duplicated, reordered, truncated, and garbled datagrams; and adversarial
    connects and accepts. *)
