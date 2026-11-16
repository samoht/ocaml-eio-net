(** What a check relies on in [Quiet_mock], the silent copy of Eio_mock's
    scheduler and clock that eio-fs and eio-net both carry. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks the silent clock, 1 us per turn, the time limit, deadlock,
    stop and idle, [max_turns] and its two verdicts, [charge], effects a hook
    answers at once, and that the clock never moves back. *)
