(** Seeded echo programs whose reads and writes meet
    {!Eio_net_chaos.Fault.Cancel}, on the host's network and on eio-net.mem. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs the seeds that [CANCEL_SEEDS] counts (defaults to 16), or the
    one [CANCEL_SEED] names, on each network, and fails with the seeds that
    lost, duplicated or garbled a byte, breached the cancel's contract, raised
    or did not end within [CANCEL_TIMEOUT] seconds (defaults to 60). *)
