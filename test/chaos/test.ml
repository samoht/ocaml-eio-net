(* The tests of the chaos API's design, and its mutants: each program under
   test takes a bug, and each test says which tier of the check finds it. *)

let () =
  Alcotest.run "eio-net.chaos"
    [
      Test_after.suite;
      Test_review1.suite;
      Test_review2.suite;
      Test_probe.suite;
      Test_review3.suite;
      Test_review4.suite;
      Test_dead_peer.suite;
      Test_keepalive.suite;
      Test_guards.suite;
      Test_sampling.suite;
    ]
