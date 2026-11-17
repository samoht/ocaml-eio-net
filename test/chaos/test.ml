(* Tests of the chaos check against programs and their mutants: each program under
   test takes a bug, and each test says which tier of the check finds it. *)

let () =
  Alcotest.run "eio-net.chaos"
    [
      Test_clients.suite;
      Test_request_response.suite;
      Test_kv_pubsub.suite;
      Test_probe.suite;
      Test_protocols.suite;
      Test_af_unix.suite;
      Test_dead_peer.suite;
      Test_keepalive.suite;
      Test_guards.suite;
      Test_sampling.suite;
      Test_restart.suite;
    ]
