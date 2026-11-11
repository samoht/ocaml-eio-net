let () =
  Alcotest.run "eio-net.chaos"
    [
      Test_after.suite;
      Test_review1.suite;
      Test_review2.suite;
      Test_probe.suite;
      Test_review3.suite;
      Test_review4.suite;
      Test_guards.suite;
    ]
