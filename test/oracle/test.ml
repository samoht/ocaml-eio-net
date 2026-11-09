let () =
  Alcotest.run "eio-net.oracle"
    [
      Test_gvisor.suite;
      Test_gvisor.unix_suite;
      Test_gvisor.udp_suite;
      Test_gvisor.tcp_suite;
      Test_review.suite;
    ]
