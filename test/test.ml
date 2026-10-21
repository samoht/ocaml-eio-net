let () =
  Alcotest.run "eio-net"
    [
      Test_eio_net_mem.suite;
      Test_frame.suite;
      Test_datagram.suite;
      Test_eio_net_chaos.suite;
      Test_relay.suite;
      Test_progress.suite;
    ]
