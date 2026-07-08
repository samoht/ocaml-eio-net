let () =
  Alcotest.run "eio-net" [ Test_eio_net_mem.suite; Test_frame.suite ]
