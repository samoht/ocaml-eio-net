let () =
  Alcotest.run "eio-net.unix" [ Test_eio_net_unix.suite; Test_interface.suite ]
