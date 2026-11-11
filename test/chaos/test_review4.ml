open Review4

let check w () = Eio_net_chaos.check w

let suite =
  ( "review4",
    [
      Alcotest.test_case "AF_UNIX lines read before a cut: correct" `Quick
        (Expect.passes (check unix_lines));
    ] )
