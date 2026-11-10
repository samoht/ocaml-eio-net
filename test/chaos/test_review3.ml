open Review3

let check w () = Eio_net_chaos.check w

let suite =
  ( "review3",
    [
      Alcotest.test_case "banner exchange: correct" `Quick
        (Expect.passes (check banner));
      Alcotest.test_case "pipelined batch: correct" `Quick
        (Expect.passes (check pipelined));
    ] )
