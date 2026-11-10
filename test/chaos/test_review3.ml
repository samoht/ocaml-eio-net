open Review3

let check w () = Eio_net_chaos.check w

let suite =
  ( "review3",
    [
      Alcotest.test_case "banner exchange: correct" `Quick
        (Expect.passes (check banner));
      Alcotest.test_case "reconnecting client: correct" `Quick
        (Expect.passes (check (connfail ?bug:None)));
      Alcotest.test_case
        "reconnecting client: retries connection failures only, by the linear \
         tier"
        `Quick
        (Expect.caught ~tier:"linear"
           (check (connfail ~bug:`Connection_failure)));
      Alcotest.test_case "pipelined batch: correct" `Quick
        (Expect.passes (check pipelined));
    ] )
