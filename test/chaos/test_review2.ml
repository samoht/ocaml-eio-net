open Review2

let suite =
  ( "review2",
    [
      Alcotest.test_case "keep-alive: correct" `Quick
        (Expect.passes (fun () -> test_kv ()));
      Alcotest.test_case "keep-alive: correct, lenient server" `Quick
        (Expect.passes (fun () -> test_kv ~lenient:true ()));
      Alcotest.test_case "keep-alive: end of body taken, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (fun () -> test_kv ~bug:`Eof_body ()));
      Alcotest.test_case "keep-alive: no reconnect, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (fun () -> test_kv ~bug:`No_reconnect ()));
      Alcotest.test_case "keep-alive: retries reads only, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (fun () -> test_kv ~bug:`Read_only ()));
      Alcotest.test_case "keep-alive: retries resets only, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (fun () -> test_kv ~bug:`Reset_only ()));
      Alcotest.test_case "broker: correct" `Quick
        (Expect.passes (fun () -> test_pubsub ()));
      Alcotest.test_case "broker: head-of-line blocking, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (fun () -> test_pubsub ~bug:`Hol ()));
      Alcotest.test_case "broker: fan-out dies, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (fun () ->
             test_pubsub ~bug:`Fanout_dies ()));
      Alcotest.test_case "broker: single read, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (fun () ->
             test_pubsub ~bug:`Single_read ()));
      Alcotest.test_case "broker: correct, pinned buffers" `Quick
        (Expect.passes (fun () -> pinned ()));
      Alcotest.test_case "broker: head-of-line blocking, pinned buffers" `Quick
        (Expect.pinned (fun () -> pinned ~bug:`Hol ()));
    ] )
