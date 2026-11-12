open Dead_peer

(* macOS queues 1 MiB over TCP, so a broadcast of 1.25 MiB fills it. *)
let check w () = Eio_net_chaos.check ~os:`Macos w

let suite =
  ( "dead peer",
    [
      Alcotest.test_case "broadcast: correct" `Quick
        (Expect.passes (check broadcast));
      Alcotest.test_case
        "broadcast: blocked on a silent subscriber, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (check (broadcast ~bug:`Sync)));
    ] )
