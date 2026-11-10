open After
open App

let read ~clock f = Frame.read ~clock f
let fetch ~clock n a = Client.fetch ~clock n a
let call ~clock s d ~id b = Rpc.call ~clock s d ~id b

(* A reader that takes any end of stream for a clean one returns a prefix of
   the messages, which [test_d] accepts: telling the two ends apart needs the
   exact cut, so the pinned [test_cut] catches it. *)
let suite =
  ( "after",
    [
      Alcotest.test_case "frames: correct" `Quick (Expect.passes (test_d ~read));
      Alcotest.test_case "frames: correct, pinned cut" `Quick
        (Expect.passes (test_cut ~read));
      Alcotest.test_case "frames: a single read, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_d ~read:(Frame.read ~bug:`Single_read)));
      Alcotest.test_case "frames: no timeout, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_d ~read:(Frame.read ~bug:`No_timeout)));
      Alcotest.test_case "frames: any end is clean, passes the check" `Quick
        (Expect.passes (test_d ~read:(Frame.read ~bug:`Eof_is_clean)));
      Alcotest.test_case "frames: any end is clean, by the pinned cut" `Quick
        (Expect.pinned (test_cut ~read:(Frame.read ~bug:`Eof_is_clean)));
      Alcotest.test_case "retries: correct" `Quick
        (Expect.passes (test_e ~fetch));
      Alcotest.test_case "retries: no timeout, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_e ~fetch:(Client.fetch ~bug:`No_timeout)));
      Alcotest.test_case "retries: refused only, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_e ~fetch:(Client.fetch ~bug:`Refused_only)));
      Alcotest.test_case "datagrams: correct" `Quick
        (Expect.passes (test_f ~call));
      Alcotest.test_case "datagrams: any reply, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (test_f ~call:(Rpc.call ~bug:`Any_reply)));
    ] )
