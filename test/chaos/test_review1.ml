open Review1

let serve ?bug ~clock l = Server.serve ?bug ~clock l

(* [`No_idle_timeout] is equivalent to the correct server: its handler reads
   with [App.Frame.read], which gives up after a second of silence on its
   own, so no fault tells the two apart. *)
let suite =
  ( "review1",
    [
      Alcotest.test_case "server: correct" `Quick
        (Expect.passes (test ~serve:(serve ?bug:None)));
      Alcotest.test_case "server: correct, pinned stall" `Quick
        (Expect.passes (test_idle ~serve:(serve ?bug:None)));
      Alcotest.test_case "server: sequential, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (test ~serve:(serve ~bug:`Sequential)));
      Alcotest.test_case "server: sequential, by the pinned stall" `Quick
        (Expect.pinned (test_idle ~serve:(serve ~bug:`Sequential)));
      Alcotest.test_case "server: raise on error, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test ~serve:(serve ~bug:`Raise_on_error)));
      Alcotest.test_case "server: raise on error, by the pinned stall" `Quick
        (Expect.pinned (test_idle ~serve:(serve ~bug:`Raise_on_error)));
      Alcotest.test_case "server: no idle timeout is equivalent" `Quick
        (Expect.passes (test ~serve:(serve ~bug:`No_idle_timeout)));
      Alcotest.test_case "server: no idle timeout is equivalent, pinned" `Quick
        (Expect.passes (test_idle ~serve:(serve ~bug:`No_idle_timeout)));
    ] )
