(* A frame sent on one end of a pair arrives whole on the other, in both
   directions, preserving frame boundaries (each recv is exactly one send). *)
let test_pair () =
  Eio_main.run @@ fun _env ->
  let a, b = Eio_net.Frame.pair () in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      a.Eio_net.Frame.send "first";
      a.Eio_net.Frame.send "second";
      b.Eio_net.Frame.send "reply")
    (fun () ->
      let f1 = b.Eio_net.Frame.recv () in
      let f2 = b.Eio_net.Frame.recv () in
      let r = a.Eio_net.Frame.recv () in
      got := [ f1; f2; r ]);
  Alcotest.(check (list string))
    "frames arrive whole and in order"
    [ "first"; "second"; "reply" ] !got

let suite =
  ("frame", [ Alcotest.test_case "in-memory pair round-trip" `Quick test_pair ])
