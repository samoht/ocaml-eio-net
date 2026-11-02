let run count size =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow writer in
  let receiver = Eio_net.Frame.of_length_prefixed_flow reader in
  let payload = String.make size 'x' in
  let frame = [ Cstruct.of_string payload ] in
  let allocated = Gc.allocated_bytes () in
  let clock = Mtime_clock.counter () in
  Eio.Fiber.both
    (fun () ->
      for _ = 1 to count do
        sender.Eio_net.Frame.send frame
      done)
    (fun () ->
      for _ = 1 to count do
        if not (String.equal payload (receiver.Eio_net.Frame.recv ())) then
          failwith "framing measurement changed or lost the payload"
      done);
  let elapsed =
    Mtime.Span.to_float_ns (Mtime_clock.count clock) /. float_of_int count
  in
  let allocated = (Gc.allocated_bytes () -. allocated) /. float_of_int count in
  Fmt.pr "%d frames/%d bytes: %.0f ns and %.2f allocated bytes/frame@." count
    size elapsed allocated

let () =
  List.iter (fun count -> List.iter (run count) [ 100; 1000 ]) [ 100; 1000 ]
