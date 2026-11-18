(* Each measurement prints the time and the allocated bytes per frame. The
   socket pair measures a whole send and receive through the kernel; the
   in-memory flows isolate the framing from it: a receive reads frames already
   encoded in a string, a send writes into a sink that discards its input. *)

module Memory_flow = struct
  type t = { source : Eio.Flow.source_ty Eio.Resource.t }

  let read_methods = []
  let single_read t buffer = Eio.Flow.single_read t.source buffer
  let single_write _ buffers = Cstruct.lenv buffers
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _ = ()

  let handler =
    Eio.Flow.Pi.two_way
      (module struct
        type nonrec t = t

        let read_methods = read_methods
        let single_read = single_read
        let single_write = single_write
        let copy = copy
        let shutdown = shutdown
      end)

  let v wire =
    let source =
      (Eio.Flow.string_source wire :> Eio.Flow.source_ty Eio.Resource.t)
    in
    Eio.Resource.T ({ source }, handler)
end

let report name count size clock allocated =
  let elapsed =
    Mtime.Span.to_float_ns (Mtime_clock.count clock) /. float_of_int count
  in
  let allocated = (Gc.allocated_bytes () -. allocated) /. float_of_int count in
  Fmt.pr "%-6s %d frames/%d bytes: %.0f ns and %.2f allocated bytes/frame@."
    name count size elapsed allocated

let socket count size =
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
        Eio_net.Frame.send sender frame
      done)
    (fun () ->
      for _ = 1 to count do
        if not (String.equal payload (Eio_net.Frame.recv receiver)) then
          failwith "framing measurement changed or lost the payload"
      done);
  report "socket" count size clock allocated

let recv count size =
  Eio_main.run @@ fun _ ->
  let payload = String.make size 'x' in
  let prefix = Bytes.create 4 in
  Bytes.set_int32_be prefix 0 (Int32.of_int size);
  let wire =
    String.concat ""
      (List.init count (fun _ -> Bytes.to_string prefix ^ payload))
  in
  let receiver = Eio_net.Frame.of_length_prefixed_flow (Memory_flow.v wire) in
  let allocated = Gc.allocated_bytes () in
  let clock = Mtime_clock.counter () in
  for _ = 1 to count do
    if String.length (Eio_net.Frame.recv receiver) <> size then
      failwith "framing measurement changed or lost the payload"
  done;
  report "recv" count size clock allocated

let send count size =
  Eio_main.run @@ fun _ ->
  let sender = Eio_net.Frame.of_length_prefixed_flow (Memory_flow.v "") in
  let frame = [ Cstruct.of_string (String.make size 'x') ] in
  let allocated = Gc.allocated_bytes () in
  let clock = Mtime_clock.counter () in
  for _ = 1 to count do
    Eio_net.Frame.send sender frame
  done;
  report "send" count size clock allocated

let () =
  List.iter
    (fun measure ->
      List.iter
        (fun count -> List.iter (measure count) [ 100; 1000 ])
        [ 1000; 10_000 ])
    [ socket; recv; send ]
