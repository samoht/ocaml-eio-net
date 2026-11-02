let send frame payload = frame.Eio_net.Frame.send [ Cstruct.of_string payload ]

let test_pair () =
  Eio_main.run @@ fun _env ->
  let a, b = Eio_net.Frame.pair () in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      send a "first";
      send a "second";
      send b "reply")
    (fun () ->
      let f1 = b.Eio_net.Frame.recv () in
      let f2 = b.Eio_net.Frame.recv () in
      let r = a.Eio_net.Frame.recv () in
      got := [ f1; f2; r ]);
  Alcotest.(check (list string))
    "frames arrive whole and in order"
    [ "first"; "second"; "reply" ]
    !got

let test_length_prefixed_roundtrip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow a in
  let receiver = Eio_net.Frame.of_length_prefixed_flow b in
  let payloads = [ ""; "one"; String.make 1000 'x'; "\x00\xff binary \x00" ] in
  let got = ref [] in
  Eio.Fiber.both
    (fun () -> List.iter (send sender) payloads)
    (fun () ->
      got := List.map (fun _ -> receiver.Eio_net.Frame.recv ()) payloads);
  Alcotest.(check (list string))
    "frames arrive whole and in order" payloads !got

let test_length_prefixed_eof () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow a in
  let receiver = Eio_net.Frame.of_length_prefixed_flow b in
  let got = ref None in
  Eio.Fiber.both
    (fun () ->
      send sender "last";
      Eio.Flow.close a)
    (fun () ->
      let first = receiver.Eio_net.Frame.recv () in
      let ended =
        match receiver.Eio_net.Frame.recv () with
        | _ -> false
        | exception End_of_file -> true
      in
      got := Some (first, ended));
  Alcotest.(check (option (pair string bool)))
    "one frame then end-of-stream"
    (Some ("last", true))
    !got

let test_length_prefixed_over_limit () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let receiver =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Framing.Config.v ~max_length:16 ())
      b
  in
  let raised = ref false in
  Eio.Fiber.both
    (fun () ->
      let hdr = Bytes.create 4 in
      Bytes.set_int32_be hdr 0 1000l;
      Eio.Flow.write a [ Cstruct.of_bytes hdr ])
    (fun () ->
      (try ignore (receiver.Eio_net.Frame.recv ())
       with Eio_net.Frame.Error _ -> raised := true);
      Eio.Flow.close a);
  Alcotest.(check bool) "over-limit frame refused" true !raised

let test_length_prefixed_batch () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow a in
  let receiver = Eio_net.Frame.of_length_prefixed_flow b in
  let payloads = [ "one"; ""; String.make 500 'z'; "\x00last\x00" ] in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      sender.Eio_net.Frame.send_batch
        (List.map (fun s -> [ Cstruct.of_string s ]) payloads))
    (fun () ->
      got := List.map (fun _ -> receiver.Eio_net.Frame.recv ()) payloads);
  Alcotest.(check (list string))
    "batched frames arrive whole and in order" payloads !got

let test_length_prefixed_16bit () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Framing.Config.v ~length_field:Uint16_be ())
      a
  in
  let receiver =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Framing.Config.v ~length_field:Uint16_be ())
      b
  in
  let got = ref "" in
  Eio.Fiber.both
    (fun () -> send sender "sixteen-bit framed")
    (fun () -> got := receiver.Eio_net.Frame.recv ());
  Alcotest.(check string)
    "16-bit-prefixed frame round-trips" "sixteen-bit framed" !got

let test_recv_slice () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow a in
  let receiver = Eio_net.Frame.of_length_prefixed_flow b in
  let got = ref ("", "") in
  Eio.Fiber.both
    (fun () -> send sender "HDRpayload-bytes")
    (fun () ->
      let s = receiver.Eio_net.Frame.recv_slice () in
      let hdr =
        Bytes.sub_string
          (Bytesrw.Bytes.Slice.bytes s)
          (Bytesrw.Bytes.Slice.first s)
          3
      in
      got := (hdr, Bytesrw.Bytes.Slice.to_string s));
  Alcotest.(check (pair string string))
    "header at offset, then whole frame"
    ("HDR", "HDRpayload-bytes")
    !got

let test_batch_refused_before_writing () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
  let channel =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Framing.Config.v ~max_length:16 ())
      writer
  in
  let refused =
    try
      channel.Eio_net.Frame.send_batch
        [
          [ Cstruct.of_string "valid" ];
          [ Cstruct.of_string (String.make 17 'x') ];
        ];
      false
    with Eio_net.Frame.Error _ -> true
  in
  Eio.Flow.shutdown writer `Send;
  let observed =
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:128 reader)
  in
  Alcotest.(check bool) "invalid batch is refused" true refused;
  Alcotest.(check string) "invalid batch emits no prefix or body" "" observed

let test_truncated_frames () =
  List.iter
    (fun (wire, expected) ->
      Eio_main.run @@ fun _ ->
      Eio.Switch.run @@ fun sw ->
      let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
      let frame = Eio_net.Frame.of_length_prefixed_flow reader in
      Eio.Flow.write writer [ Cstruct.of_string wire ];
      Eio.Flow.shutdown writer `Send;
      Alcotest.check_raises "exact partial EOF" (Eio_net.Frame.Error expected)
        (fun () -> ignore (frame.Eio_net.Frame.recv ())))
    [
      ( "\000\000",
        Framing.Truncated { part = Prefix; expected = 4; received = 2 } );
      ( "\000\000\000\003x",
        Framing.Truncated { part = Payload; expected = 3; received = 1 } );
    ]

module Fragmented_flow = struct
  type mode = Yield | Block of unit Eio.Promise.t * unit Eio.Promise.u
  type t = { base : Eio.Flow.two_way_ty Eio.Resource.t; mode : mode }

  let read_methods = []
  let single_read t buffer = Eio.Flow.single_read t.base buffer

  let rec single_write t = function
    | [] -> 0
    | first :: rest when Cstruct.length first = 0 -> single_write t rest
    | first :: _ ->
        let count = Eio.Flow.single_write t.base [ Cstruct.sub first 0 1 ] in
        (match t.mode with
        | Yield -> Eio.Fiber.yield ()
        | Block (blocked, notify) ->
            Eio.Promise.resolve notify ();
            Eio.Promise.await blocked);
        count

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t how = Eio.Flow.shutdown t.base how

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

  let wrap mode base =
    Eio.Resource.T
      ({ base :> Eio.Flow.two_way_ty Eio.Resource.t; mode }, handler)
end

let test_partial_write_cancelled () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
  let started, notify = Eio.Promise.create () in
  let blocked, _ = Eio.Promise.create () in
  let flow = Fragmented_flow.wrap (Block (blocked, notify)) writer in
  let channel = Eio_net.Frame.of_length_prefixed_flow flow in
  Eio.Fiber.first
    (fun () -> send channel "first")
    (fun () -> Eio.Promise.await started);
  Alcotest.check_raises "cancelled partial write cannot resume"
    (Eio_net.Frame.Error Framing.Aborted) (fun () -> send channel "second");
  Eio.Flow.shutdown writer `Send;
  let wire =
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:128 reader)
  in
  Alcotest.(check string)
    "only the first partial prefix reached peer" "\000" wire

let test_concurrent_fragmented_writers () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
  let sender =
    Eio_net.Frame.of_length_prefixed_flow (Fragmented_flow.wrap Yield writer)
  in
  let receiver = Eio_net.Frame.of_length_prefixed_flow reader in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      Eio.Fiber.both
        (fun () ->
          sender.Eio_net.Frame.send_batch
            (List.map (fun s -> [ Cstruct.of_string s ]) [ "a1"; "a2" ]))
        (fun () ->
          sender.Eio_net.Frame.send_batch
            (List.map (fun s -> [ Cstruct.of_string s ]) [ "b1"; "b2" ])))
    (fun () ->
      for _ = 1 to 4 do
        got := receiver.Eio_net.Frame.recv () :: !got
      done);
  let got = List.rev !got in
  Alcotest.(check bool)
    "whole batches survive byte-sized competing writes" true
    (got = [ "a1"; "a2"; "b1"; "b2" ] || got = [ "b1"; "b2"; "a1"; "a2" ])

let suite =
  ( "frame",
    [
      Alcotest.test_case "truncated frame EOF" `Quick test_truncated_frames;
      Alcotest.test_case "partial write cancellation" `Quick
        test_partial_write_cancelled;
      Alcotest.test_case "competing fragmented writers" `Quick
        test_concurrent_fragmented_writers;
      Alcotest.test_case "invalid batch emits no bytes" `Quick
        test_batch_refused_before_writing;
      Alcotest.test_case "in-memory pair round-trip" `Quick test_pair;
      Alcotest.test_case "recv_slice ingress view" `Quick test_recv_slice;
      Alcotest.test_case "length-prefixed round-trip" `Quick
        test_length_prefixed_roundtrip;
      Alcotest.test_case "length-prefixed batch (GSO)" `Quick
        test_length_prefixed_batch;
      Alcotest.test_case "length-prefixed 16-bit prefix" `Quick
        test_length_prefixed_16bit;
      Alcotest.test_case "length-prefixed end-of-stream" `Quick
        test_length_prefixed_eof;
      Alcotest.test_case "length-prefixed over-limit refused" `Quick
        test_length_prefixed_over_limit;
    ] )
