let send frame rope = frame.Eio_net.Frame.send (Rope.of_string rope)

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
  let receiver = Eio_net.Frame.of_length_prefixed_flow ~max_frame:16 b in
  let raised = ref false in
  Eio.Fiber.both
    (fun () ->
      let hdr = Bytes.create 4 in
      Bytes.set_int32_be hdr 0 1000l;
      Eio.Flow.write a [ Cstruct.of_bytes hdr ])
    (fun () ->
      (try ignore (receiver.Eio_net.Frame.recv ())
       with Invalid_argument _ -> raised := true);
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
      sender.Eio_net.Frame.send_batch (List.map Rope.of_string payloads))
    (fun () ->
      got := List.map (fun _ -> receiver.Eio_net.Frame.recv ()) payloads);
  Alcotest.(check (list string))
    "batched frames arrive whole and in order" payloads !got

let test_length_prefixed_16bit () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender = Eio_net.Frame.of_length_prefixed_flow ~prefix_bytes:2 a in
  let receiver = Eio_net.Frame.of_length_prefixed_flow ~prefix_bytes:2 b in
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
  let channel = Eio_net.Frame.of_length_prefixed_flow ~max_frame:16 writer in
  let refused =
    try
      channel.Eio_net.Frame.send_batch
        [ Rope.of_string "valid"; Rope.of_string (String.make 17 'x') ];
      false
    with Invalid_argument _ -> true
  in
  Eio.Flow.shutdown writer `Send;
  let observed =
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:128 reader)
  in
  Alcotest.(check bool) "invalid batch is refused" true refused;
  Alcotest.(check string) "invalid batch emits no prefix or body" "" observed

let suite =
  ( "frame",
    [
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
