let send frame payload = Eio_net.Frame.send frame [ Cstruct.of_string payload ]

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
      let f1 = Eio_net.Frame.recv b in
      let f2 = Eio_net.Frame.recv b in
      let r = Eio_net.Frame.recv a in
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
    (fun () -> got := List.map (fun _ -> Eio_net.Frame.recv receiver) payloads);
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
      let first = Eio_net.Frame.recv receiver in
      let ended =
        match Eio_net.Frame.recv receiver with
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
      ~framing:(Eio_net_framing.Config.v ~max_length:16 ())
      b
  in
  let raised = ref false in
  Eio.Fiber.both
    (fun () ->
      let hdr = Bytes.create 4 in
      Bytes.set_int32_be hdr 0 1000l;
      Eio.Flow.write a [ Cstruct.of_bytes hdr ])
    (fun () ->
      (try ignore (Eio_net.Frame.recv receiver)
       with Eio_net_framing.Error _ -> raised := true);
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
      Eio_net.Frame.send_batch sender
        (List.map (fun s -> [ Cstruct.of_string s ]) payloads))
    (fun () -> got := List.map (fun _ -> Eio_net.Frame.recv receiver) payloads);
  Alcotest.(check (list string))
    "batched frames arrive whole and in order" payloads !got

let test_length_prefixed_16bit () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let sender =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Eio_net_framing.Config.v ~length_field:Uint16_be ())
      a
  in
  let receiver =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Eio_net_framing.Config.v ~length_field:Uint16_be ())
      b
  in
  let got = ref "" in
  Eio.Fiber.both
    (fun () -> send sender "sixteen-bit framed")
    (fun () -> got := Eio_net.Frame.recv receiver);
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
      let s = Eio_net.Frame.recv_slice receiver in
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

(* A caller's channel gets one-frame batches and a slice it may write
   through without changing the string its [recv] returned. *)
let test_v_defaults () =
  let held = "frame" in
  let sent = ref [] in
  let channel =
    Eio_net.Frame.v
      ~recv:(fun () -> held)
      ~send:(fun bufs -> sent := Cstruct.copyv bufs :: !sent)
      ()
  in
  Alcotest.(check (list string))
    "recv_batch is one frame" [ "frame" ]
    (Eio_net.Frame.recv_batch channel);
  let slice = Eio_net.Frame.recv_slice channel in
  Bytes.set
    (Bytesrw.Bytes.Slice.bytes slice)
    (Bytesrw.Bytes.Slice.first slice)
    'F';
  Alcotest.(check string) "the slice is a copy" "frame" held;
  Eio_net.Frame.send_batch channel
    [ [ Cstruct.of_string "a" ]; [ Cstruct.of_string "b" ] ];
  Alcotest.(check (list string))
    "send_batch sends one frame at a time" [ "a"; "b" ] (List.rev !sent)

let test_v_overrides () =
  let batches = ref 0 in
  let channel =
    Eio_net.Frame.v
      ~recv_batch:(fun () -> [ "x"; "y" ])
      ~recv_slice:(fun () -> Bytesrw.Bytes.Slice.of_string "slice")
      ~send_batch:(fun _ -> incr batches)
      ~recv:(fun () -> "frame")
      ~send:(fun _ -> Alcotest.fail "send called for a batch")
      ()
  in
  Alcotest.(check (list string))
    "recv_batch is the caller's" [ "x"; "y" ]
    (Eio_net.Frame.recv_batch channel);
  Alcotest.(check string)
    "recv_slice is the caller's" "slice"
    (Bytesrw.Bytes.Slice.to_string (Eio_net.Frame.recv_slice channel));
  Eio_net.Frame.send_batch channel [ [ Cstruct.of_string "a" ] ];
  Alcotest.(check int) "send_batch is the caller's" 1 !batches

let test_batch_refused_before_writing () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let writer, reader = Eio_unix.Net.socketpair_stream ~sw () in
  let channel =
    Eio_net.Frame.of_length_prefixed_flow
      ~framing:(Eio_net_framing.Config.v ~max_length:16 ())
      writer
  in
  let refused =
    try
      Eio_net.Frame.send_batch channel
        [
          [ Cstruct.of_string "valid" ];
          [ Cstruct.of_string (String.make 17 'x') ];
        ];
      false
    with Eio_net_framing.Error _ -> true
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
      Alcotest.check_raises "exact partial EOF" (Eio_net_framing.Error expected)
        (fun () -> ignore (Eio_net.Frame.recv frame)))
    [
      ( "\000\000",
        Eio_net_framing.Truncated { part = Prefix; expected = 4; received = 2 }
      );
      ( "\000\000\000\003x",
        Eio_net_framing.Truncated { part = Payload; expected = 3; received = 1 }
      );
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
    (Eio_net_framing.Error Eio_net_framing.Aborted) (fun () ->
      send channel "second");
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
          Eio_net.Frame.send_batch sender
            (List.map (fun s -> [ Cstruct.of_string s ]) [ "a1"; "a2" ]))
        (fun () ->
          Eio_net.Frame.send_batch sender
            (List.map (fun s -> [ Cstruct.of_string s ]) [ "b1"; "b2" ])))
    (fun () ->
      for _ = 1 to 4 do
        got := Eio_net.Frame.recv receiver :: !got
      done);
  let got = List.rev !got in
  Alcotest.(check bool)
    "whole batches survive byte-sized competing writes" true
    (got = [ "a1"; "a2"; "b1"; "b2" ] || got = [ "b1"; "b2"; "a1"; "a2" ])

(* A flow whose input arrives through a stream: [None] ends it, and a read
   with nothing queued blocks, so a test can cancel a receive at a chosen
   point. Writes accumulate in a buffer. *)
module Scripted_flow = struct
  type t = { input : string option Eio.Stream.t; output : Buffer.t }

  let single_read t buffer =
    match Eio.Stream.take t.input with
    | None ->
        Eio.Stream.add t.input None;
        raise End_of_file
    | Some chunk ->
        Cstruct.blit_from_string chunk 0 buffer 0 (String.length chunk);
        String.length chunk

  let single_write t buffers =
    let data = Cstruct.copyv buffers in
    Buffer.add_string t.output data;
    String.length data

  let handler =
    Eio.Flow.Pi.two_way
      (module struct
        type nonrec t = t

        let read_methods = []
        let single_read = single_read
        let single_write = single_write
        let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
        let shutdown _ _ = ()
      end)

  let v chunks =
    let input = Eio.Stream.create max_int in
    List.iter (Eio.Stream.add input) chunks;
    let t = { input; output = Buffer.create 64 } in
    (t, Eio.Resource.T (t, handler))
end

let invalid_length length =
  Eio_net_framing.Invalid_length { length; minimum = 0; maximum = 1_048_576 }

let test_sticky_read_error () =
  Eio_mock.Backend.run @@ fun () ->
  let _, flow =
    Scripted_flow.v [ Some "\255\255\255\255"; Some "\000\000\000\001x"; None ]
  in
  let channel = Eio_net.Frame.of_length_prefixed_flow flow in
  let refusal = Eio_net_framing.Error (invalid_length 0xffff_ffffL) in
  Alcotest.check_raises "hostile length refused" refusal (fun () ->
      ignore (Eio_net.Frame.recv channel));
  Alcotest.check_raises "the refusal is sticky" refusal (fun () ->
      ignore (Eio_net.Frame.recv channel));
  let _, flow = Scripted_flow.v [ Some "\000\000\000\003x"; None ] in
  let channel = Eio_net.Frame.of_length_prefixed_flow flow in
  let truncated =
    Eio_net_framing.Error
      (Truncated { part = Payload; expected = 3; received = 1 })
  in
  Alcotest.check_raises "truncated payload" truncated (fun () ->
      ignore (Eio_net.Frame.recv channel));
  Alcotest.check_raises "truncation is sticky, not a clean end" truncated
    (fun () -> ignore (Eio_net.Frame.recv channel))

let test_cancelled_read () =
  Eio_mock.Backend.run @@ fun () ->
  let script, flow = Scripted_flow.v [ Some "\000\000\000\005ab" ] in
  let channel = Eio_net.Frame.of_length_prefixed_flow flow in
  Eio.Fiber.first (fun () -> ignore (Eio_net.Frame.recv channel)) (fun () -> ());
  List.iter (Eio.Stream.add script.input) [ Some "cde"; None ];
  Alcotest.check_raises "a receive cancelled inside a payload aborts"
    (Eio_net_framing.Error Aborted) (fun () ->
      ignore (Eio_net.Frame.recv channel));
  let script, flow = Scripted_flow.v [] in
  let channel = Eio_net.Frame.of_length_prefixed_flow flow in
  Eio.Fiber.first (fun () -> ignore (Eio_net.Frame.recv channel)) (fun () -> ());
  List.iter (Eio.Stream.add script.input) [ Some "\000\000\000\002ok"; None ];
  Alcotest.(check string)
    "a receive cancelled before a frame leaves the stream usable" "ok"
    (Eio_net.Frame.recv channel)

let test_refused_batch_usable () =
  Eio_mock.Backend.run @@ fun () ->
  let script, flow = Scripted_flow.v [] in
  let framing = Eio_net_framing.Config.v ~max_length:4 () in
  let channel = Eio_net.Frame.of_length_prefixed_flow ~framing flow in
  Alcotest.check_raises "oversized batch refused"
    (Eio_net_framing.Error
       (Invalid_length { length = 5L; minimum = 0; maximum = 4 }))
    (fun () ->
      Eio_net.Frame.send_batch channel
        [ [ Cstruct.of_string "ok" ]; [ Cstruct.of_string "large" ] ]);
  Alcotest.(check string) "nothing written" "" (Buffer.contents script.output);
  send channel "next";
  Alcotest.(check string)
    "a refused batch leaves the stream usable" "\000\000\000\004next"
    (Buffer.contents script.output)

let suite =
  ( "frame",
    [
      Alcotest.test_case "truncated frame EOF" `Quick test_truncated_frames;
      Alcotest.test_case "sticky read error" `Quick test_sticky_read_error;
      Alcotest.test_case "cancelled read" `Quick test_cancelled_read;
      Alcotest.test_case "a refused batch leaves the stream usable" `Quick
        test_refused_batch_usable;
      Alcotest.test_case "partial write cancellation" `Quick
        test_partial_write_cancelled;
      Alcotest.test_case "competing fragmented writers" `Quick
        test_concurrent_fragmented_writers;
      Alcotest.test_case "invalid batch emits no bytes" `Quick
        test_batch_refused_before_writing;
      Alcotest.test_case "in-memory pair round-trip" `Quick test_pair;
      Alcotest.test_case "recv_slice ingress view" `Quick test_recv_slice;
      Alcotest.test_case "v defaults" `Quick test_v_defaults;
      Alcotest.test_case "v overrides" `Quick test_v_overrides;
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
