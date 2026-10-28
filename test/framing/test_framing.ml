let error = Alcotest.testable Framing.pp_error Framing.equal_error

let encoded config payload =
  match Framing.encode config (Rope.of_string payload) with
  | Ok bytes -> Rope.to_string bytes
  | Error error -> Alcotest.failf "%a" Framing.pp_error error

let rec feed state bytes frames =
  let state, event = Framing.Decoder.next_event state in
  match event with
  | Read needed when String.length bytes > 0 ->
      let count = min needed (String.length bytes) in
      let chunk = String.sub bytes 0 count in
      let rest = String.sub bytes count (String.length bytes - count) in
      feed (Framing.Decoder.handle state (Data chunk)) rest frames
  | Read _ -> (state, List.rev frames)
  | Frame frame -> feed state bytes (frame :: frames)
  | End_of_stream -> (state, List.rev frames)
  | Failed error -> Alcotest.failf "%a" Framing.pp_error error

let terminal state expected =
  let _, event = Framing.Decoder.next_event state in
  match event with
  | Failed observed -> Alcotest.check error "terminal refusal" expected observed
  | _ -> Alcotest.fail "expected terminal framing refusal"

let test_vectors () =
  List.iter
    (fun (field, prefix) ->
      let config = Framing.Config.v ~length_field:field () in
      Alcotest.(check string)
        "network-order length" (prefix ^ "abc") (encoded config "abc");
      let wire =
        encoded config "" ^ encoded config "abc" ^ encoded config "xyz"
      in
      let batch =
        Framing.encode_batch config
          [ Rope.empty; Rope.of_string "abc"; Rope.of_string "xyz" ]
      in
      Alcotest.(check (result string error))
        "batch retains frame order and boundaries" (Ok wire)
        (Result.map Rope.to_string batch);
      let state = ref (Framing.Decoder.v config) and frames = ref [] in
      String.iter
        (fun byte ->
          let next, output = feed !state (String.make 1 byte) [] in
          state := next;
          frames := !frames @ output;
          Alcotest.(check bool)
            "retained input obeys bound" true
            (Framing.Decoder.pending_bytes !state
            <= Framing.Config.buffer_size config))
        wire;
      Alcotest.(check (list string))
        "fragmented FIFO drain" [ ""; "abc"; "xyz" ] !frames)
    [ (Framing.Uint16_be, "\000\003"); (Framing.Uint32_be, "\000\000\000\003") ]

let test_partial_eof () =
  let config = Framing.Config.v ~max_length:8 () in
  let state =
    Framing.Decoder.handle (Framing.Decoder.v config) (Eof "\000\000")
  in
  terminal state (Truncated { part = Prefix; expected = 4; received = 2 });
  let state, _ = feed (Framing.Decoder.v config) "\000\000\000\003" [] in
  let state = Framing.Decoder.handle state (Eof "x") in
  terminal state (Truncated { part = Payload; expected = 3; received = 1 });
  Alcotest.(check int)
    "truncation releases body" 0
    (Framing.Decoder.pending_bytes state);
  terminal
    (Framing.Decoder.handle state (Data "xx"))
    (Truncated { part = Payload; expected = 3; received = 1 })

let test_hostile_lengths () =
  let config = Framing.Config.v ~max_length:8 () in
  List.iter
    (fun (prefix, length) ->
      let state =
        Framing.Decoder.handle (Framing.Decoder.v config) (Data prefix)
      in
      terminal state (Invalid_length { length; minimum = 0; maximum = 8 });
      Alcotest.(check int)
        "no refused payload is retained" 0
        (Framing.Decoder.pending_bytes state))
    [ ("\128\000\000\000", 2147483648L); ("\255\255\255\255", 4294967295L) ];
  let state = Framing.Decoder.handle (Framing.Decoder.v config) (Data "") in
  terminal state (Invalid_chunk { length = 0; requested = 4 });
  let state =
    Framing.Decoder.handle (Framing.Decoder.v config) (Data "12345")
  in
  terminal state (Invalid_chunk { length = 5; requested = 4 })

let test_ready_then_terminal () =
  let config = Framing.Config.v () in
  let state =
    Framing.Decoder.handle (Framing.Decoder.v config) (Data "\000\000\000\003")
  in
  let state = Framing.Decoder.handle state (Data "abc") in
  let state = Framing.Decoder.handle state (Eof "") in
  let state = Framing.Decoder.handle state Abort in
  let state, event = Framing.Decoder.next_event state in
  Alcotest.(check bool)
    "ready frame survives end" true
    (match event with Frame "abc" -> true | _ -> false);
  let _, event = Framing.Decoder.next_event state in
  Alcotest.(check bool)
    "clean end stays terminal" true
    (match event with End_of_stream -> true | _ -> false)

let test_encoder_lifecycle () =
  let config = Framing.Config.v ~max_length:4 () in
  let writer = Framing.Encoder.v config in
  let writer, result =
    Framing.Encoder.outgoing writer
      [ Rope.of_string "ok"; Rope.of_string "large" ]
  in
  Alcotest.(check bool)
    "whole invalid batch refused" true (Result.is_error result);
  let writer, result =
    Framing.Encoder.outgoing writer [ Rope.of_string "ok" ]
  in
  Alcotest.(check bool)
    "refusal leaves writer usable" true (Result.is_ok result);
  Alcotest.(check int)
    "one write accounts encoded bytes" 6
    (Framing.Encoder.pending_bytes writer);
  let writer, result =
    Framing.Encoder.outgoing writer [ Rope.of_string "next" ]
  in
  Alcotest.(check (result unit error))
    "no overlapping write" (Error Busy)
    (Result.map (fun _ -> ()) result);
  let writer = Framing.Encoder.complete writer Failed in
  let writer = Framing.Encoder.complete writer Sent in
  let _, result = Framing.Encoder.outgoing writer [ Rope.of_string "next" ] in
  Alcotest.(check (result unit error))
    "failed write cannot resume" (Error Aborted)
    (Result.map (fun _ -> ()) result)

let test_boundaries () =
  let config = Framing.Config.v ~length_field:Uint16_be () in
  Alcotest.(check int)
    "uint16 default derives its capacity" 65535
    (Framing.Config.max_length config);
  let payload = String.make 65535 'x' in
  Alcotest.(check int)
    "largest uint16 payload" 65537
    (String.length (encoded config payload));
  Alcotest.(check bool)
    "uint16 does not wrap" true
    (Result.is_error (Framing.encode config (Rope.of_string (payload ^ "x"))));
  let config = Framing.Config.v ~max_length:0 () in
  let state, frames = feed (Framing.Decoder.v config) (encoded config "") [] in
  Alcotest.(check (list string)) "zero-only framing drains" [ "" ] frames;
  let _, event =
    Framing.Decoder.next_event (Framing.Decoder.handle state (Eof ""))
  in
  Alcotest.(check bool)
    "empty clean EOF" true
    (match event with End_of_stream -> true | _ -> false)

let test_virtual_payload () =
  let rec grow payload =
    if Rope.length payload > max_int / 4 then payload
    else grow (Rope.concat payload payload)
  in
  let payload = grow (Rope.of_string "x") in
  let writer, result =
    Framing.Encoder.outgoing
      (Framing.Encoder.v (Framing.Config.v ()))
      [ Rope.of_string "valid"; payload ]
  in
  Alcotest.(check bool)
    "shared virtual payload refused before traversal" true
    (Result.is_error result);
  Alcotest.(check int)
    "invalid batch retains no encoded payload" 0
    (Framing.Encoder.pending_bytes writer);
  let _, result = Framing.Encoder.outgoing writer [ Rope.of_string "next" ] in
  Alcotest.(check bool)
    "large refusal leaves writer usable" true (Result.is_ok result)

let suite =
  ( "framing",
    [
      Alcotest.test_case "network byte order and fragmented FIFO" `Quick
        test_vectors;
      Alcotest.test_case "partial prefix and payload EOF" `Quick
        test_partial_eof;
      Alcotest.test_case "hostile lengths and invalid progress" `Quick
        test_hostile_lengths;
      Alcotest.test_case "ready frame before EOF and abort" `Quick
        test_ready_then_terminal;
      Alcotest.test_case "atomic encoder lifecycle" `Quick
        test_encoder_lifecycle;
      Alcotest.test_case "uint16 and empty frame boundaries" `Quick
        test_boundaries;
      Alcotest.test_case "shared virtual payload preflight" `Quick
        test_virtual_payload;
    ] )
