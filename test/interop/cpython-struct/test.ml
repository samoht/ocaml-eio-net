type vector = {
  operation : string;
  length_field : int;
  length : int64;
  prefix_hex : string;
}

let codec =
  Csv.Row.(
    obj (fun operation length_field length prefix_hex ->
        { operation; length_field; length; prefix_hex })
    |> col "operation" Csv.Field.string ~enc:(fun v -> v.operation)
    |> col "length_field" Csv.Field.int ~enc:(fun v -> v.length_field)
    |> col "length" Csv.Field.int64 ~enc:(fun v -> v.length)
    |> col "prefix_hex" Csv.Field.string ~enc:(fun v -> v.prefix_hex)
    |> finish)

let assert_frame config prefix payload =
  let state = Framing.Decoder.handle (Framing.Decoder.v config) (Data prefix) in
  let state, event = Framing.Decoder.next_event state in
  let state, event =
    match event with
    | Read length ->
        Alcotest.(check int)
          "oracle payload length requested" (String.length payload) length;
        Framing.Decoder.next_event (Framing.Decoder.handle state (Data payload))
    | event -> (state, event)
  in
  match event with
  | Frame received ->
      Alcotest.(check string) "oracle payload delivered" payload received;
      Alcotest.(check int)
        "oracle frame drained" 0
        (Framing.Decoder.pending_bytes state)
  | _ -> Alcotest.fail "oracle frame was not delivered"

let check_encode config vector =
  let prefix = Hex.decode vector.prefix_hex in
  let payload = String.make (Int64.to_int vector.length) 'x' in
  (match Framing.encode config (Rope.of_string payload) with
  | Ok frame ->
      Alcotest.(check string)
        "Python struct bytes" (prefix ^ payload) (Rope.to_string frame)
  | Error error -> Alcotest.failf "%a" Framing.pp_error error);
  assert_frame config prefix payload

let check_refuse_declared config vector =
  let state =
    Framing.Decoder.handle (Framing.Decoder.v config)
      (Data (Hex.decode vector.prefix_hex))
  in
  let _, event = Framing.Decoder.next_event state in
  match event with
  | Failed (Invalid_length { length; _ }) ->
      Alcotest.(check int64)
        "unsigned oracle length stays exact" vector.length length
  | _ -> Alcotest.fail "hostile oracle length requested payload bytes"

let check vector () =
  let length_field =
    match vector.length_field with
    | 16 -> Framing.Uint16_be
    | 32 -> Uint32_be
    | _ -> Alcotest.fail "unknown oracle length field"
  in
  let config = Framing.Config.v ~length_field () in
  match vector.operation with
  | "encode" -> check_encode config vector
  | "refuse_encode" ->
      let result =
        Framing.encode config
          (Rope.of_string (String.make (Int64.to_int vector.length) 'x'))
      in
      Alcotest.(check bool)
        "Python struct overflow is refused" true (Result.is_error result)
  | "refuse_declared" -> check_refuse_declared config vector
  | _ -> Alcotest.fail "unknown oracle operation"

let () =
  let input =
    In_channel.with_open_bin "traces/vectors.csv" In_channel.input_all
  in
  let vectors = Csv.of_string_exn ~max_rows:32 ~max_cols:4 codec input in
  Alcotest.run "framing-python"
    [
      ( "struct",
        List.map
          (fun vector ->
            Alcotest.test_case
              (Fmt.str "%s uint%d length %Ld" vector.operation
                 vector.length_field vector.length)
              `Quick (check vector))
          vectors );
    ]
