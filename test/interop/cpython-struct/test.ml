type vector = {
  operation : string;
  length_field : int;
  length : int64;
  prefix_hex : string;
}

(* One vector per CRLF-terminated line after the header: four comma-separated
   fields, none of them quoted. *)
let vector line =
  match String.split_on_char ',' line with
  | [ operation; length_field; length; prefix_hex ] ->
      {
        operation;
        length_field = int_of_string length_field;
        length = Int64.of_string length;
        prefix_hex;
      }
  | _ -> Fmt.failwith "malformed vector: %S" line

let vectors input =
  match
    List.map String.trim (String.split_on_char '\n' (String.trim input))
  with
  | "operation,length_field,length,prefix_hex" :: lines -> List.map vector lines
  | _ -> failwith "vectors.csv: unexpected header"

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
  let prefix = Ohex.decode vector.prefix_hex in
  let payload = String.make (Int64.to_int vector.length) 'x' in
  (match Framing.encode config [ Cstruct.of_string payload ] with
  | Ok frame ->
      Alcotest.(check string)
        "Python struct bytes" (prefix ^ payload) (Cstruct.copyv frame)
  | Error error -> Alcotest.failf "%a" Framing.pp_error error);
  assert_frame config prefix payload

let check_refuse_declared config vector =
  let state =
    Framing.Decoder.handle (Framing.Decoder.v config)
      (Data (Ohex.decode vector.prefix_hex))
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
          [ Cstruct.of_string (String.make (Int64.to_int vector.length) 'x') ]
      in
      Alcotest.(check bool)
        "Python struct overflow is refused" true (Result.is_error result)
  | "refuse_declared" -> check_refuse_declared config vector
  | _ -> Alcotest.fail "unknown oracle operation"

let () =
  let input =
    In_channel.with_open_bin "traces/vectors.csv" In_channel.input_all
  in
  let vectors = vectors input in
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
