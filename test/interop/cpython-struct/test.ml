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

module Framing = Eio_net_framing

(* A source that hands out its chunks one at a time, so the reader sees the
   prefix before any payload byte. *)
module Chunked = struct
  type t = string list ref

  let read_methods = []

  let single_read t dst =
    match !t with
    | [] -> raise End_of_file
    | chunk :: rest ->
        let length = min (Cstruct.length dst) (String.length chunk) in
        Cstruct.blit_from_string chunk 0 dst 0 length;
        t :=
          if length = String.length chunk then rest
          else String.sub chunk length (String.length chunk - length) :: rest;
        length

  let reader config chunks =
    Eio.Buf_read.of_flow
      ~max_size:(Framing.Config.buffer_size config)
      (Eio.Resource.T
         ( ref (List.filter (( <> ) "") chunks),
           Eio.Flow.Pi.source
             (module struct
               type nonrec t = t

               let read_methods = read_methods
               let single_read = single_read
             end) ))
end

let assert_frame config prefix payload =
  let r = Chunked.reader config [ prefix; payload ] in
  Alcotest.(check string)
    "oracle payload delivered" payload (Framing.read config r);
  Alcotest.(check int)
    "oracle frame consumed"
    (String.length prefix + String.length payload)
    (Eio.Buf_read.consumed_bytes r);
  Alcotest.check_raises "oracle frame drained" End_of_file (fun () ->
      ignore (Framing.read config r))

let check_encode config vector =
  let prefix = Ohex.decode vector.prefix_hex in
  let payload = String.make (Int64.to_int vector.length) 'x' in
  (match Framing.encode config [ [ Cstruct.of_string payload ] ] with
  | Ok frame ->
      Alcotest.(check string)
        "Python struct bytes" (prefix ^ payload) (Cstruct.copyv frame)
  | Error error -> Alcotest.failf "%a" Framing.pp_error error);
  assert_frame config prefix payload

let check_refuse_declared config vector =
  let prefix = Ohex.decode vector.prefix_hex in
  let r = Chunked.reader config [ prefix; "payload" ] in
  match Framing.read config r with
  | _ -> Alcotest.fail "hostile oracle length was accepted"
  | exception Framing.Error (Invalid_length { length; _ }) ->
      Alcotest.(check int64)
        "unsigned oracle length stays exact" vector.length length;
      Alcotest.(check int)
        "hostile oracle length requested no payload byte" 0
        (Eio.Buf_read.buffered_bytes r)

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
          [
            [ Cstruct.of_string (String.make (Int64.to_int vector.length) 'x') ];
          ]
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
