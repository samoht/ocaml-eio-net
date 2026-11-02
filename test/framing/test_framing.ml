module Framing = Eio_net_framing

let error = Alcotest.testable Framing.pp_error Framing.equal_error

(* A source that hands out its chunks one at a time, so a reader sees the
   input split exactly at the chunk boundaries. *)
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

  let handler =
    Eio.Flow.Pi.source
      (module struct
        type nonrec t = t

        let read_methods = read_methods
        let single_read = single_read
      end)

  let reader config chunks =
    Eio.Buf_read.of_flow
      ~max_size:(Framing.Config.buffer_size config)
      (Eio.Resource.T (ref (List.filter (( <> ) "") chunks), handler))
end

(* [split sizes s] cuts [s] into chunks whose lengths cycle through [sizes]. *)
let split sizes s =
  let rec go offset acc = function
    | [] -> go offset acc sizes
    | size :: rest ->
        if offset >= String.length s then List.rev acc
        else
          let length = min size (String.length s - offset) in
          go (offset + length) (String.sub s offset length :: acc) rest
  in
  go 0 [] sizes

let payloads strings = List.map (fun s -> [ Cstruct.of_string s ]) strings

let encoded config strings =
  match Framing.encode config (payloads strings) with
  | Ok buffers -> Cstruct.copyv buffers
  | Error e -> Alcotest.failf "%a" Framing.pp_error e

let written config batch =
  let buffer = Buffer.create 64 in
  Framing.write config (Eio.Flow.buffer_sink buffer) batch;
  Buffer.contents buffer

(* Every frame until [End_of_file], checking after each read that the reader
   holds no more than the configuration's bound. *)
let read_all config r =
  let rec go acc =
    match Framing.read config r with
    | frame ->
        Alcotest.(check bool)
          "retained input obeys the bound" true
          (Eio.Buf_read.buffered_bytes r <= Framing.Config.buffer_size config);
        go (frame :: acc)
    | exception End_of_file -> List.rev acc
  in
  go []

let read_error config r =
  match Framing.read config r with
  | frame -> Alcotest.failf "expected a framing error, read %S" frame
  | exception End_of_file -> Alcotest.fail "expected a framing error, got EOF"
  | exception Framing.Error e -> e

let fields =
  [ (Framing.Uint16_be, "\000\003"); (Framing.Uint32_be, "\000\000\000\003") ]

let test_network_order () =
  List.iter
    (fun (length_field, prefix) ->
      let config = Framing.Config.v ~length_field () in
      Alcotest.(check string)
        "network-order length" (prefix ^ "abc") (encoded config [ "abc" ]))
    fields;
  let config = Framing.Config.v () in
  let wire = encoded config [ String.make 0x0003_0201 'x' ] in
  Alcotest.(check string)
    "uint32 most significant byte first" "\000\003\002\001"
    (String.sub wire 0 4);
  let config = Framing.Config.v ~length_field:Uint16_be () in
  let wire = encoded config [ String.make 0x0201 'x' ] in
  Alcotest.(check string)
    "uint16 most significant byte first" "\002\001" (String.sub wire 0 2)

let test_batch () =
  List.iter
    (fun (length_field, prefix) ->
      let config = Framing.Config.v ~length_field () in
      let empty = String.sub prefix 0 (String.length prefix - 1) ^ "\000" in
      let wire = empty ^ (prefix ^ "abc") ^ prefix ^ "xyz" in
      Alcotest.(check string)
        "batch keeps frame order and boundaries" wire
        (encoded config [ ""; "abc"; "xyz" ]);
      Alcotest.(check string)
        "a payload's buffers make one frame" (prefix ^ "abc")
        (written config [ [ Cstruct.of_string "a"; Cstruct.of_string "bc" ] ]);
      Alcotest.(check string)
        "the empty payload is its prefix alone" empty (written config [ [] ]);
      Alcotest.(check string)
        "write is encode" wire
        (written config (payloads [ ""; "abc"; "xyz" ]));
      Alcotest.(check (result (list string) error))
        "the empty batch is no buffer" (Ok [])
        (Result.map (List.map Cstruct.to_string) (Framing.encode config []));
      Alcotest.(check string)
        "the empty batch writes nothing" "" (written config []))
    fields

let test_no_copy () =
  let config = Framing.Config.v () in
  let a = Cstruct.of_string "abc" and b = Cstruct.of_string "de" in
  let c = Cstruct.of_string "fgh" in
  match Framing.encode config [ [ a; b ]; [ c ] ] with
  | Error e -> Alcotest.failf "%a" Framing.pp_error e
  | Ok [ p0; a'; b'; p1; c' ] ->
      Alcotest.(check bool)
        "payload buffers are not copied" true
        (a == a' && b == b' && c == c');
      Alcotest.(check bool)
        "the prefixes share one buffer" true
        (p0.Cstruct.buffer == p1.Cstruct.buffer)
  | Ok buffers ->
      Alcotest.failf "unexpected layout of %d buffers" (List.length buffers)

let test_fragmented () =
  List.iter
    (fun (length_field, _) ->
      let config = Framing.Config.v ~length_field () in
      let frames = [ ""; "abc"; "xyz"; String.make 300 'z'; "" ] in
      let wire = encoded config frames in
      List.iter
        (fun sizes ->
          Alcotest.(check (list string))
            (Fmt.str "FIFO drain of chunks %a" Fmt.(Dump.list int) sizes)
            frames
            (read_all config (Chunked.reader config (split sizes wire))))
        [ [ 1 ]; [ 2 ]; [ 3; 1; 4; 1; 5 ]; [ 7; 100 ]; [ String.length wire ] ];
      Alcotest.(check (list string))
        "a whole string" frames
        (read_all config (Eio.Buf_read.of_string wire)))
    fields

let test_end_of_input () =
  let config = Framing.Config.v ~max_length:8 () in
  Alcotest.(check (list string))
    "empty input is a clean end" []
    (read_all config (Eio.Buf_read.of_string ""));
  let r = Eio.Buf_read.of_string "\000\000\000\002ab" in
  Alcotest.(check string) "frame" "ab" (Framing.read config r);
  Alcotest.check_raises "clean end after a frame" End_of_file (fun () ->
      ignore (Framing.read config r));
  List.iter
    (fun (chunks, expected) ->
      let r = Chunked.reader config chunks in
      Alcotest.check error "exact truncation" expected (read_error config r);
      Alcotest.(check int)
        "truncation releases the incomplete frame" 0
        (Eio.Buf_read.buffered_bytes r))
    [
      ([ "\000" ], Truncated { part = Prefix; expected = 4; received = 1 });
      ([ "\000\000" ], Truncated { part = Prefix; expected = 4; received = 2 });
      ( [ "\000\000"; "\000" ],
        Truncated { part = Prefix; expected = 4; received = 3 } );
      ( [ "\000\000\000\003x" ],
        Truncated { part = Payload; expected = 3; received = 1 } );
      ( [ "\000\000\000\003"; "x"; "y" ],
        Truncated { part = Payload; expected = 3; received = 2 } );
    ]

let test_hostile_lengths () =
  let config = Framing.Config.v ~max_length:8 () in
  List.iter
    (fun (prefix, length) ->
      let r = Chunked.reader config [ prefix; String.make 16 'p' ] in
      Alcotest.check error "exact unsigned refusal"
        (Invalid_length { length; minimum = 0; maximum = 8 })
        (read_error config r);
      Alcotest.(check int)
        "only the prefix is consumed" 4
        (Eio.Buf_read.consumed_bytes r);
      Alcotest.(check int)
        "no payload byte is requested" 0
        (Eio.Buf_read.buffered_bytes r))
    [
      ("\128\000\000\000", 0x8000_0000L);
      ("\255\255\255\255", 0xffff_ffffL);
      ("\000\000\000\009", 9L);
    ];
  let config = Framing.Config.v ~length_field:Uint16_be ~max_length:8 () in
  let r = Chunked.reader config [ "\255\255"; "p" ] in
  Alcotest.check error "uint16 refusal"
    (Invalid_length { length = 65535L; minimum = 0; maximum = 8 })
    (read_error config r);
  Alcotest.(check int)
    "uint16 prefix consumed" 2
    (Eio.Buf_read.consumed_bytes r);
  let config = Framing.Config.v ~min_length:2 ~max_length:8 () in
  Alcotest.check error "below the minimum"
    (Invalid_length { length = 1L; minimum = 2; maximum = 8 })
    (read_error config (Eio.Buf_read.of_string "\000\000\000\001x"));
  Alcotest.(check (result unit error))
    "encoding below the minimum"
    (Error (Invalid_length { length = 1L; minimum = 2; maximum = 8 }))
    (Result.map ignore (Framing.encode config (payloads [ "x" ])))

let test_uint16_boundaries () =
  let config = Framing.Config.v ~length_field:Uint16_be () in
  Alcotest.(check int)
    "uint16 default derives its capacity" 65535
    (Framing.Config.max_length config);
  let payload = String.make 65535 'x' in
  let wire = encoded config [ payload ] in
  Alcotest.(check int) "largest uint16 frame" 65537 (String.length wire);
  Alcotest.(check (list string))
    "largest uint16 payload reads back" [ payload ]
    (read_all config (Chunked.reader config (split [ 4096 ] wire)));
  Alcotest.(check (result unit error))
    "uint16 does not wrap"
    (Error (Invalid_length { length = 65536L; minimum = 0; maximum = 65535 }))
    (Result.map ignore (Framing.encode config (payloads [ payload ^ "x" ])));
  Alcotest.check_raises "uint16 capacity is enforced"
    (Invalid_argument
       "Eio_net_framing.Config.v: length range 0-65536 outside 0-65535")
    (fun () ->
      ignore (Framing.Config.v ~length_field:Uint16_be ~max_length:65536 ()))

let test_zero_length () =
  let config = Framing.Config.v ~max_length:0 () in
  let wire = encoded config [ "" ] in
  Alcotest.(check string) "zero-length frame" "\000\000\000\000" wire;
  Alcotest.(check (list string))
    "zero-only framing drains" [ ""; "" ]
    (read_all config (Chunked.reader config [ wire; wire ]));
  Alcotest.(check bool)
    "zero-only framing refuses a byte" true
    (Result.is_error (Framing.encode config (payloads [ "x" ])))

let test_oversized_in_batch () =
  let config = Framing.Config.v () in
  let maximum = Framing.Config.max_length config in
  let batch =
    [ [ Cstruct.of_string "valid" ]; [ Cstruct.create (maximum + 1) ] ]
  in
  Alcotest.(check (result unit error))
    "an oversized frame refuses the batch"
    (Error
       (Invalid_length
          { length = Int64.of_int (maximum + 1); minimum = 0; maximum }))
    (Result.map ignore (Framing.encode config batch));
  let buffer = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buffer in
  Alcotest.check_raises "write refuses the batch"
    (Framing.Error
       (Invalid_length
          { length = Int64.of_int (maximum + 1); minimum = 0; maximum }))
    (fun () -> Framing.write config sink batch);
  Alcotest.(check string) "nothing is written" "" (Buffer.contents buffer);
  Framing.write config sink (payloads [ "next" ]);
  Alcotest.(check string)
    "a later batch is written" "\000\000\000\004next" (Buffer.contents buffer)

let test_config () =
  let config = Framing.Config.v () in
  Alcotest.(check string)
    "defaults" "uint32be length:0-1048576"
    (Fmt.str "%a" Framing.Config.pp config);
  Alcotest.(check int)
    "buffer size" 1_048_580
    (Framing.Config.buffer_size config);
  Alcotest.(check bool)
    "equal" true
    (Framing.Config.equal config
       (Framing.Config.v ~length_field:Uint32_be ~max_length:1_048_576 ()));
  List.iter
    (fun (name, f) ->
      Alcotest.(check bool)
        name true
        (match f () with
        | (_ : Framing.Config.t) -> false
        | exception Invalid_argument _ -> true))
    [
      ("negative minimum", fun () -> Framing.Config.v ~min_length:(-1) ());
      ( "maximum below minimum",
        fun () -> Framing.Config.v ~min_length:4 ~max_length:3 () );
      ( "minimum above the default maximum",
        fun () -> Framing.Config.v ~min_length:2_000_000 () );
      ( "maximum above uint32",
        fun () -> Framing.Config.v ~max_length:0x1_0000_0000 () );
    ]

let suite =
  ( "framing",
    [
      Alcotest.test_case "network byte order" `Quick test_network_order;
      Alcotest.test_case "batch order, empty payload and empty batch" `Quick
        test_batch;
      Alcotest.test_case "payloads are not copied" `Quick test_no_copy;
      Alcotest.test_case "fragmented input in FIFO order" `Quick test_fragmented;
      Alcotest.test_case "clean and truncated end of input" `Quick
        test_end_of_input;
      Alcotest.test_case "hostile lengths read no payload" `Quick
        test_hostile_lengths;
      Alcotest.test_case "uint16 boundaries" `Quick test_uint16_boundaries;
      Alcotest.test_case "zero-length framing" `Quick test_zero_length;
      Alcotest.test_case "an oversized frame refuses its batch" `Quick
        test_oversized_in_batch;
      Alcotest.test_case "configurations" `Quick test_config;
    ] )
