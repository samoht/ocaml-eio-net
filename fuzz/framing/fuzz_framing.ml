open Alcobar
module Framing = Eio_net_framing

let field sixteen = if sixteen then Framing.Uint16_be else Framing.Uint32_be

(* A source that hands out its chunks one at a time, so the reader sees the
   input split exactly at the chunk boundaries. *)
module Chunked = struct
  type t = string list ref

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

        let read_methods = []
        let single_read = single_read
      end)

  (* [reader config sizes wire] reads [wire] in chunks whose lengths cycle
     through [sizes]. *)
  let reader config sizes wire =
    let sizes = List.map (fun size -> 1 + (size mod 64)) (1 :: sizes) in
    let rec split offset acc = function
      | [] -> split offset acc sizes
      | size :: rest ->
          if offset >= String.length wire then List.rev acc
          else
            let length = min size (String.length wire - offset) in
            split (offset + length) (String.sub wire offset length :: acc) rest
    in
    Eio.Buf_read.of_flow
      ~max_size:(Framing.Config.buffer_size config)
      (Eio.Resource.T (ref (split 0 [] sizes), handler))
end

let assert_bound config r =
  if Eio.Buf_read.buffered_bytes r > Framing.Config.buffer_size config then
    fail "the reader retained bytes beyond the configured bound"

let bounded maximum bytes =
  if String.length bytes > maximum then String.sub bytes 0 maximum else bytes

(* Batches written one after another, with arbitrary oversized payloads that
   must refuse their whole batch, read back under arbitrary fragmentation. *)
let round_trip sixteen sizes batches =
  let maximum = if List.length sizes mod 2 = 0 then 100 else 1000 in
  let config =
    Framing.Config.v ~length_field:(field sixteen) ~max_length:maximum ()
  in
  let buffer = Buffer.create 1024 in
  let sink = Eio.Flow.buffer_sink buffer in
  let expected =
    List.concat_map
      (fun batch ->
        let batch =
          List.map
            (fun (invalid, payload) ->
              if invalid then String.make (maximum + 1) 'x'
              else bounded maximum payload)
            batch
        in
        let admissible =
          List.for_all (fun payload -> String.length payload <= maximum) batch
        in
        let before = Buffer.length buffer in
        match
          Framing.write config sink
            (List.map (fun s -> [ Cstruct.of_string s ]) batch)
        with
        | () when admissible -> batch
        | () -> fail "an oversized batch reached the stream"
        | exception Framing.Error (Invalid_length _) when not admissible ->
            if Buffer.length buffer <> before then
              fail "a refused batch wrote bytes";
            []
        | exception Framing.Error e ->
            failf "an admissible batch was refused: %a" Framing.pp_error e)
      batches
  in
  let r = Chunked.reader config sizes (Buffer.contents buffer) in
  let rec drain acc =
    match Framing.read config r with
    | frame ->
        assert_bound config r;
        drain (frame :: acc)
    | exception End_of_file -> List.rev acc
    | exception Framing.Error e -> failf "%a" Framing.pp_error e
  in
  check_eq ~pp:Fmt.(Dump.list string) expected (drain [])

(* Arbitrary bytes end in frames, then a clean end or an error, within a
   bounded number of reads and without the reader holding more than the
   bound. *)
let arbitrary_wire sixteen sizes bytes =
  let maximum = if List.length sizes mod 2 = 0 then 100 else 1000 in
  let config =
    Framing.Config.v ~length_field:(field sixteen) ~max_length:maximum ()
  in
  let r = Chunked.reader config sizes bytes in
  let limit = (String.length bytes / Framing.Config.prefix_length config) + 1 in
  let rec go reads =
    if reads > limit then fail "a read consumed no frame";
    match Framing.read config r with
    | payload ->
        assert_bound config r;
        if String.length payload > maximum then fail "oversized accepted frame";
        go (reads + 1)
    | exception End_of_file -> ()
    | exception Framing.Error (Invalid_length { length; _ }) ->
        if Int64.compare length (Int64.of_int maximum) <= 0 then
          fail "an admissible length was refused";
        assert_bound config r
    | exception Framing.Error (Truncated _) ->
        if Eio.Buf_read.buffered_bytes r <> 0 then
          fail "truncation retained the incomplete frame"
    | exception Framing.Error Aborted -> fail "read reported Aborted"
  in
  go 0

module Failing_flow = struct
  type t = { buffer : Buffer.t; mutable budget : int }

  let single_write t buffers =
    if t.budget = 0 then failwith "write failed";
    let data = Cstruct.copyv buffers in
    let length = min t.budget (String.length data) in
    Buffer.add_string t.buffer (String.sub data 0 length);
    t.budget <- t.budget - length;
    length

  let handler =
    Eio.Flow.Pi.two_way
      (module struct
        type nonrec t = t

        let read_methods = []
        let single_read _ _ = raise End_of_file
        let single_write = single_write
        let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
        let shutdown _ _ = ()
      end)
end

(* A write that fails partway refuses every later write, and nothing more
   reaches the flow. *)
let failed_writer sixteen budget payload =
  Eio_mock.Backend.run @@ fun () ->
  let framing = Framing.Config.v ~length_field:(field sixteen) () in
  let payload = bounded 1000 payload in
  let frame_length =
    Framing.Config.prefix_length framing + String.length payload
  in
  let budget = budget mod frame_length in
  let flow = { Failing_flow.buffer = Buffer.create 64; budget } in
  let channel =
    Eio_net.Frame.of_length_prefixed_flow ~framing
      (Eio.Resource.T (flow, Failing_flow.handler))
  in
  (match channel.send [ Cstruct.of_string payload ] with
  | () -> fail "a write with too small a budget succeeded"
  | exception Failure _ -> ());
  (match channel.send [ Cstruct.of_string "later" ] with
  | () -> fail "a failed stream resumed"
  | exception Framing.Error Aborted -> ()
  | exception _ -> fail "a failed stream did not report Aborted");
  if Buffer.length flow.buffer <> budget then
    fail "bytes reached the flow after the failure"

let suite =
  ( "framing",
    [
      test_case "batches round-trip under arbitrary fragmentation"
        [ bool; list uint8; list (list (pair bool bytes)) ]
        round_trip;
      test_case "arbitrary wire obeys work and retention bounds"
        [ bool; list uint8; bytes ]
        arbitrary_wire;
      test_case "partial failed writes never reopen" [ bool; uint16; bytes ]
        failed_writer;
    ] )
