open Alcobar

let field sixteen = if sixteen then Framing.Uint16_be else Framing.Uint32_be

let assert_bound config state =
  if Framing.Decoder.pending_bytes state > Framing.Config.buffer_size config
  then fail "framing retained bytes beyond the configured session bound"

let rec drain config state wire quantum frames =
  assert_bound config state;
  let state, event = Framing.Decoder.next_event state in
  match event with
  | Read requested ->
      if requested <= 0 then fail "decoder requested nonpositive progress";
      if String.length wire = 0 then (state, List.rev frames)
      else
        let length = min requested (min quantum (String.length wire)) in
        let first = String.sub wire 0 length in
        let rest = String.sub wire length (String.length wire - length) in
        drain config
          (Framing.Decoder.handle state (Data first))
          rest quantum frames
  | Frame frame -> drain config state wire quantum (frame :: frames)
  | End_of_stream when String.length wire = 0 -> (state, List.rev frames)
  | End_of_stream -> fail "decoder ended before consuming fair input"
  | Failed error -> failf "%a" Framing.pp_error error

let bounded maximum bytes =
  if String.length bytes > maximum then String.sub bytes 0 maximum else bytes

let fair_delivery sixteen quantum batches =
  let maximum = if quantum mod 2 = 0 then 100 else 1000 in
  let config =
    Framing.Config.v ~length_field:(field sixteen) ~max_length:maximum ()
  in
  let writer = ref (Framing.Encoder.v config) in
  let reader = ref (Framing.Decoder.v config) in
  let expected = ref [] and delivered = ref [] in
  let batches =
    [ [ (false, "opening") ] ] @ batches @ [ [ (false, "closing") ] ]
  in
  List.iter
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
      let next, result =
        Framing.Encoder.outgoing !writer
          (List.map (fun s -> [ Cstruct.of_string s ]) batch)
      in
      writer := next;
      match result with
      | Error (Framing.Invalid_length _) when not admissible ->
          if Framing.Encoder.pending_bytes !writer <> 0 then
            fail "invalid batch stalled writer"
      | Error error -> failf "fair writer refused: %a" Framing.pp_error error
      | Ok _ when not admissible -> fail "oversized batch reached the stream"
      | Ok wire ->
          let state, frames =
            drain config !reader (Cstruct.copyv wire) (1 + (quantum mod 31)) []
          in
          reader := state;
          expected := !expected @ batch;
          delivered := !delivered @ frames;
          writer := Framing.Encoder.complete !writer Sent;
          check_eq ~pp:(Fmt.Dump.list Fmt.string) !expected !delivered;
          if Framing.Decoder.pending_bytes !reader <> 0 then
            fail "complete batch stayed queued";
          if Framing.Encoder.pending_bytes !writer <> 0 then
            fail "completed write stayed pending")
    batches;
  let state = Framing.Decoder.handle !reader (Eof "") in
  let _, event = Framing.Decoder.next_event state in
  match event with
  | End_of_stream -> ()
  | _ -> fail "fair delivery did not converge to clean EOF"

let arbitrary_wire sixteen size bytes =
  let maximum = if size mod 2 = 0 then 100 else 1000 in
  let config =
    Framing.Config.v ~length_field:(field sixteen) ~max_length:maximum ()
  in
  let state = ref (Framing.Decoder.v config) and offset = ref 0 in
  let remaining = ref (String.length bytes + 2) in
  let terminal = ref false in
  while not !terminal do
    decr remaining;
    if !remaining < 0 then
      fail "decoder failed to consume input or report an outcome";
    assert_bound config !state;
    let next, event = Framing.Decoder.next_event !state in
    state := next;
    match event with
    | Read requested ->
        if requested <= 0 || requested > Framing.Config.buffer_size config then
          fail "decoder requested unbounded or zero-byte work";
        if !offset = String.length bytes then
          state := Framing.Decoder.handle !state (Eof "")
        else begin
          let count = min requested (String.length bytes - !offset) in
          state :=
            Framing.Decoder.handle !state
              (Data (String.sub bytes !offset count));
          offset := !offset + count
        end
    | Frame payload ->
        if String.length payload > maximum then fail "oversized accepted frame"
    | End_of_stream | Failed _ -> terminal := true
  done;
  assert_bound config !state

let cancelled_writer sixteen payload =
  let config = Framing.Config.v ~length_field:(field sixteen) () in
  let writer = Framing.Encoder.v config in
  let writer, result =
    Framing.Encoder.outgoing writer
      [ [ Cstruct.of_string (bounded 1000 payload) ] ]
  in
  match result with
  | Error error -> failf "%a" Framing.pp_error error
  | Ok _ ->
      let writer = Framing.Encoder.complete writer Failed in
      let writer = Framing.Encoder.complete writer Sent in
      let _, result =
        Framing.Encoder.outgoing writer [ [ Cstruct.of_string "later" ] ]
      in
      (match result with
      | Error Framing.Aborted -> ()
      | _ -> fail "failed stream resumed");
      if Framing.Encoder.pending_bytes writer <> 0 then
        fail "failed stream retained pending bytes"

let suite =
  ( "framing",
    [
      test_case "finite fragmentation drains every accepted batch in FIFO order"
        [ bool; uint8; list (list (pair bool bytes)) ]
        fair_delivery;
      test_case "arbitrary wire obeys work and retention bounds"
        [ bool; uint8; bytes ] arbitrary_wire;
      test_case "partial failed writes never reopen or replay" [ bool; bytes ]
        cancelled_writer;
    ] )
