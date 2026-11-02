type length_field = Uint16_be | Uint32_be
type part = Prefix | Payload

type error =
  | Invalid_length of { length : int64; minimum : int; maximum : int }
  | Invalid_chunk of { length : int; requested : int }
  | Truncated of { part : part; expected : int; received : int }
  | Busy
  | Aborted

let equal_error a b = a = b

let pp_error ppf = function
  | Invalid_length { length; minimum; maximum } ->
      Fmt.pf ppf "frame length %Ld outside %d-%d" length minimum maximum
  | Invalid_chunk { length; requested } ->
      Fmt.pf ppf "received %d bytes for a read of 1-%d" length requested
  | Truncated { part; expected; received } ->
      Fmt.pf ppf "truncated %s: received %d of %d bytes"
        (match part with Prefix -> "length prefix" | Payload -> "payload")
        received expected
  | Busy -> Fmt.string ppf "a frame write is already pending"
  | Aborted -> Fmt.string ppf "the framed stream has been aborted"

module Prefix = struct
  let uint16 =
    Wire.map
      ~decode:(fun value -> Int64.of_int (Wire.UInt16.to_int value))
      ~encode:(fun value -> Wire.UInt16.v (Int64.to_int value))
      Wire.uint16be

  let uint32 =
    Wire.map
      ~decode:(fun value ->
        Int64.logand (Int64.of_int32 (Wire.UInt32.to_int32 value)) 0xffff_ffffL)
      ~encode:(fun value -> Wire.UInt32.of_int (Int64.to_int value))
      Wire.uint32be

  let get = function Uint16_be -> uint16 | Uint32_be -> uint32
end

module Config = struct
  type t = { length_field : length_field; minimum : int; maximum : int }

  let capacity = function Uint16_be -> 65535L | Uint32_be -> 0xffff_ffffL

  let v ?(length_field = Uint32_be) ?(min_length = 0) ?max_length () =
    let capacity =
      min (capacity length_field) (Int64.of_int Sys.max_string_length)
    in
    let maximum =
      Option.value ~default:(Int64.to_int (min capacity 1_048_576L)) max_length
    in
    if min_length < 0 || maximum < min_length || Int64.of_int maximum > capacity
    then
      Fmt.invalid_arg "Framing.Config.v: length range %d-%d outside 0-%Ld"
        min_length maximum capacity;
    { length_field; minimum = min_length; maximum }

  let length_field t = t.length_field
  let min_length t = t.minimum
  let max_length t = t.maximum
  let prefix_length t = Option.get (Wire.size (Prefix.get t.length_field))
  let buffer_size t = t.maximum + prefix_length t
  let equal a b = a = b

  let pp ppf t =
    Fmt.pf ppf "%s length:%d-%d"
      (match t.length_field with
      | Uint16_be -> "uint16be"
      | Uint32_be -> "uint32be")
      t.minimum t.maximum
end

let check_length config length =
  if
    length < Int64.of_int (Config.min_length config)
    || length > Int64.of_int (Config.max_length config)
  then
    Error
      (Invalid_length
         {
           length;
           minimum = Config.min_length config;
           maximum = Config.max_length config;
         })
  else Ok (Int64.to_int length)

let encode config payload =
  match check_length config (Int64.of_int (Cstruct.lenv payload)) with
  | Error error -> Error error
  | Ok length ->
      let prefix =
        Wire.to_string
          (Prefix.get (Config.length_field config))
          (Int64.of_int length)
      in
      Ok (Cstruct.of_string prefix :: payload)

let encode_batch config payloads =
  let rec encode_all acc = function
    | [] -> Ok (List.rev acc)
    | payload :: rest -> (
        match encode config payload with
        | Error error -> Error error
        | Ok frame -> encode_all (frame :: acc) rest)
  in
  match encode_all [] payloads with
  | Error error -> Error error
  | Ok frames -> Ok (List.concat frames)

module Decoder = struct
  type input = Data of string | Eof of string | Abort
  type event = Read of int | Frame of string | End_of_stream | Failed of error
  type after = Continue | End_after | Fail_after of error

  type phase =
    | Prefix of string
    (* [chunks] are the bytes received so far, the latest first. *)
    | Payload of { length : int; received : int; chunks : string list }
    | Ready of string * after
    | Ended
    | Broken of error

  type t = { config : Config.t; phase : phase }

  let v config = { config; phase = Prefix "" }

  let pending_bytes t =
    match t.phase with
    | Prefix bytes | Ready (bytes, _) -> String.length bytes
    | Payload { received; _ } -> received
    | Ended | Broken _ -> 0

  let requested t =
    match t.phase with
    | Prefix bytes -> Config.prefix_length t.config - String.length bytes
    | Payload { length; received; _ } -> length - received
    | Ready _ | Ended | Broken _ -> 0

  let fail t error =
    match t.phase with
    | Ready (frame, Continue) ->
        { t with phase = Ready (frame, Fail_after error) }
    | Ready (_, (End_after | Fail_after _)) -> t
    | Ended | Broken _ -> t
    | Prefix _ | Payload _ -> { t with phase = Broken error }

  let prefix t bytes =
    let length =
      Wire.of_string_exn ~consume:`All
        (Prefix.get (Config.length_field t.config))
        bytes
    in
    match check_length t.config length with
    | Error error -> fail t error
    | Ok 0 -> { t with phase = Ready ("", Continue) }
    | Ok length ->
        { t with phase = Payload { length; received = 0; chunks = [] } }

  let feed t bytes =
    let length = String.length bytes in
    let needed = requested t in
    if length = 0 || length > needed then
      fail t (Invalid_chunk { length; requested = needed })
    else
      match t.phase with
      | Prefix previous ->
          let bytes = previous ^ bytes in
          if String.length bytes = Config.prefix_length t.config then
            prefix t bytes
          else { t with phase = Prefix bytes }
      | Payload { length; received; chunks } ->
          let received = received + String.length bytes in
          let chunks = bytes :: chunks in
          if received = length then
            let frame =
              match chunks with
              | [ bytes ] -> bytes
              | chunks -> String.concat "" (List.rev chunks)
            in
            { t with phase = Ready (frame, Continue) }
          else { t with phase = Payload { length; received; chunks } }
      | Ready _ | Ended | Broken _ -> t

  let eof t =
    match t.phase with
    | Prefix "" -> { t with phase = Ended }
    | Prefix bytes ->
        fail t
          (Truncated
             {
               part = Prefix;
               expected = Config.prefix_length t.config;
               received = String.length bytes;
             })
    | Payload { length; received; _ } ->
        fail t (Truncated { part = Payload; expected = length; received })
    | Ready (frame, Continue) -> { t with phase = Ready (frame, End_after) }
    | Ready (_, (End_after | Fail_after _)) -> t
    | Ended | Broken _ -> t

  let handle t = function
    | Data bytes -> feed t bytes
    | Eof bytes -> eof (if String.length bytes = 0 then t else feed t bytes)
    | Abort -> fail t Aborted

  let next_event t =
    match t.phase with
    | Prefix _ | Payload _ -> (t, Read (requested t))
    | Ended -> (t, End_of_stream)
    | Broken error -> (t, Failed error)
    | Ready (frame, after) ->
        let phase =
          match after with
          | Continue -> Prefix ""
          | End_after -> Ended
          | Fail_after error -> Broken error
        in
        ({ t with phase }, Frame frame)
end

module Encoder = struct
  type completion = Sent | Failed
  type phase = Idle | Writing of int | Closed
  type t = { config : Config.t; phase : phase }

  let v config = { config; phase = Idle }

  let pending_bytes t =
    match t.phase with Writing bytes -> bytes | Idle | Closed -> 0

  let outgoing t payloads =
    match t.phase with
    | Closed -> (t, Error Aborted)
    | Writing _ -> (t, Error Busy)
    | Idle -> (
        match encode_batch t.config payloads with
        | Error error -> (t, Error error)
        | Ok bytes -> ({ t with phase = Writing (Cstruct.lenv bytes) }, Ok bytes)
        )

  let complete t completion =
    match (t.phase, completion) with
    | Writing _, Sent -> { t with phase = Idle }
    | Writing _, Failed -> { t with phase = Closed }
    | (Idle | Closed), _ -> t
end
