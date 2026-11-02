open! Hermetic

type length_field = Uint16_be | Uint32_be
type part = Prefix | Payload

type error =
  | Invalid_length of { length : int64; minimum : int; maximum : int }
  | Truncated of { part : part; expected : int; received : int }
  | Aborted

let equal_error a b =
  match (a, b) with
  | Invalid_length a, Invalid_length b ->
      Int64.equal a.length b.length
      && Int.equal a.minimum b.minimum
      && Int.equal a.maximum b.maximum
  | Truncated a, Truncated b ->
      a.part = b.part
      && Int.equal a.expected b.expected
      && Int.equal a.received b.received
  | Aborted, Aborted -> true
  | (Invalid_length _ | Truncated _ | Aborted), _ -> false

let pp_error ppf = function
  | Invalid_length { length; minimum; maximum } ->
      Fmt.pf ppf "frame length %Ld outside %d-%d" length minimum maximum
  | Truncated { part; expected; received } ->
      Fmt.pf ppf "truncated %s: received %d of %d bytes"
        (match part with Prefix -> "length prefix" | Payload -> "payload")
        received expected
  | Aborted -> Fmt.string ppf "the framed stream has been aborted"

exception Error of error

let () =
  Printexc.register_printer (function
    | Error e -> Some (Fmt.str "Eio_net_framing.Error: %a" pp_error e)
    | _ -> None)

module Config = struct
  type t = {
    length_field : length_field;
    minimum : int;
    maximum : int;
    prefix_length : int;
  }

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
      Fmt.invalid_arg
        "Eio_net_framing.Config.v: length range %d-%d outside 0-%Ld" min_length
        maximum capacity;
    let prefix_length =
      match length_field with Uint16_be -> 2 | Uint32_be -> 4
    in
    { length_field; minimum = min_length; maximum; prefix_length }

  let length_field t = t.length_field
  let min_length t = t.minimum
  let max_length t = t.maximum
  let prefix_length t = t.prefix_length
  let buffer_size t = t.maximum + t.prefix_length

  let equal a b =
    a.length_field = b.length_field
    && Int.equal a.minimum b.minimum
    && Int.equal a.maximum b.maximum

  let pp ppf t =
    Fmt.pf ppf "%s length:%d-%d"
      (match t.length_field with
      | Uint16_be -> "uint16be"
      | Uint32_be -> "uint32be")
      t.minimum t.maximum
end

let invalid (config : Config.t) length =
  Invalid_length { length; minimum = config.minimum; maximum = config.maximum }

(* Truncation consumes what the reader holds of the frame, so that it retains
   nothing past the refusal. *)
let truncated r part expected =
  let received = Eio.Buf_read.buffered_bytes r in
  Eio.Buf_read.consume r received;
  raise (Error (Truncated { part; expected; received }))

(* Two [BE.uint16] reads, unlike [BE.uint32], return unboxed integers. The
   uint32 length is composed as an [int64], which stays exact on a platform
   whose [int] has fewer than 33 bits and is not allocated on one that has. *)
let read_length (config : Config.t) r =
  match config.length_field with
  | Uint16_be ->
      let length = Eio.Buf_read.BE.uint16 r in
      if length < config.minimum || length > config.maximum then
        raise (Error (invalid config (Int64.of_int length)));
      length
  | Uint32_be ->
      let high = Eio.Buf_read.BE.uint16 r in
      let low = Eio.Buf_read.BE.uint16 r in
      let length =
        Int64.logor (Int64.shift_left (Int64.of_int high) 16) (Int64.of_int low)
      in
      if
        Int64.compare length (Int64.of_int config.minimum) < 0
        || Int64.compare length (Int64.of_int config.maximum) > 0
      then raise (Error (invalid config length));
      Int64.to_int length

let read (config : Config.t) r =
  (match Eio.Buf_read.ensure r config.prefix_length with
  | () -> ()
  | exception End_of_file ->
      if Eio.Buf_read.buffered_bytes r = 0 then raise End_of_file
      else truncated r Prefix config.prefix_length);
  match read_length config r with
  | 0 -> ""
  | length -> (
      match Eio.Buf_read.take length r with
      | payload -> payload
      | exception End_of_file -> truncated r Payload length)

let rec check (config : Config.t) frames = function
  | [] -> Ok frames
  | payload :: batch ->
      let length = Cstruct.lenv payload in
      if length < config.minimum || length > config.maximum then
        Result.Error (invalid config (Int64.of_int length))
      else check config (frames + 1) batch

let set_prefix (config : Config.t) prefixes off length =
  match config.length_field with
  | Uint16_be -> Cstruct.BE.set_uint16 prefixes off length
  | Uint32_be ->
      Cstruct.BE.set_uint16 prefixes off (length lsr 16);
      Cstruct.BE.set_uint16 prefixes (off + 2) (length land 0xffff)

(* Not tail-recursive, so the buffers come out in order without a reversal;
   the depth is the number of frames in the batch. *)
let rec layout config prefixes off = function
  | [] -> []
  | payload :: batch ->
      let length = Cstruct.lenv payload in
      let size = Config.prefix_length config in
      set_prefix config prefixes off length;
      let rest = layout config prefixes (off + size) batch in
      Cstruct.sub prefixes off size :: List.append payload rest

let encode ?prefixes config batch =
  match check config 0 batch with
  | Error _ as e -> e
  | Ok 0 -> Ok []
  | Ok frames ->
      let size = frames * Config.prefix_length config in
      let prefixes =
        match prefixes with
        | Some buf when Cstruct.length buf >= size -> buf
        | Some _ | None -> Cstruct.create_unsafe size
      in
      Ok (layout config prefixes 0 batch)

let write ?prefixes config sink batch =
  match encode ?prefixes config batch with
  | Error e -> raise (Error e)
  | Ok [] -> ()
  | Ok buffers -> Eio.Flow.write sink buffers
