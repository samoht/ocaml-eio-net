open! Hermetic

type t = {
  recv : unit -> string;
  recv_batch : unit -> string list;
  recv_slice : unit -> Bytesrw.Bytes.Slice.t;
  send : Cstruct.t list -> unit;
  send_batch : Cstruct.t list list -> unit;
}

let pp ppf _ = Fmt.string ppf "<frame channel>"

(* A frame the transports of this module receive is a fresh string that
   nothing else holds, so its slice views it in place: a write through the
   slice changes only that frame. A string from a caller's [recv] may be held
   elsewhere, so its slice is a copy. *)
let view s =
  Bytesrw.Bytes.Slice.of_bytes_or_eod (Stdlib_original.Bytes.unsafe_of_string s)

let copy s = Bytesrw.Bytes.Slice.of_bytes_or_eod (Bytes.of_string s)

let make ~slice ~recv ~send =
  {
    recv;
    recv_batch = (fun () -> [ recv () ]);
    recv_slice = (fun () -> slice (recv ()));
    send;
    send_batch = List.iter send;
  }

let v ~recv ~send = make ~slice:copy ~recv ~send

let of_datagram_socket ?max_frame ?dst socket =
  let channel = Datagram.of_socket ?max_frame ?dst socket in
  make ~slice:view
    ~recv:(fun () -> snd (channel.Datagram.recv ()))
    ~send:(channel.Datagram.send None)

exception Error of Framing.error

let of_length_prefixed_flow ?(framing = Framing.Config.v ()) flow =
  let buffered =
    Eio.Buf_read.of_flow ~max_size:(Framing.Config.buffer_size framing) flow
  in
  let decoder = ref (Framing.Decoder.v framing) in
  let encoder = ref (Framing.Encoder.v framing) in
  let read_lock = Eio.Mutex.create () and write_lock = Eio.Mutex.create () in
  let observe needed =
    match Eio.Buf_read.take needed buffered with
    | bytes -> Framing.Decoder.Data bytes
    | exception End_of_file ->
        Framing.Decoder.Eof
          (Eio.Buf_read.take (Eio.Buf_read.buffered_bytes buffered) buffered)
    | exception exn ->
        decoder := Framing.Decoder.handle !decoder Abort;
        raise exn
  in
  let take_frame () =
    Eio.Mutex.use_ro read_lock (fun () ->
        let rec interpret () =
          let state, event = Framing.Decoder.next_event !decoder in
          decoder := state;
          match event with
          | Read needed ->
              decoder := Framing.Decoder.handle !decoder (observe needed);
              interpret ()
          | Frame payload -> payload
          | End_of_stream -> raise End_of_file
          | Failed error -> raise (Error error)
        in
        interpret ())
  in
  let send_batch payloads =
    Eio.Mutex.use_ro write_lock (fun () ->
        let state, result = Framing.Encoder.outgoing !encoder payloads in
        encoder := state;
        match result with
        | Result.Error error -> raise (Error error)
        | Ok encoded -> (
            match Eio.Flow.write flow encoded with
            | () -> encoder := Framing.Encoder.complete !encoder Sent
            | exception exn ->
                encoder := Framing.Encoder.complete !encoder Failed;
                raise exn))
  in
  {
    recv = take_frame;
    recv_batch = (fun () -> [ take_frame () ]);
    recv_slice = (fun () -> view (take_frame ()));
    send = (fun payload -> send_batch [ payload ]);
    send_batch;
  }

let pair () =
  let a_to_b = Eio.Stream.create max_int in
  let b_to_a = Eio.Stream.create max_int in
  let chan recv_stream send_stream =
    let recv () = Eio.Stream.take recv_stream in
    let send bufs = Eio.Stream.add send_stream (Cstruct.copyv bufs) in
    {
      recv;
      recv_batch = (fun () -> [ recv () ]);
      recv_slice = (fun () -> view (recv ()));
      send;
      send_batch = List.iter send;
    }
  in
  (chan b_to_a a_to_b, chan a_to_b b_to_a)
