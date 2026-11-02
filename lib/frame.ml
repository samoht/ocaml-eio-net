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

(* [locked m f x] is [Eio.Mutex.use_ro m (fun () -> f x)] without allocating
   the closure. *)
let locked mutex f x =
  Eio.Mutex.lock mutex;
  match f x with
  | v ->
      Eio.Mutex.unlock mutex;
      v
  | exception exn ->
      Eio.Mutex.unlock mutex;
      raise exn

(* A read that fails after consuming part of a frame leaves the reader inside
   it, and a write that fails may leave part of a batch on the wire: in either
   case the direction refuses further use rather than resynchronise on the
   middle of a frame. A refused batch writes nothing, so it leaves the stream
   usable. *)
let of_length_prefixed_flow ?(framing = Eio_net_framing.Config.v ()) flow =
  let reader =
    Eio.Buf_read.of_flow
      ~max_size:(Eio_net_framing.Config.buffer_size framing)
      flow
  in
  let read_failure = ref None and write_failed = ref false in
  let read_lock = Eio.Mutex.create () and write_lock = Eio.Mutex.create () in
  let read_frame () =
    match !read_failure with
    | Some error -> raise (Eio_net_framing.Error error)
    | None -> (
        let consumed = Eio.Buf_read.consumed_bytes reader in
        match Eio_net_framing.read framing reader with
        | payload -> payload
        | exception (Eio_net_framing.Error error as exn) ->
            read_failure := Some error;
            raise exn
        | exception End_of_file -> raise End_of_file
        | exception exn ->
            if Eio.Buf_read.consumed_bytes reader <> consumed then
              read_failure := Some Eio_net_framing.Aborted;
            raise exn)
  in
  let write_batch payloads =
    if !write_failed then raise (Eio_net_framing.Error Eio_net_framing.Aborted);
    match Eio_net_framing.encode framing payloads with
    | Error error -> raise (Eio_net_framing.Error error)
    | Ok [] -> ()
    | Ok buffers -> (
        match Eio.Flow.write flow buffers with
        | () -> ()
        | exception exn ->
            write_failed := true;
            raise exn)
  in
  let take_frame () = locked read_lock read_frame () in
  let send_batch payloads = locked write_lock write_batch payloads in
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
