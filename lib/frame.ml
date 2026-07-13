type t = {
  recv : unit -> string;
  recv_slice : unit -> Bytesrw.Bytes.Slice.t;
  send : Rope.t -> unit;
  send_batch : Rope.t list -> unit;
}

let pp ppf _ = Fmt.string ppf "<frame channel>"

(* A frame's bytes as a view (offset + length into owned bytes) rather than a
   fresh string, so a decode layer strips its header by advancing the offset
   instead of copying. The view is valid until the next recv (bytesrw's slice
   contract): a layer that retains the payload past that must copy it. An empty
   frame yields eod. *)
let slice_of_string s =
  Bytesrw.Bytes.Slice.of_bytes_or_eod (Bytes.unsafe_of_string s)

(* Default batch send: one frame at a time. A transport that can coalesce a
   burst into a single [writev] (software GSO) overrides this. *)
let v ~recv ~send =
  {
    recv;
    recv_slice = (fun () -> slice_of_string (recv ()));
    send;
    send_batch = List.iter send;
  }

(* A rope's leaves map one-to-one onto iovec entries, so a payload carried as a
   zero-copy rope leaf reaches the wire through [writev] without being
   concatenated into a whole-frame string. *)
let rope_cstructs rope = List.map Cstruct.of_string (Rope.to_strings rope)

(* The addressed {!Datagram} channel owns the socket plumbing (and the refusal
   of oversized datagrams); this carrier just drops the source address. Each
   datagram is its own [send] syscall, so [v]'s one-at-a-time batching is
   already the best a datagram socket can do. *)
let of_datagram_socket ?max_frame ?dst socket =
  let channel = Datagram.of_socket ?max_frame ?dst socket in
  v
    ~recv:(fun () -> snd (channel.Datagram.recv ()))
    ~send:(channel.Datagram.send None)

let of_length_prefixed_flow ?(max_frame = 1_048_576) ?(prefix_bytes = 4) flow =
  let buf_r = Eio.Buf_read.of_flow ~max_size:(max_frame + prefix_bytes) flow in
  let get_len s =
    let n = ref 0 in
    for i = 0 to prefix_bytes - 1 do
      n := (!n lsl 8) lor Char.code s.[i]
    done;
    !n
  in
  let take_frame () =
    let hdr = Eio.Buf_read.take prefix_bytes buf_r in
    let len = get_len hdr in
    if len > max_frame then
      Fmt.invalid_arg "Eio_net.Frame: frame length %d out of range (max %d)" len
        max_frame;
    Eio.Buf_read.take len buf_r
  in
  let prefix rope =
    let hdr = Bytes.create prefix_bytes in
    let len = Rope.length rope in
    for i = 0 to prefix_bytes - 1 do
      Bytes.set_uint8 hdr i ((len lsr (8 * (prefix_bytes - 1 - i))) land 0xff)
    done;
    Cstruct.of_bytes hdr
  in
  (* Software GSO: a whole burst of frames -- each its length prefix then its
     rope leaves -- goes out in one [writev] rather than one syscall per
     frame. *)
  let send_batch ropes =
    let iovecs =
      List.concat_map (fun rope -> prefix rope :: rope_cstructs rope) ropes
    in
    if iovecs <> [] then Eio.Flow.write flow iovecs
  in
  {
    recv = take_frame;
    recv_slice = (fun () -> slice_of_string (take_frame ()));
    send = (fun rope -> send_batch [ rope ]);
    send_batch;
  }

let pair () =
  let a_to_b = Eio.Stream.create max_int in
  let b_to_a = Eio.Stream.create max_int in
  let chan recv_stream send_stream =
    let recv () = Eio.Stream.take recv_stream in
    let send r = Eio.Stream.add send_stream (Rope.to_string r) in
    {
      recv;
      recv_slice = (fun () -> slice_of_string (recv ()));
      send;
      send_batch = List.iter send;
    }
  in
  (chan b_to_a a_to_b, chan a_to_b b_to_a)
