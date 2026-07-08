type t = { recv : unit -> string; send : Rope.t -> unit }

let pp ppf _ = Fmt.string ppf "<frame channel>"
let v ~recv ~send = { recv; send }

(* One writev of the rope's leaves: each leaf goes straight into an iovec entry,
   so a payload carried as a zero-copy rope leaf is never concatenated into a
   whole-frame string on the way to the wire. *)
let rope_cstructs rope = List.map Cstruct.of_string (Rope.to_strings rope)

let of_datagram_socket ?(max_frame = 65536) ?dst socket =
  let buf = Cstruct.create max_frame in
  let recv () =
    let _src, n = Eio.Net.recv socket buf in
    Cstruct.to_string (Cstruct.sub buf 0 n)
  in
  let send rope =
    let bufs = rope_cstructs rope in
    match dst with
    | Some dst -> Eio.Net.send socket ~dst bufs
    | None -> Eio.Net.send socket bufs
  in
  { recv; send }

let of_length_prefixed_flow ?(max_frame = 1_048_576) flow =
  let buf_r = Eio.Buf_read.of_flow ~max_size:(max_frame + 4) flow in
  let recv () =
    let hdr = Eio.Buf_read.take 4 buf_r in
    let len = Int32.to_int (String.get_int32_be hdr 0) in
    if len < 0 || len > max_frame then
      Fmt.invalid_arg "Eio_net.Frame: frame length %d out of range (max %d)" len
        max_frame;
    Eio.Buf_read.take len buf_r
  in
  let send rope =
    let hdr = Bytes.create 4 in
    Bytes.set_int32_be hdr 0 (Int32.of_int (Rope.length rope));
    Eio.Flow.write flow (Cstruct.of_bytes hdr :: rope_cstructs rope)
  in
  { recv; send }

let pair () =
  let a_to_b = Eio.Stream.create max_int in
  let b_to_a = Eio.Stream.create max_int in
  let a =
    {
      recv = (fun () -> Eio.Stream.take b_to_a);
      send = (fun r -> Eio.Stream.add a_to_b (Rope.to_string r));
    }
  in
  let b =
    {
      recv = (fun () -> Eio.Stream.take a_to_b);
      send = (fun r -> Eio.Stream.add b_to_a (Rope.to_string r));
    }
  in
  (a, b)
