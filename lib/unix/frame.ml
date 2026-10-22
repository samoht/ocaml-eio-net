let max_batch_limit = 64

external send_batch_raw :
  Unix.file_descr -> Cstruct.t array -> int array -> int -> int
  = "caml_eio_net_sendmsg_batch"

external recv_batch_raw :
  Unix.file_descr -> Cstruct.t array -> int array -> int -> int -> int
  = "caml_eio_net_recvmsg_batch"

let widen_datagram_buffers fd =
  let send = 2 * 1024 * 1024 in
  let recv = send * 2 in
  (try Unix.setsockopt_int fd Unix.SO_SNDBUF send with Unix.Unix_error _ -> ());
  try Unix.setsockopt_int fd Unix.SO_RCVBUF recv with Unix.Unix_error _ -> ()

let of_connected_datagram_fd ~sw ?(max_frame = 65536) ?(max_batch = 32) fd =
  if max_frame <= 0 then invalid_arg "Eio_net_unix.Frame: max_frame <= 0";
  if max_batch <= 0 then invalid_arg "Eio_net_unix.Frame: max_batch <= 0";
  if max_batch > max_batch_limit then
    invalid_arg "Eio_net_unix.Frame: max_batch > 64";
  let socket = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:false fd in
  let recv_buf = Cstruct.create max_frame in
  let recv () =
    let _source, len = Eio.Net.recv socket recv_buf in
    Cstruct.to_string ~len recv_buf
  in
  let buffers = Array.init max_batch (fun _ -> Cstruct.create max_frame) in
  let lengths = Array.make max_batch 0 in
  let recv_batch () =
    let _source, first_len = Eio.Net.recv socket buffers.(0) in
    lengths.(0) <- first_len;
    let extra = recv_batch_raw fd buffers lengths 1 max_batch in
    List.init (extra + 1) (fun i ->
        Cstruct.to_string ~len:lengths.(i) buffers.(i))
  in
  let rec copy_frames frames index =
    match frames with
    | [] -> (index, [])
    | _ when index = max_batch -> (index, frames)
    | frame :: rest ->
        let len = Rope.length frame in
        if len > max_frame then
          invalid_arg "Eio_net_unix.Frame: frame exceeds max_frame";
        ignore (Rope.blit_to_cstruct frame buffers.(index));
        lengths.(index) <- len;
        copy_frames rest (index + 1)
  in
  let rec send_prepared count =
    let sent = send_batch_raw fd buffers lengths count in
    if sent = 0 then (
      Eio.Fiber.yield ();
      send_prepared count)
    else if sent < count then begin
      Array.blit buffers sent buffers 0 (count - sent);
      Array.blit lengths sent lengths 0 (count - sent);
      send_prepared (count - sent)
    end
  in
  let send_batch frames =
    let rec chunks = function
      | [] -> ()
      | frames ->
          let count, rest = copy_frames frames 0 in
          send_prepared count;
          chunks rest
    in
    chunks frames
  in
  let base = Eio_net.Frame.v ~recv ~send:(fun frame -> send_batch [ frame ]) in
  { base with Eio_net.Frame.recv_batch; send_batch }
