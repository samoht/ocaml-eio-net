let max_batch_limit = 64

external send_batch_raw :
  Unix.file_descr -> Cstruct.t array -> int array -> int -> int -> int
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
  (* Eio waits for a descriptor to be ready only when an operation on it would
     block; a blocking one would stall every fiber of the domain instead. *)
  Unix.set_nonblock fd;
  let socket = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:false fd in
  (* One byte of headroom tells a datagram longer than [max_frame] from one
     exactly [max_frame] long. *)
  let check len =
    if len > max_frame then
      raise
        (Eio.Exn.create
           (Eio.Exn.X (Eio_net.Datagram.Too_long { max = max_frame })))
  in
  let recv_buf = Cstruct.create (max_frame + 1) in
  let recv () =
    let _source, len = Eio.Net.recv socket recv_buf in
    check len;
    Cstruct.to_string ~len recv_buf
  in
  (* Receiving and sending have their own buffers: a frame waiting to go out
     is not overwritten by one coming in. *)
  let recv_buffers =
    Array.init max_batch (fun _ -> Cstruct.create (max_frame + 1))
  in
  let recv_lengths = Array.make max_batch 0 in
  let recv_batch () =
    let _source, first_len = Eio.Net.recv socket recv_buffers.(0) in
    recv_lengths.(0) <- first_len;
    let extra = recv_batch_raw fd recv_buffers recv_lengths 1 max_batch in
    for i = 0 to extra do
      check recv_lengths.(i)
    done;
    List.init (extra + 1) (fun i ->
        Cstruct.to_string ~len:recv_lengths.(i) recv_buffers.(i))
  in
  let send_buffers = Array.init max_batch (fun _ -> Cstruct.create max_frame) in
  let send_lengths = Array.make max_batch 0 in
  let rec copy_frames frames index =
    match frames with
    | [] -> (index, [])
    | _ when index = max_batch -> (index, frames)
    | frame :: rest ->
        let len = Cstruct.lenv frame in
        if len > max_frame then
          invalid_arg "Eio_net_unix.Frame: frame exceeds max_frame";
        ignore (Cstruct.fillv ~src:frame ~dst:send_buffers.(index));
        send_lengths.(index) <- len;
        copy_frames rest (index + 1)
  in
  (* Sends the prepared frames from [first] on, waiting for the socket to be
     writable while it accepts none. *)
  let rec send_prepared first count =
    if first < count then begin
      let sent = send_batch_raw fd send_buffers send_lengths first count in
      if sent = 0 then Eio_unix.await_writable fd;
      send_prepared (first + sent) count
    end
  in
  let send_batch frames =
    let rec chunks = function
      | [] -> ()
      | frames ->
          let count, rest = copy_frames frames 0 in
          send_prepared 0 count;
          chunks rest
    in
    chunks frames
  in
  Eio_net.Frame.v ~recv_batch ~send_batch ~recv
    ~send:(fun frame -> send_batch [ frame ])
    ()
