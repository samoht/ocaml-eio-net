let max_batch_limit = 64

external send_batch_raw :
  Unix.file_descr -> Cstruct.t array -> int array -> int -> int -> int
  = "caml_eio_net_sendmsg_batch"

external recv_batch_raw :
  Unix.file_descr -> Cstruct.t array -> int array -> int -> int -> int
  = "caml_eio_net_recvmsg_batch"

(* [eio f x] is [f x], a failure of the system the {!Eio.Io} error Eio's own
   sockets raise for it. *)
let eio f x =
  try f x with Unix.Unix_error (e, fn, arg) -> raise (Eio_unix.Err.v e fn arg)

(* [use fd f] is [f] on the descriptor of [fd], which the channel's switch
   closes: once it has, it raises Eio's error for a closed descriptor. *)
let use fd f = Eio_unix.Fd.use_exn "Eio_net_unix.Frame" fd f

let widen_datagram_buffers fd =
  let send = 2 * 1024 * 1024 in
  let recv = send * 2 in
  (try Unix.setsockopt_int fd Unix.SO_SNDBUF send with Unix.Unix_error _ -> ());
  try Unix.setsockopt_int fd Unix.SO_RCVBUF recv with Unix.Unix_error _ -> ()

(* [check_length ~max_length len] raises the {!Eio_net.Datagram.Too_long} error
   if a datagram of [len] bytes is longer than [max_length]. *)
let check_length ~max_length len =
  if len > max_length then raise (Eio_net.Datagram.too_long ~max_length)

(* [last_received lengths] is the index of the last datagram a batch filled
   before it failed: the ones it did not reach have a length of -1. *)
let last_received lengths =
  let max_batch = Array.length lengths in
  let rec received i =
    if i < max_batch && lengths.(i) >= 0 then received (i + 1) else i - 1
  in
  received 1

(* [receive_rest ~failed fd buffers lengths] fills the datagrams of a batch
   after its first, and is the index of the last one filled. A failure after
   the first is kept in [failed] for the next receive. *)
let receive_rest ~failed fd buffers lengths =
  match
    use fd (fun fd ->
        recv_batch_raw fd buffers lengths 1 (Array.length buffers))
  with
  | extra -> extra
  | exception Unix.Unix_error (e, fn, arg) ->
      failed := Some (Eio_unix.Err.v e fn arg);
      last_received lengths

let receiver ~max_length ~max_batch socket fd =
  let check = check_length ~max_length in
  (* One byte of headroom tells a datagram longer than [max_length] from one
     exactly [max_length] long. *)
  let recv_buf = Cstruct.create (max_length + 1) in
  let recv_buffers =
    Array.init max_batch (fun _ -> Cstruct.create (max_length + 1))
  in
  let recv_lengths = Array.make max_batch 0 in
  (* The failure a batch met after its first datagram, which the system
     reports once: the next receive raises it. *)
  let failed = ref None in
  let raise_failed () =
    Option.iter
      (fun e ->
        failed := None;
        raise e)
      !failed
  in
  let receive () =
    raise_failed ();
    let _source, len = Eio.Net.recv socket recv_buf in
    check len;
    len
  in
  let recv () = Cstruct.to_string ~len:(receive ()) recv_buf in
  (* The bytes are fresh, so the slice views them without a second copy. *)
  let recv_slice () =
    Bytesrw.Bytes.Slice.of_bytes_or_eod
      (Cstruct.to_bytes ~len:(receive ()) recv_buf)
  in
  let recv_batch () =
    raise_failed ();
    let _source, first_len = Eio.Net.recv socket recv_buffers.(0) in
    recv_lengths.(0) <- first_len;
    let extra = receive_rest ~failed fd recv_buffers recv_lengths in
    for i = 0 to extra do
      check recv_lengths.(i)
    done;
    List.init (extra + 1) (fun i ->
        Cstruct.to_string ~len:recv_lengths.(i) recv_buffers.(i))
  in
  (recv, recv_slice, recv_batch)

(* In microseconds. *)
let min_backoff = 50
let max_backoff = 10_000

let sender ~max_length ~max_batch fd =
  let send_buffers =
    Array.init max_batch (fun _ -> Cstruct.create max_length)
  in
  let send_lengths = Array.make max_batch 0 in
  let rec copy_frames frames index =
    match frames with
    | [] -> (index, [])
    | _ when index = max_batch -> (index, frames)
    | frame :: rest ->
        let len = Cstruct.lenv frame in
        check_length ~max_length len;
        ignore (Cstruct.fillv ~src:frame ~dst:send_buffers.(index));
        send_lengths.(index) <- len;
        copy_frames rest (index + 1)
  in
  (* Sends the prepared frames from [first] on, waiting for the socket to be
     writable while it accepts none. On ENOBUFS the socket polls writable, so
     the wait is a sleep that doubles from 50 us to 10 ms. *)
  let rec send_prepared ~backoff first count =
    if first < count then
      match
        use fd
          (eio (fun fd ->
               send_batch_raw fd send_buffers send_lengths first count))
      with
      | 0 ->
          use fd Eio_unix.await_writable;
          send_prepared ~backoff:min_backoff first count
      | -1 ->
          Eio_unix.sleep (float backoff *. 1e-6);
          send_prepared ~backoff:(min max_backoff (2 * backoff)) first count
      | sent -> send_prepared ~backoff:min_backoff (first + sent) count
  in
  let rec send_batch = function
    | [] -> ()
    | frames ->
        let count, rest = copy_frames frames 0 in
        send_prepared ~backoff:min_backoff 0 count;
        send_batch rest
  in
  send_batch

let of_connected_datagram_fd ?(max_length = 65536) ?(max_batch = 32) ~sw fd =
  let fail fmt =
    Fmt.invalid_arg ("Eio_net_unix.Frame.of_connected_datagram_fd: " ^^ fmt)
  in
  if max_length <= 0 then fail "max_length is %d, must be positive" max_length;
  if max_batch <= 0 then fail "max_batch is %d, must be positive" max_batch;
  if max_batch > max_batch_limit then
    fail "max_batch is %d, it must be at most %d" max_batch max_batch_limit;
  (* Eio waits for a descriptor to be ready only when an operation on it would
     block; a blocking one would stall every fiber of the domain instead. *)
  eio Unix.set_nonblock fd;
  let socket = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:false fd in
  let fd = Eio_unix.Resource.fd socket in
  (* Receiving and sending have their own buffers: a frame waiting to go out
     is not overwritten by one coming in. *)
  let recv, recv_slice, recv_batch =
    receiver ~max_length ~max_batch socket fd
  in
  let send_batch = sender ~max_length ~max_batch fd in
  (* Receives share their buffers, and so do sends, which wait for the socket
     with a batch half sent: each direction runs one call at a time. *)
  let recv_lock = Eio.Mutex.create () and send_lock = Eio.Mutex.create () in
  let recv () = Eio.Mutex.use_ro recv_lock recv in
  let recv_slice () = Eio.Mutex.use_ro recv_lock recv_slice in
  let recv_batch () = Eio.Mutex.use_ro recv_lock recv_batch in
  let send_batch frames =
    Eio.Mutex.use_ro send_lock (fun () -> send_batch frames)
  in
  Eio_net.Frame.v ~recv_batch ~recv_slice ~send_batch ~recv
    ~send:(fun frame -> send_batch [ frame ])
    ()
