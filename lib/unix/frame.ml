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

let widen_datagram_buffers fd =
  let send = 2 * 1024 * 1024 in
  let recv = send * 2 in
  (try Unix.setsockopt_int fd Unix.SO_SNDBUF send with Unix.Unix_error _ -> ());
  try Unix.setsockopt_int fd Unix.SO_RCVBUF recv with Unix.Unix_error _ -> ()

(* [check_length ~max_frame len] raises the {!Eio_net.Datagram.Too_long} error
   if a datagram of [len] bytes is longer than [max_frame]. *)
let check_length ~max_frame len =
  if len > max_frame then
    raise
      (Eio.Exn.create
         (Eio.Exn.X (Eio_net.Datagram.Too_long { max = max_frame })))

(* [last_received lengths] is the index of the last datagram a batch filled
   before it failed: the ones it did not reach have a length of -1. *)
let last_received lengths =
  let max_batch = Array.length lengths in
  let rec received i =
    if i < max_batch && lengths.(i) >= 0 then received (i + 1) else i - 1
  in
  received 1

(* [receiver ~max_frame ~max_batch socket fd] is the [recv] and [recv_batch]
   of the channel over [fd], which [socket] wraps. *)
let receiver ~max_frame ~max_batch socket fd =
  let check = check_length ~max_frame in
  (* One byte of headroom tells a datagram longer than [max_frame] from one
     exactly [max_frame] long. *)
  let recv_buf = Cstruct.create (max_frame + 1) in
  let recv_buffers =
    Array.init max_batch (fun _ -> Cstruct.create (max_frame + 1))
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
  let recv () =
    raise_failed ();
    let _source, len = Eio.Net.recv socket recv_buf in
    check len;
    Cstruct.to_string ~len recv_buf
  in
  let recv_batch () =
    raise_failed ();
    let _source, first_len = Eio.Net.recv socket recv_buffers.(0) in
    recv_lengths.(0) <- first_len;
    let extra =
      match recv_batch_raw fd recv_buffers recv_lengths 1 max_batch with
      | extra -> extra
      | exception Unix.Unix_error (e, fn, arg) ->
          failed := Some (Eio_unix.Err.v e fn arg);
          last_received recv_lengths
    in
    for i = 0 to extra do
      check recv_lengths.(i)
    done;
    List.init (extra + 1) (fun i ->
        Cstruct.to_string ~len:recv_lengths.(i) recv_buffers.(i))
  in
  (recv, recv_batch)

(* In microseconds. *)
let min_backoff = 50
let max_backoff = 10_000

(* [sender ~max_frame ~max_batch fd] is the [send_batch] of the channel over
   [fd]. *)
let sender ~max_frame ~max_batch fd =
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
     writable while it accepts none. On ENOBUFS the socket polls writable, so
     the wait is a sleep that doubles from 50 us to 10 ms. *)
  let rec send_prepared ~backoff first count =
    if first < count then
      match eio (send_batch_raw fd send_buffers send_lengths first) count with
      | 0 ->
          Eio_unix.await_writable fd;
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

let of_connected_datagram_fd ~sw ?(max_frame = 65536) ?(max_batch = 32) fd =
  if max_frame <= 0 then invalid_arg "Eio_net_unix.Frame: max_frame <= 0";
  if max_batch <= 0 then invalid_arg "Eio_net_unix.Frame: max_batch <= 0";
  if max_batch > max_batch_limit then
    invalid_arg "Eio_net_unix.Frame: max_batch > 64";
  (* Eio waits for a descriptor to be ready only when an operation on it would
     block; a blocking one would stall every fiber of the domain instead. *)
  eio Unix.set_nonblock fd;
  let socket = Eio_unix.Net.import_socket_datagram ~sw ~close_unix:false fd in
  (* Receiving and sending have their own buffers: a frame waiting to go out
     is not overwritten by one coming in. *)
  let recv, recv_batch = receiver ~max_frame ~max_batch socket fd in
  let send_batch = sender ~max_frame ~max_batch fd in
  (* Receives share their buffers, and so do sends, which wait for the socket
     with a batch half sent: each direction runs one call at a time. *)
  let recv_lock = Eio.Mutex.create () and send_lock = Eio.Mutex.create () in
  let recv () = Eio.Mutex.use_ro recv_lock recv in
  let recv_batch () = Eio.Mutex.use_ro recv_lock recv_batch in
  let send_batch frames =
    Eio.Mutex.use_ro send_lock (fun () -> send_batch frames)
  in
  Eio_net.Frame.v ~recv_batch ~send_batch ~recv
    ~send:(fun frame -> send_batch [ frame ])
    ()
