(** Frame channels over Unix datagram descriptors. *)

val of_connected_datagram_fd :
  sw:Eio.Switch.t ->
  ?max_frame:int ->
  ?max_batch:int ->
  Unix.file_descr ->
  Eio_net.Frame.t
(** [of_connected_datagram_fd ~sw ?max_frame ?max_batch fd] is a frame channel
    over the connected Unix datagram descriptor [fd], with one datagram per
    frame, of at most [max_frame] bytes (defaults to [65536]). The caller owns
    [fd], which is put in non-blocking mode.

    On Darwin, {!Eio_net.Frame.send_batch} uses [sendmsg_x] and
    {!Eio_net.Frame.recv_batch} uses [recvmsg_x] to transfer up to [max_batch]
    datagrams (defaults to [32]) in one system call; other systems send and
    receive them one at a time, in order. A received datagram longer than
    [max_frame] raises [Invalid_argument] and drops the datagrams received in
    the same batch; the channel stays usable.

    One fiber at a time may send and one may receive; a send and a receive may
    run at the same time.

    Raises [Invalid_argument] if [max_frame] or [max_batch] is not positive, or
    if [max_batch] exceeds [64]. *)

val widen_datagram_buffers : Unix.file_descr -> unit
(** [widen_datagram_buffers fd] raises [SO_SNDBUF] to 2 MiB and [SO_RCVBUF] to 4
    MiB on a datagram descriptor. This is best-effort: a kernel may clamp the
    request to its configured maximum. *)
