(** Frame channels over Unix datagram descriptors. *)

val of_connected_datagram_fd :
  sw:Eio.Switch.t ->
  ?max_frame:int ->
  ?max_batch:int ->
  Unix.file_descr ->
  Eio_net.Frame.t
(** [of_connected_datagram_fd ~sw fd] presents a connected Unix datagram
    descriptor as a frame channel, with one datagram per frame. The caller owns
    [fd]. On Darwin, {!Eio_net.Frame.send_batch} uses [sendmsg_x] and
    {!Eio_net.Frame.recv_batch} uses [recvmsg_x] to transfer up to [max_batch]
    datagrams in one system call; other systems use ordered per-datagram
    fallbacks. [max_frame] defaults to 65536 and [max_batch] to 32; [max_batch]
    must not exceed 64. Concurrent sends or receives are not supported. *)

val widen_datagram_buffers : Unix.file_descr -> unit
(** [widen_datagram_buffers fd] raises [SO_SNDBUF] to 2 MiB and [SO_RCVBUF] to 4
    MiB on a datagram descriptor. This is best-effort: a kernel may clamp the
    request to its configured maximum. *)
