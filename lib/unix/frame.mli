(** Frame channels over Unix datagram descriptors. *)

val of_connected_datagram_fd :
  sw:Eio.Switch.t ->
  ?max_frame:int ->
  ?max_batch:int ->
  Unix.file_descr ->
  Eio_net.Frame.t
(** [of_connected_datagram_fd ~sw ~max_frame ~max_batch fd] is a frame channel
    over the connected Unix datagram descriptor [fd], with one datagram per
    frame, of at most [max_frame] bytes (defaults to [65536]). The caller owns
    [fd], which is put in non-blocking mode and stays open when [sw] finishes;
    the channel is usable until then.

    On Darwin, {!Eio_net.Frame.send_batch} uses [sendmsg_x] and
    {!Eio_net.Frame.recv_batch} uses [recvmsg_x] to transfer up to [max_batch]
    datagrams (defaults to [32]) in one system call; other systems send and
    receive them one at a time, in order. A received datagram longer than
    [max_frame] raises the {!Eio.Io} error {!Eio_net.Datagram.Too_long} and
    drops the datagrams received in the same batch; the channel stays usable.
    Sends copy each frame into a buffer of [max_frame] bytes and raise
    [Invalid_argument] on a longer one, and on a send the system refuses the
    {!Eio.Io} error {!Eio_unix.Err.v} makes of it; either way, frames before it
    in the batch may have been sent. A receive the system refuses raises that
    {!Eio.Io} error too.

    Fibers may send and receive at once: concurrent receives run one at a time,
    and so do concurrent sends, a batch to its end, while a send and a receive
    run at the same time.

    Raises [Invalid_argument] if [max_frame] or [max_batch] is not positive, or
    if [max_batch] exceeds [64]. *)

val widen_datagram_buffers : Unix.file_descr -> unit
(** [widen_datagram_buffers fd] sets [SO_SNDBUF] to 2 MiB and [SO_RCVBUF] to 4
    MiB on the datagram descriptor [fd]. A kernel may clamp either to its
    configured maximum, and a setting it refuses is left as it was: the call
    never raises. *)
