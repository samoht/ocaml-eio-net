(** {!Eio_net.Frame} channels over Unix datagram descriptors, with several
    datagrams sent or received in one system call where the system allows. *)

val of_connected_datagram_fd :
  ?max_length:int ->
  ?max_batch:int ->
  sw:Eio.Switch.t ->
  Unix.file_descr ->
  Eio_net.Frame.t
(** [of_connected_datagram_fd ~max_length ~max_batch ~sw fd] is a frame channel
    over the connected Unix datagram descriptor [fd], with one datagram per
    frame, of at most [max_length] bytes (defaults to [65536]). The caller owns
    [fd], which is put in non-blocking mode and stays open when [sw] finishes;
    the channel is usable until then, and a send or a receive raises
    [Invalid_argument] once it has. It takes a [Unix.file_descr], as
    {!Eio_unix.Net.import_socket_datagram} does, because the channel uses [fd]
    until [sw] finishes, longer than {!Eio_unix.Fd.use_exn} lends one.

    On Darwin, {!Eio_net.Frame.send_batch} uses [sendmsg_x] and
    {!Eio_net.Frame.recv_batch} uses [recvmsg_x] to transfer up to [max_batch]
    datagrams (defaults to [32]) in one system call; other systems send and
    receive them one at a time, in order. A received datagram longer than
    [max_length] raises the {!Eio.Io} error {!Eio_net.Datagram.Too_long} and
    drops the datagrams received in the same batch; the channel stays usable.
    Sends copy each frame into a buffer of [max_length] bytes. A longer one
    raises {!Eio_net.Datagram.too_long} before it is sent, and the channel stays
    usable; a send the system refuses raises the {!Eio.Io} error
    {!Eio_unix.Err.v} makes of it. Either way, frames before it in the batch may
    have been sent. A send waits while the socket is full: for writability, or
    on macOS, where a full socket answers [ENOBUFS] and still polls writable,
    for a sleep that doubles from 50 us to 10 ms. A receive the system refuses
    raises that {!Eio.Io} error too; one a batch meets after its first datagram
    is raised by the next receive, and the batch returns the datagrams before
    it.

    Fibers may send and receive at once: concurrent receives run one at a time,
    and so do concurrent sends, a batch to its end, while a send and a receive
    run at the same time.

    @raise Invalid_argument
      if [max_length] or [max_batch] is not positive, or if [max_batch] exceeds
      [64]. *)
