(** Unix adapters for frame channels and stream writes. *)

module Frame : sig
  val of_connected_datagram_fd :
    sw:Eio.Switch.t ->
    ?max_frame:int ->
    ?max_batch:int ->
    Unix.file_descr ->
    Eio_net.Frame.t
  (** [of_connected_datagram_fd ~sw fd] presents a connected Unix datagram
      descriptor as a frame channel, with one datagram per frame. The caller
      owns [fd]. On Darwin, {!Eio_net.Frame.send_batch} uses [sendmsg_x] and
      {!Eio_net.Frame.recv_batch} uses [recvmsg_x] to transfer up to [max_batch]
      datagrams in one system call; other systems use ordered per-datagram
      fallbacks. [max_frame] defaults to 65536 and [max_batch] to 32;
      [max_batch] must not exceed 64. Concurrent sends or receives are not
      supported. *)

  val widen_datagram_buffers : Unix.file_descr -> unit
  (** [widen_datagram_buffers fd] raises [SO_SNDBUF] to 2 MiB and [SO_RCVBUF] to
      4 MiB on a datagram descriptor. This is best-effort: a kernel may clamp
      the request to its configured maximum. *)
end

module Stream : sig
  val offload_single_write : Eio_unix.Fd.t -> Cstruct.t list -> int
  (** [offload_single_write fd bufs] performs one vectored write on a systhread
      through a writev stub that releases the runtime lock, so the calling
      domain keeps running fibers while the kernel copies [bufs] into the socket
      buffer. Semantics match {!Eio.Flow.single_write}: the call returns after
      writing at least one byte, waiting for writability when the socket is
      full, and may consume fewer bytes than offered, including fewer buffers,
      since one call passes at most 256 iovecs. *)

  val offload_writer : _ Eio.Resource.t -> (Cstruct.t list -> int) option
  (** [offload_writer flow] is [Some write] when [flow] exposes an OS file
      descriptor, where [write] is {!offload_single_write} on it, and [None]
      otherwise. *)
end

(** Unix-domain stream sockets. *)
module Local : sig
  val connect :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    Fpath.t ->
    [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  (** [connect ~sw ~net path] connects to the Unix-domain stream socket at
      [path], whatever its length. A path that a socket address holds is
      connected through [net]. A longer one is reached from its directory,
      opened for the purpose, as Git's unix-socket.c reaches it by changing into
      that directory: on macOS that directory is made the working directory of
      the connecting thread alone ([pthread_fchdir_np]), on FreeBSD the
      connection is made with [connectat(2)], and on Linux through the short
      link [/proc/self/fd/<dir>/<name>]. The working directory of the process is
      never changed, and the base name must fit an address. Either way a failure
      is the {!Eio.Io} error {!Eio.Net.connect} raises: a missing socket or
      directory is [Eio.Fs.Not_found], a socket nobody listens on
      [Eio.Net.Connection_failure (Refused _)]. *)
end
