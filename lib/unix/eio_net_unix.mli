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
