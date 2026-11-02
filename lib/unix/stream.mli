(** Stream writes offloaded to a systhread. *)

val offload_single_write : Eio_unix.Fd.t -> Cstruct.t list -> int
(** [offload_single_write fd bufs] performs one vectored write on a systhread
    through a writev stub that releases the runtime lock, so the calling domain
    keeps running fibers while the kernel copies [bufs] into the socket buffer.
    Semantics match {!Eio.Flow.single_write}: the call returns after writing at
    least one byte, waiting for writability when the socket is full, and may
    consume fewer bytes than offered, including fewer buffers, since one call
    passes at most 256 iovecs. A peer that has gone (EPIPE, ECONNRESET) raises
    [Eio.Net.Connection_reset] as an {!Eio.Io} error, and any other failure
    raises [Unix.Unix_error]. *)

val offload_writer : _ Eio.Resource.t -> (Cstruct.t list -> int) option
(** [offload_writer flow] is [Some write] when [flow] exposes an OS file
    descriptor, where [write] is {!offload_single_write} on it, and [None]
    otherwise. *)
