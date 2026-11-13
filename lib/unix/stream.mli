(** Stream writes offloaded to a systhread. *)

val offload_single_write : Eio_unix.Fd.t -> Cstruct.t list -> int
(** [offload_single_write fd bufs] performs one vectored write on a systhread
    through a writev stub that releases the runtime lock, so the calling domain
    keeps running fibers while the kernel copies [bufs] into the socket buffer.
    Semantics match {!Eio.Flow.single_write}: the call returns after writing at
    least one byte, waiting for writability when the socket is full, or [0] at
    once when every buffer is empty, and may consume fewer bytes than offered,
    including fewer buffers, since one call passes at most 256 of its non-empty
    buffers. A failure is the {!Eio.Io} error {!Eio_unix.Err.v} makes of it, as
    Eio's own sockets raise it: a peer that has gone (EPIPE, ECONNRESET) is
    [Eio.Net.Connection_reset]. *)

val offload_writer : _ Eio.Resource.t -> (Cstruct.t list -> int) option
(** [offload_writer flow] is [Some write] when [flow] exposes an OS file
    descriptor, where [write] is {!offload_single_write} on it, and [None]
    otherwise. *)
