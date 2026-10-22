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
  val max_length : int
  (** [max_length] is the longest path, in bytes, a Unix-domain socket address
      holds: the size of [sun_path] less its terminating NUL, 103 on macOS and
      107 on Linux. A program that binds a socket, which {!connect}'s detour
      through the directory cannot help, is refused a longer path with
      [ENAMETOOLONG]. *)

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

  val listen :
    sw:Eio.Switch.t ->
    backlog:int ->
    perm:int ->
    Fpath.t ->
    [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t
  (** [listen ~sw ~backlog ~perm path] is a stream socket listening at [path],
      whose file bind(2) makes with the permission bits [perm] and no other, so
      that no peer the mode refuses ever reaches it. The process's umask is
      [lnot perm] for the duration of the bind alone, and calls are serialised;
      a file another thread creates in that interval takes that mask. A socket
      already at [path], left by a listener that has gone or not, is replaced;
      any other file there is kept and the bind fails. Releasing [sw] closes the
      socket and removes [path] while it is still that socket's file. A failure
      is an {!Eio.Io} error, made by {!Eio_unix.Err.v}: a path longer than
      {!max_length} is [ENAMETOOLONG] and a file in the way [EADDRINUSE],
      unclassified. *)
end

(** TCP sockets. *)
module Tcp : sig
  val listen :
    ?reuse_addr:bool ->
    ?reuse_port:bool ->
    sw:Eio.Switch.t ->
    backlog:int ->
    _ Eio.Net.t ->
    Eio.Net.Ipaddr.v4v6 ->
    int ->
    [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t
  (** [listen ~sw ~backlog net ip port] is {!Eio.Net.listen} on
      [`Tcp (ip, port)], but a bind(2) or listen(2) that fails, which Eio's
      POSIX backend raises as [Unix.Unix_error], is the {!Eio.Io} error
      {!Eio_unix.Err.v} makes of it: a port in use is [EADDRINUSE],
      unclassified. *)
end

val connect_failure : exn -> [ `Refused | `Unreachable ] option
(** [connect_failure exn] is the answer a failed {!Eio.Net.connect} raising
    [exn] carries from the network, or [None] when it carries none, as a timeout
    or a local error does:

    - [`Refused] for [Eio.Net.Connection_failure (Refused _)], a reset in answer
      to the SYN (ECONNREFUSED).
    - [`Unreachable] for ENETUNREACH and EHOSTUNREACH, which Eio reports as an
      unclassified {!Eio_unix.Unix_error}: an ICMP destination unreachable, or
      the host's own routing table, says no path to the peer exists. RFC 1122
      section 4.2.3.9 makes it a hard error for an attempt in SYN-SENT.

    Linking this library registers it with {!Eio_net_connect.register}, so a
    connect that asks {!Eio_net_connect.classify} over a Unix network uses it
    with no argument. *)
