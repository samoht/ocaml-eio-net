(** Unix-domain stream sockets. *)

val max_length : int
(** [max_length] is the longest path, in bytes, a Unix-domain socket address
    holds: the size of [sun_path] less its terminating NUL, 103 on macOS and 107
    on Linux. {!connect} reaches a longer path through its directory, but
    bind(2) has no such detour, so {!listen} refuses a longer path with
    [ENAMETOOLONG]. *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  Fpath.t ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [connect ~sw ~net path] connects to the Unix-domain stream socket at [path],
    whatever its length. A path that a socket address holds is connected through
    [net]. A longer one is reached from its directory, opened for the purpose,
    as Git's unix-socket.c reaches it by changing into that directory: on macOS
    that directory is made the working directory of the connecting thread alone
    ([pthread_fchdir_np]), on FreeBSD the connection is made with
    [connectat(2)], and on Linux through the short link
    [/proc/self/fd/<dir>/<name>]. The working directory of the process is never
    changed. The base name must fit an address: {!max_length} bytes on macOS and
    FreeBSD, and on Linux {!max_length} less the length of the
    [/proc/self/fd/<dir>/] prefix; a longer one is refused with [ENAMETOOLONG].
    On a Linux system with no [/proc] mounted, the link does not resolve and a
    longer path fails as a missing socket does, with [Eio.Fs.Not_found]. Other
    systems refuse a longer path with [Eio.Exn.Not_available]. Either way a
    failure is the {!Eio.Io} error {!Eio.Net.connect} raises: a missing socket
    or directory is [Eio.Fs.Not_found], a socket nobody listens on
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
    [lnot perm] for the duration of the bind alone, and calls are serialised; a
    file another thread creates in that interval takes that mask. A socket
    already at [path], left by a listener that has gone or not, is replaced; any
    other file there is kept and the bind fails. Releasing [sw] closes the
    socket and removes [path] while it is still that socket's file. A failure is
    an {!Eio.Io} error, made by {!Eio_unix.Err.v}: a path longer than
    {!max_length} is [ENAMETOOLONG] and a file in the way [EADDRINUSE],
    unclassified. *)
