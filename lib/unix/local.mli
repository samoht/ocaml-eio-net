(** Unix-domain stream sockets.

    A socket is reached through a capability: {!connect} through the network it
    is handed, as {!Eio.Net.connect} is, and {!connect_at} and {!listen} through
    the directory the {!Eio.Path.t} they are handed is part of, whose descriptor
    resolves the socket's name, so a path of any length works and a confined
    directory confines it. No function here opens a file by a name of its own,
    changes the working directory of the process or its umask. *)

val max_length : int
(** [max_length] is the longest path, in bytes, a Unix-domain socket address
    holds: the size of [sun_path] less its terminating NUL, 103 on macOS and 107
    on Linux. {!connect} refuses a longer path; {!connect_at} and {!listen}
    reach one through its directory. *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  Fpath.t ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [connect ~sw ~net path] is [Eio.Net.connect ~sw net (`Unix path)]: the
    connection [net] makes to the Unix-domain stream socket at [path], and
    nothing else. A path longer than {!max_length} is refused with
    [ENAMETOOLONG], an {!Eio.Io} error made by {!Eio_unix.Err.v}, before [net]
    is asked: {!connect_at} reaches it through its directory. *)

val connect_at :
  sw:Eio.Switch.t ->
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t ->
  _ Eio.Path.t ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [connect_at ~sw ~net path] connects to the Unix-domain stream socket at
    [path], whatever its length. The directory that holds it is opened through
    [path]'s own capability, which refuses a path that leaves a confined
    directory as {!Eio.Path.open_in} does, and the socket is reached by its base
    name from that directory, as Git's unix-socket.c reaches it by changing into
    it: on macOS that directory is made the working directory of the connecting
    thread alone ([pthread_fchdir_np]), on FreeBSD the connection is made with
    [connectat(2)], and on Linux through the short link
    [/proc/self/fd/<dir>/<name>]. [net] is the host's network, whose authority a
    socket of the host needs: an in-memory network does not have the type.

    A base name that is a symbolic link is refused with [Eio.Fs.Symlink], as
    {!Eio.Path.open_in} [~follow:false] refuses one, since its target is not
    under the directory's authority. It is checked before the connect, which
    follows a link, so one who can write the directory can swap a link in
    between and have this process connect, with its own credentials, to a socket
    outside the directory. macOS gives no descriptor of a socket's file to
    connect through, and the window is left open on every system: [connect_at]
    is for a directory no untrusted user writes. The base name must fit an
    address: {!max_length} bytes on macOS and FreeBSD, and on Linux
    {!max_length} less the length of the [/proc/self/fd/<dir>/] prefix; a longer
    one is refused with [ENAMETOOLONG]. On a Linux system with no [/proc]
    mounted, the link does not resolve and the connect fails as a missing socket
    does, with [Eio.Fs.Not_found]. Other systems refuse with [ENOSYS]. A failure
    is the {!Eio.Io} error {!Eio.Net.connect} raises: a missing socket or
    directory is [Eio.Fs.Not_found], a socket nobody listens on
    [Eio.Net.Connection_failure (Refused _)].

    Raises [Invalid_argument] if [path] names no file ([""], ["/"], or a base
    name of ["."] or [".."]) or if its directory has no descriptor of the host,
    as one of a mock file system. *)

val listen :
  ?replace:bool ->
  sw:Eio.Switch.t ->
  net:[> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t ->
  backlog:int ->
  perm:int ->
  _ Eio.Path.t ->
  [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t
(** [listen ~replace ~sw ~net ~backlog ~perm path] is a stream socket listening
    at [path], whatever its length, whose file has the permission bits [perm]
    from the moment a peer can reach it. The directory that holds [path] is
    opened through [path]'s own capability, as for {!connect_at}, and [net] is
    the host's network. The socket is bound and given its mode in a directory of
    its own, made in that one and that no other user can enter, and it listens
    before it is linked to [path]'s base name; the process's umask is neither
    read nor changed. On Linux the bind goes through [/proc], as {!connect_at}
    does.

    A file already at [path] is kept and the listen fails with [EADDRINUSE],
    unclassified, as bind(2) fails. When [replace] is [true] (defaults to
    [false]) a socket there, whether a listener still serves it or not, is
    replaced; a file of any other kind is still kept. Releasing [sw] closes the
    socket and removes [path] while it is still that socket's file. Each is
    checked before the name is replaced or removed, so one who can write the
    directory can swap another file in between, which is then replaced or
    removed in its place. renameat(2) and unlinkat(2) act on the name and follow
    no link, so that is only a name the writer could itself remove or put there:
    no file outside the directory is reached. A failure is an {!Eio.Io} error,
    made by {!Eio_unix.Err.v}.

    {!Eio.Net.listening_addr} is [`Unix p], with [p] the native path of [path]
    ({!Eio.Path.native}), which may be longer than {!max_length}. The kernel
    knows the socket only by the name it was bound at in its private directory,
    so that name, which reaches nothing, is what a client's getpeername(2)
    reports.

    Raises [Invalid_argument] as {!connect_at} does. *)
