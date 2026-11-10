(** Sequences of socket operations, run on any {!Eio.Net.t} and printed as a
    trace: the operations and the outcome of each. The kernel's traces, recorded
    through eio_posix, are the oracle eio-net.mem replays. *)

type transport = Tcp4 | Tcp6 | Unix_stream | Udp4 | Udp6 | Unix_dgram

val transports : transport list
(** Every transport, streams first. *)

val transport_name : transport -> string
(** [transport_name t] names [t], as its trace file does. *)

val transport_of_string : string -> transport
(** [transport_of_string s] is the transport [s] names. Raises
    [Invalid_argument] if it names none. *)

val sequences : transport -> string list list
(** [sequences t] is the sequences of operations on [t] the traces hold, in a
    fixed order: on a stream, every sequence of up to four operations and up to
    three more after a connect and an accept, using each endpoint only once it
    exists; on datagram sockets, every sequence of up to three operations. *)

val run : _ Eio.Net.t -> transport -> string list -> string list
(** [run net t ops] runs [ops] on a fresh listener or pair of datagram sockets
    of [t] and is the outcome of each operation: a result, ["blk"] for one that
    would wait, or the error it raised. Unix paths are relative to the current
    directory. *)

val scenarios : string list
(** [scenarios] names the scenarios the traces hold, in a fixed order: three
    clients of one listener, backlogs, listens, connects and datagrams across
    the loopback addresses, address reuse after a close, Unix paths, and host
    and service resolution. *)

val scenario : [ `Generic ] Eio.Net.ty Eio.Resource.t -> string -> string
(** [scenario net name] runs the scenario [name] on [net] and is its outcome, on
    one line without commas. Unix paths are relative to the current directory.
*)
