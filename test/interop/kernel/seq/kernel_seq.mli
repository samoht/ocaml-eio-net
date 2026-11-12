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
    the loopback addresses, address reuse after a close, Unix paths, host and
    service resolution, bind error shapes, autobind, UDP reuse, datagram error
    order, edge addresses, a connect to itself, a listener closed with
    connections queued, Linux's abstract Unix addresses and the empty path,
    [listening_addr] after a close, the names of IPv4-compatible and abstract
    addresses, the address a failed connect keeps bound, a Unix path with a NUL,
    connects bound at the IPv6 wildcard address next to IPv4 holds on their
    port, connects and datagrams off the loopback interface, timed closes, and
    connects that run out of ephemeral ports. *)

val timed : string -> bool
(** [timed name] is [true] for a scenario that lets time pass: TIME_WAIT and
    FIN_WAIT2, probed now and after Linux's 60-second timers. *)

val narrow_ports : string -> bool
(** [narrow_ports name] is [true] for a scenario that runs out of ephemeral
    ports. Only a host whose range is narrowed to a few ports records it, as a
    container with [net.ipv4.ip_local_port_range] set: on another host it would
    take the host's ports. *)

val off_loopback : string -> bool
(** [off_loopback name] is [true] for a scenario that sends toward an address
    off the loopback interface. Only a host with loopback only, as a container
    with no network, records it: on another host it would leave the machine. *)

val scenario :
  ?wait:(float -> unit) ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t ->
  string ->
  string
(** [scenario ~wait net name] runs the scenario [name] on [net] and is its
    outcome, on one line without commas. [wait d] lets [d] seconds pass: a timed
    scenario calls it. Unix paths are relative to the current directory. *)
