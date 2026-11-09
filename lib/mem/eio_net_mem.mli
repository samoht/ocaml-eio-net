(** In-memory {!Eio.Net} network.

    This backend is intended for hermetic tests that need client code and server
    code to talk through the normal {!Eio.Net} APIs without binding OS sockets.
    It supports stream sockets and datagram sockets. *)

type Eio.Exn.Backend.t +=
  | Closed
        (** The backend code carried by the connection reset a mem flow raises,
            where a TCP socket raises one:
            - A write after this end shut its send. An empty write returns 0
              instead, as on Linux; macOS fails it with EPIPE.
            - A write after the peer closed: the first is lost and answered with
              a reset, which every later write raises. On a Unix-domain
              connection the first write raises it, as EPIPE.
            - A read, once the queued bytes are read, and a write after the peer
              closed with bytes of ours unread.
            - A read and a write on a connection its listener closed before
              accepting it.

            The first read or write that raises the reset reports it, as a
            socket clears its pending error: reads after it raise [End_of_file],
            and writes keep raising the reset.

            A read after the peer closed or shut its send otherwise raises
            [End_of_file] once the queued bytes are read. A write after the peer
            shut its receive side returns and its bytes are lost, as on Linux;
            macOS resets the second such write. A read after this end shut its
            receive side raises [End_of_file]. A read, a write or a shutdown on
            a flow after its own close raises [Invalid_argument], as Eio's own
            sockets do. *)
  | No_listener
        (** The backend code carried by the connection failure [connect] raises
            when no listener is bound at a TCP address, or the listener closes
            while the connection waits for it, as a real socket raises
            [Connection_failure (Refused _)], and of the one a datagram [send]
            raises at the path of a closed Unix-domain datagram socket. It is
            also the code of the [Eio.Fs.Not_found] error a [connect] or a
            datagram [send] raises at a Unix path no socket binds, which has no
            file. *)
  | Address_in_use
        (** The backend code of the unclassified {!Eio.Io} error [listen] raises
            at an address a listener holds, and [datagram_socket] at one a
            datagram socket holds, where a real socket's bind fails with
            [EADDRINUSE]. Eio's POSIX backend raises that one as a raw
            [Unix.Unix_error] from [listen], and [Eio_net_unix.Tcp.listen] as an
            {!Eio.Io} error. *)
  | No_destination
        (** The backend code of the unclassified {!Eio.Io} error a datagram
            [send] without a destination raises, as a socket that is not
            connected fails it with [EDESTADDRREQ]. *)
  | Message_too_long
        (** The backend code of the unclassified {!Eio.Io} error a datagram
            [send] raises for a datagram longer than a socket sends, where a
            socket fails with [EMSGSIZE]. *)
  | Invalid_port
        (** The backend code of the unclassified {!Eio.Io} error a datagram
            [send] to UDP port 0 raises, where Linux fails it with [EINVAL]. *)
  | Network_unreachable
        (** The backend code of the unclassified {!Eio.Io} error a [connect]
            bound to an IPv6 address raises at an IPv4-mapped one, and a
            datagram [send] from such a socket, where Linux fails with
            [ENETUNREACH]. *)
  | Address_family
        (** The backend code of the unclassified {!Eio.Io} error a [connect]
            bound to an IPv4-mapped address raises at an IPv6 one, and a
            datagram [send] from such a socket, where a socket fails with
            [EAFNOSUPPORT]. *)
  | Wrong_socket_type
        (** The backend code of the unclassified {!Eio.Io} error a datagram
            [send] raises at the path of a stream socket, and a [connect] at the
            path of a datagram socket, where a socket fails with [EPROTOTYPE].
        *)

val v :
  ?capacity:int ->
  sw:Eio.Switch.t ->
  unit ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~capacity ~sw ()] is a network whose [listen] and [connect] operations
    are paired in memory.

    A connection is closed when the switch passed to [connect] finishes, so the
    accepting end then reads EOF. The two ends of a connection may be used from
    different domains: a write in one wakes a read waiting in the other. Each
    direction of a connection queues at most [capacity] bytes (defaults to no
    bound): a write takes what fits and waits while the queue is full, as a
    socket's send buffer makes it wait, so a test with a small capacity shows a
    peer that writes without reading. Raises [Invalid_argument] if [capacity] is
    not positive.

    A [listen] binds the TCP or Unix stream address it is given. One on TCP port
    [0] binds the lowest port of the dynamic range, 49152-65535 (RFC 6335,
    section 6), that no listener holds, as a socket's bind does, and
    [listening_addr] reports it; it raises [Failure] if every port of the range
    is held. A [listen] at an address a listener holds raises the error that
    {!Address_in_use} marks; a TCP address overlaps an address of the same port
    that is equal to it or is the wildcard address of its family, and the IPv6
    wildcard address overlaps every address of its port, as bind(2) has it on
    Linux for a dual-stack socket. Listeners, datagram sockets and clients bound
    with [bind_to] share one namespace of Unix paths, as they share a file
    system: a path one holds is in use to the others. With [reuse_addr], a
    [listen] at a Unix-domain path takes the path over instead, as Eio's POSIX
    backend removes the socket file there before it binds: the listener that
    held it stays open, and no [connect] reaches it. A [listen] with
    [reuse_port] raises [Invalid_argument]. A listener is closed, and its
    address free again, when it is closed or when the switch passed to [listen]
    or to {!v} finishes; the connections waiting to be accepted are reset with
    it, as a kernel resets them. A [connect] reaches the listener at its
    address, or at the wildcard address of its family and port, or, for an IPv4
    address, at the IPv6 wildcard address of its port, as a dual-stack listener
    takes it. An IPv4-mapped IPv6 address stands for the IPv4 address it maps,
    and a wildcard destination for the loopback address of its family, as both
    kernels route them. It waits while [128] connections wait to be accepted
    there; with no listener, or one that closes while it waits, it raises the
    connection failure that {!No_listener} marks, as a connect to a closed port
    is refused. At a datagram socket's path it raises the error that
    {!Wrong_socket_type} marks. At a Unix path no socket holds it raises
    [Eio.Fs.Not_found]: a listener leaves its file until the switch passed to
    [listen] finishes, as Eio's POSIX backend removes it then: until then, a
    [connect] there is refused and a [listen] there finds it in use. An accepted
    connection is closed when it is closed or when the switch passed to [accept]
    finishes. An accepted connection's peer address is the client's: the
    loopback address its destination reaches, IPv4-mapped for an IPv4 client of
    an IPv6 listener, at a port of the dynamic range no socket holds, counted
    down from its top, which the client holds until its flow closes, or
    [`Unix ""] for a Unix-domain client, which binds no path. Host resolution
    maps ["127.0.0.1"] and ["::1"] with a numeric service to the loopback
    address of their family at that port, once as a stream address and once as a
    datagram address, and ["localhost"] to both families, IPv6 first, as glibc
    and macOS order them. Any other host or service maps to no address.

    [connect], and [setsockopt] on a connection or a listener, accept and ignore
    the socket options whose effect these queues already have: [TCP_NODELAY] and
    [TCP_QUICKACK], which ask that writes not be coalesced and that
    acknowledgements not be delayed, neither of which happens here. They refuse
    every other option with [Invalid_argument], naming it, because ignoring one
    that would change what a caller observes is a wrong answer with no symptom;
    [setsockopt] on a datagram socket refuses every option. A caller cannot tell
    an ignored option from an applied one, so a test that must know an option
    took effect needs a real socket. A [connect] bound with [bind_to] holds that
    address, which the listener sees as its peer's, at a port of the dynamic
    range for port 0; one at an address a listener or another client holds
    raises the error that {!Address_in_use} marks, and one bound to an IPv6
    address connecting to an IPv4-mapped one, or the reverse, raises the error
    that {!Network_unreachable} or {!Address_family} marks; one bound to an
    address of another family, IPv4 for IPv6 or a path for TCP, raises the error
    that {!Address_family} marks, as macOS fails it with EAFNOSUPPORT where
    Linux says EINVAL. [getsockopt] raises [Invalid_argument] on every socket.

    A [datagram_socket] binds the address it is given. [`UdpV4] and [`UdpV6]
    bind the loopback address of their family, and an address with port [0]
    binds its IP address, each at the lowest port of the dynamic range that no
    datagram socket holds. An address that overlaps one a datagram socket holds,
    by the rule [listen] follows, raises the error that {!Address_in_use} marks;
    [reuse_port] raises [Invalid_argument]. A datagram sent to it arrives whole,
    with the sender's address as this socket sees it, the loopback address the
    datagram reached for a wildcard sender and IPv4-mapped for an IPv4 sender to
    an IPv6 socket, after every datagram sent to it before: the network never
    drops, cuts or reorders one, and a send never waits. A datagram reaches a
    socket by the rule a [connect] reaches a listener. One sent to an address no
    socket binds that way is lost, as UDP loses it; one sent to a Unix path no
    socket binds raises [Eio.Fs.Not_found], and one sent to a listener's or a
    client's path raises the error that {!Wrong_socket_type} marks. A closed
    Unix-domain datagram socket leaves its path behind, as Eio's POSIX backend
    leaves the file: a datagram sent there is refused with the connection
    failure that {!No_listener} marks, and a socket bound there raises the error
    that {!Address_in_use} marks. A send without a destination raises the error
    that {!No_destination} marks, since no socket is connected. A datagram
    longer than 65507 bytes to an IPv4 address, 65527 to an IPv6 one, or 212960
    to a Unix path, as Linux's default send buffer allows, raises the error that
    {!Message_too_long} marks. One sent to UDP port 0 raises the error that
    {!Invalid_port} marks, as on Linux; macOS sends it over IPv4. One sent from
    an IPv6 address to an IPv4-mapped one, or the reverse, raises the error that
    {!Network_unreachable} or {!Address_family} marks. One from an IPv4 socket
    to an IPv6 address, or between UDP and a Unix path, raises the error that
    {!Address_family} marks; one from an IPv6 socket to an IPv4 address goes the
    IPv4 way, as on Linux, so it is sent from the wildcard address and refused
    from any other. A [recv] into a buffer shorter than the datagram gets its
    first bytes, as a socket does. The socket is closed, and its address free
    again, when the switch passed to [datagram_socket] finishes; a send or a
    receive on it then raises [Invalid_argument]. *)

val pipe :
  ?capacity:int ->
  unit ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pipe ~capacity ()] is a connected pair of in-memory stream sockets: bytes
    written on one are read on the other, and vice versa, preserving order. Each
    direction queues at most [capacity] bytes, as for {!v}. Unlike {!v} it binds
    no address and needs no [listen] and [connect] handshake: it is the bare
    transport for a test, or a wrapper such as an adversarial flow, that needs
    two ends of a pipe. Its ends take socket options as a connection of {!v}
    does. Blocking reads suspend the calling fiber until data arrives, so both
    ends must run under an Eio event loop; the two ends may be in different
    domains. No switch closes them: the caller does. *)
