(** In-memory {!Eio.Net} network.

    This backend is intended for hermetic tests that need client code and server
    code to talk through the normal {!Eio.Net} APIs without binding OS sockets.
    It supports stream sockets and datagram sockets. *)

(** {1:errors Errors}

    A bind that fails in [listen] or [datagram_socket] raises the raw
    [Unix.Unix_error] eio_posix raises there, so that one handler serves both:
    [EADDRINUSE], [EADDRNOTAVAIL] or [EINVAL] from ["bind"], and [ENAMETOOLONG]
    with the path for a Unix path [sun_path] cannot hold. Every other error is
    an {!Eio.Io} error, marked by one of these codes. *)

type Eio.Exn.Backend.t +=
  | Closed
        (** The backend code carried by the connection reset a mem flow raises,
            where a TCP socket raises one:
            - A write after this end shut its send. An empty write returns 0
              instead, as on Linux; macOS fails it with EPIPE. On TCP the write
              reports a pending reset, so a read after it returns end of file.
            - A write after the peer closed: the first is lost and answered with
              a reset, which every later write raises; reads keep returning end
              of file, which came first. On a Unix-domain connection the first
              write raises the reset, as EPIPE.
            - A read, once the queued bytes are read, and a write after the peer
              closed with bytes of ours unread.
            - A read and a write on a connection its listener closed before
              accepting it.
            - A write to a TCP peer that shut down both ways: the first is lost
              and resets the connection, as Linux resets it then, so the peer's
              next read raises the reset and every later write does. A write to
              a full queue waits, as Linux holds data the peer's window refuses
              and the peer's reads free no room, until the peer closes.
            - A write on a Unix-domain connection whose peer shut its receive
              side, as EPIPE. One that waits on a full queue raises it once its
              own end shuts its send side or the peer shuts its receive side, as
              a blocking write(2) on Linux and eio_linux do; eio_posix keeps
              waiting, as Linux's poll(2) reports no change then.

            The first read or write that raises the reset reports it, as a
            socket clears its pending error: reads after it raise [End_of_file],
            and writes keep raising the reset. On a Unix-domain connection only
            a read reports it: a write raises the reset as EPIPE and leaves it
            to the next read.

            A read after the peer closed or shut its send otherwise raises
            [End_of_file] once the queued bytes are read. A read after this end
            shut its receive side returns the bytes queued, and raises
            [End_of_file] once none is; macOS drops them. As Linux then
            advertises no more room to the writer, what it reads frees none: a
            writer waiting on a full queue keeps waiting, and the bytes it
            writes into room left at the shutdown arrive and are read too. A
            read into an empty buffer raises [End_of_file], as Eio's backends
            do. On TCP, a write after the peer shut its receive side, and only
            that, is queued and read in the same way. A read, a write or a
            shutdown on a flow after its own close raises [Invalid_argument], as
            Eio's own sockets do. One already in flight finishes against the
            open socket, and the socket closes once none is, as Eio defers a
            descriptor's close to its users: a read waiting across its flow's
            close still gets what the peer writes. *)
  | No_listener
        (** The backend code carried by the connection failure [connect] raises
            when no listener is bound at a TCP address, or the listener closes
            while the connection waits for it, or at a Unix path whose socket
            has closed or that a client is bound at, as a real socket raises
            [Connection_failure (Refused _)], and of the one a datagram [send]
            raises at the path of a closed Unix-domain datagram socket, or at an
            abstract address no socket of its type binds. It is also the code of
            the [Eio.Fs.Not_found] error a [connect] or a datagram [send] raises
            at a Unix path no socket binds, which has no file. *)
  | Address_in_use
        (** The backend code of the unclassified {!Eio.Io} error a [connect]
            bound with [bind_to] raises at an address a socket holds, or at port
            0 when every port of the dynamic range is held, where bind(2) fails
            with [EADDRINUSE]. A [connect] that fails holds the address it was
            bound at until its switch finishes, as eio_posix keeps its socket
            there and Linux its address; macOS frees a TCP one at once. [listen]
            and [datagram_socket] raise that one raw ({!section-errors}). *)
  | No_destination
        (** The backend code of the unclassified {!Eio.Io} error a UDP [send]
            without a destination raises, as a socket that is not connected
            fails it with [EDESTADDRREQ]. *)
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
            [ENETUNREACH]; and of the one a [connect] to a broadcast or
            multicast address raises, which no route reaches. The network has
            the loopback interface only, so a [connect] or a datagram [send] to
            an address off it raises it too, as on a host with loopback only,
            from any source: any but 127.0.0.0/8, [::1], the wildcard addresses
            and their IPv4-mapped forms, IPv4 multicast and broadcast aside. An
            IPv4-compatible address such as [::127.0.0.1] and an IPv6 multicast
            one are off it. A datagram to an IPv4 broadcast or multicast address
            from a socket bound at a wildcard address raises it too, as that
            socket has no route to send it. From an IPv4-mapped address, a
            [connect] or a datagram to an IPv6 address off loopback raises it
            before the error that {!Address_family} marks, as Linux finds no
            route first, and a [connect] to a link-local one the error that
            {!Invalid_address} marks. *)
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
  | Not_connected
        (** The backend code of the unclassified {!Eio.Io} error a Unix-domain
            datagram [send] without a destination raises, as Linux fails it with
            [ENOTCONN]; macOS says [EDESTADDRREQ]. *)
  | Address_not_available
        (** The backend code of the unclassified {!Eio.Io} error a [connect]
            with [bind_to] raises at an IP address no interface has, where
            bind(2) fails with [EADDRNOTAVAIL]; [listen] and [datagram_socket]
            raise that one raw. A [connect] without [bind_to] raises it when
            every port of the dynamic range is held, as connect(2) fails on
            Linux. The network has the loopback interface only: an IPv4 socket
            binds an address of 127.0.0.0/8, the broadcast address or a
            multicast one, an IPv6 one [::1], either the wildcard address of its
            family, and an IPv6 one the IPv4-mapped form of those IPv4
            addresses, as on Linux. *)
  | Name_too_long
        (** The backend code of the unclassified {!Eio.Io} error raised for a
            Unix path of 108 bytes or more, which Linux's [sun_path] cannot hold
            with its NUL, where OCaml's [Unix] fails with [ENAMETOOLONG]: by
            [connect], [bind_to] and a datagram [send]. [listen] and
            [datagram_socket] raise that one raw. *)
  | Invalid_address
        (** The backend code of the unclassified {!Eio.Io} error a [connect]
            raises when its socket cannot bind the address [bind_to] gives: an
            IPv4 one for an IPv6 destination, or an IP one for a Unix path,
            where bind(2) fails with [EINVAL] on Linux, or binds a link-local
            IPv6 address, or a multicast one of interface or link scope, which
            needs a scope that Eio's addresses do not carry, or any IPv6
            multicast one; [listen] and [datagram_socket] raise those raw. It is
            also the code of the one a [connect] raises at a link-local address
            or at the empty Unix path; and of the one a datagram [send] raises
            at an address its socket cannot send to, described in
            {!section-datagrams}. *)
  | Broadcast
        (** The backend code of the [Eio.Fs.Permission_denied] error a datagram
            [send] raises at 127.255.255.255, the broadcast address of
            loopback's network, or at 255.255.255.255 from a socket bound at a
            loopback address, where Linux fails it with [EACCES] as the socket
            has no [SO_BROADCAST]. *)

(** {1 Networks} *)

val v :
  ?capacity:int ->
  ?clock:_ Eio.Time.Mono.t ->
  sw:Eio.Switch.t ->
  unit ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~capacity ~clock ~sw ()] is a network whose [listen] and [connect]
    operations are paired in memory, and whose datagram sockets deliver to each
    other. {!section-streams}, {!section-datagrams} and {!section-paths} say how
    its sockets behave.

    Each direction of a connection queues at most [capacity] bytes: a write
    takes what fits and waits while the queue is full, as a socket's send buffer
    makes it wait, so a test with a small capacity shows a peer that writes
    without reading. It defaults to 4 MiB over TCP and 256 KiB over a Unix path,
    at least what Linux's default buffers hold on loopback: a writer queues
    about 2.6 MiB over TCP before it waits, and at most about 250 KiB over
    AF_UNIX, less in small writes. macOS holds less: about 330 KiB over TCP and
    8 KiB over AF_UNIX. Raises [Invalid_argument] if [capacity] is not positive.

    On [clock], the end of a TCP connection whose FIN went first holds its
    address in TIME_WAIT for 60 seconds, as Linux holds it ({!section-streams}).
    Without a clock there is no TIME_WAIT: an address is free once both ends
    close.

    Host resolution reads numbers as glibc's getaddrinfo(3) does, on a
    dual-stack host with no services database and no name server. The service is
    read first: as strtoul(3) reads it in base 10, the whole string, with
    leading spaces and a sign, the empty one as 0, kept to its low 32 bits and
    taken modulo 65536; a negative one, or any other string, raises
    [Address_lookup_failed SERVICE], service names such as ["http"] included.
    The host is an IPv4 address as inet_aton(3) reads it (["127.1"],
    ["0x7f.1"]), an IPv6 one without brackets, which may name a scope after a
    [%] as glibc takes one, a number of 32 bits or, for a link-local address or
    a multicast one of interface or link scope, ["lo"], the only interface; or
    ["localhost"] in any case, or the empty name, the last two standing for
    [::1] then [127.0.0.1]. Any other name, ["localhost."] included, maps to no
    address, as [/etc/hosts] does not hold it and there is no name server to
    ask, and so do an empty host and service; a host with a name server may
    resolve it. Each address comes once as a stream address and once as a
    datagram one. On a real system the loopback order, and whether IPv6
    addresses come at all, depend on the host's addresses and its [/etc/hosts]:
    glibc leaves them out on a host with no IPv6 address but loopback, and turns
    ["localhost"]'s [::1] into [127.0.0.1]. The order here is glibc's on a
    dual-stack host, [::1] before [127.0.0.1] and each address as a stream one
    before a datagram one, fixed so that a test sees the same addresses
    everywhere; macOS gives the datagram one first.

    [getnameinfo] names [127.0.0.1], IPv4-mapped, IPv4-compatible ([::7f00:1])
    or neither, and [::1] ["localhost"], as [/etc/hosts] does, and gives any
    other address as a number, as glibc gives one that no name server knows: an
    IPv4-compatible one in inet_ntop(3)'s dotted form, as [::0.1.0.0]. The
    service stays a number, ["80"] where glibc says ["http"], as there is no
    services database. The host of a Unix address is ["localhost"], as there is
    no host name, where glibc gives the host's, and its service is its path up
    to the first NUL, as glibc reads a C string: an abstract address has none.
*)

val pipe :
  ?capacity:int ->
  unit ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pipe ~capacity ()] is a connected pair of in-memory stream sockets: bytes
    written on one are read on the other, and vice versa, preserving order. Each
    direction queues at most [capacity] bytes (defaults to 4 MiB), as for {!v}.
    Unlike {!v} it binds no address and needs no [listen] and [connect]
    handshake: it is the bare transport for a test, or a wrapper such as an
    adversarial flow, that needs two ends of a pipe. Its ends take socket
    options as a connection of {!v} does. Blocking reads suspend the calling
    fiber until data arrives, so both ends must run under an Eio event loop; the
    two ends may be in different domains. No switch closes them: the caller
    does. *)

val connection : _ Eio.Net.stream_socket -> int option
(** [connection s] is [Some n] if [s] is an end of a connection of this library,
    made by {!v} or {!pipe}, and [None] otherwise. The two ends of a connection
    have the same [n], and no other connection has it. A wrapper that sees both
    ends, such as a fault injector, pairs them with it. *)

(** {1:streams Streams}

    A [listen] binds the TCP or Unix stream address it is given. One on TCP port
    [0] binds the lowest port of the dynamic range, 49152-65535 (RFC 6335,
    section 6), that no listener holds, as a socket's bind does, and
    [listening_addr] reports it. A [listen] at an address a listener holds, or
    at port [0] when every port of the range is held, raises [EADDRINUSE]
    ({!section-errors}), as bind(2) fails on Linux; a TCP address overlaps an
    address of the same port that is equal to it or is the wildcard address of
    its family, and the IPv6 wildcard address overlaps every address of its
    port, as bind(2) has it on Linux for a dual-stack socket. A [listen] with
    [reuse_port] raises [Invalid_argument]. A listener is closed, and frees its
    own hold on its address, when it is closed or when the switch passed to
    [listen] or to {!v} finishes; the connections waiting to be accepted are
    reset with it, as a kernel resets them. An [accept] or a [listening_addr]
    begun after its listener closed, whoever closed it, raises
    [Invalid_argument], as Eio's own sockets do; one already waiting when it
    closes keeps waiting, as on eio_posix, and the listener stays open, taking
    connections, until that accept returns, as Eio defers a descriptor's close
    to its users.

    A [connect] reaches the listener at its address, or at the wildcard address
    of its family and port, or, for an IPv4 address, at the IPv6 wildcard
    address of its port, as a dual-stack listener takes it. An IPv4-mapped IPv6
    address stands for the IPv4 address it maps, and a wildcard destination for
    the loopback address of its family, as both kernels route them; from a
    socket bound at an IPv4 address other than the wildcard, 0.0.0.0 stands for
    that address, and from an IPv4-mapped one [::] stands for ::ffff:127.0.0.1,
    as Linux routes them. It waits while [backlog + 1] connections wait to be
    accepted there, as Linux's accept queue takes one more than the backlog,
    which it caps at 4096 ([net.core.somaxconn]); with no listener, or one that
    closes while it waits, it raises the connection failure that {!No_listener}
    marks, as a connect to a closed port is refused.

    A connection is closed when the switch passed to [connect] finishes, so the
    accepting end then reads EOF, and an accepted connection when it is closed
    or when the switch passed to [accept] finishes. The two ends of a connection
    may be used from different domains: a write in one wakes a read waiting in
    the other. An accepted connection's peer address is the client's: the
    loopback address of its destination's family, 127.0.0.1 for any address of
    127.0.0.0/8 as Linux picks it, IPv4-mapped for an IPv4 client of an IPv6
    listener, at a port of the dynamic range no socket holds, counted down from
    its top, or [`Unix ""] for a Unix-domain client, which binds no path.

    Each end of a TCP connection holds an address while it is open: the client
    its own, and the accepted end, queued or accepted, the listener's port at
    the address the client reached, as Linux's accepted socket does, even once
    the listener closed. A queued end that its listener's close resets lets go
    at once, as Linux destroys it. Without a clock, or after a reset, each other
    end lets go once both ends have closed. Given a clock, the end whose FIN
    went first, sent by a shutdown of its send side or by its close, holds its
    address once closed in TIME_WAIT, for 60 seconds from the peer's FIN, as
    Linux does; the other end lets go when it closes. Closed before the peer's
    FIN, it waits for that FIN for 60 seconds, Linux's [tcp_fin_timeout], and
    then lets go. A [listen] or a [connect] bound at an address held this way
    raises [EADDRINUSE], or the error that {!Address_in_use} marks, unless both
    have [reuse_addr]: as on Linux, an accepted end has its listener's, a client
    has none, and a listener's own hold refuses every other socket.

    A [connect] bound with [bind_to] at its own destination, where no listener
    is, connects to itself, as Linux's simultaneous open does: it reads what it
    writes, and once closed holds its address in TIME_WAIT.

    A [connect] bound with [bind_to] holds that address, which the listener sees
    as its peer's, at a port of the dynamic range for port 0. One at an address
    a listener or another client holds raises the error that {!Address_in_use}
    marks, a wildcard one by the rule [listen] follows: the IPv6 wildcard
    address finds every hold on its port. Once connected from a wildcard
    address, a client holds the address it sends from, and a later bind at a
    wildcard address of its port finds it in use, as on Linux; one bound to an
    IPv6 address connecting to an IPv4-mapped one, or the reverse, raises the
    error that {!Network_unreachable} or {!Address_family} marks; one bound to
    an address of another family raises, as Linux's bind(2) fails, the error
    that {!Address_family} marks from an IPv4 socket, IPv6 or a path for IPv4,
    and the one that {!Invalid_address} marks from an IPv6 or a Unix-domain
    socket, IPv4 for IPv6 or TCP for a path; macOS says EAFNOSUPPORT for both.

    A connection follows Linux, and macOS differs from it in more than the two
    cases {!Closed} names, the empty write after a shutdown and the bytes read
    after a receive shutdown. Once a peer has closed and a reset has come, macOS
    keeps raising [ECONNRESET] on every read, where Linux reports the reset once
    and then end-of-file, and a read after the peer's close can raise the reset
    where Linux reads end-of-file. Writes after a shutdown of either end differ
    too: an empty write after a reset raises it on macOS and returns 0 on Linux,
    and a write after the peer shut down can succeed on one and fail on the
    other. A test that must hold on macOS needs a real socket there.

    [connect], and [setsockopt] on a connection or a listener, accept and ignore
    the socket options whose effect these queues already have: [TCP_NODELAY] and
    [TCP_QUICKACK], which ask that writes not be coalesced and that
    acknowledgements not be delayed, neither of which happens here. They refuse
    every other option with [Invalid_argument], naming it, because ignoring one
    that would change what a caller observes is a wrong answer with no symptom.
    A caller cannot tell an ignored option from an applied one, so a test that
    must know an option took effect needs a real socket. [getsockopt] raises
    [Invalid_argument] on every socket.

    {1:datagrams Datagrams}

    A [datagram_socket] binds the address it is given. [`UdpV4] and [`UdpV6]
    bind the wildcard address of their family, as a kernel binds an unbound
    socket on its first send, and an address with port [0] binds its IP address,
    each at the lowest port of the dynamic range that no datagram socket holds.
    With every port of the range held, they raise [EADDRINUSE], where Linux
    fails an unbound socket's first send with [EAGAIN]. An address that overlaps
    one a datagram socket holds, by the rule [listen] follows, raises
    [EADDRINUSE] ({!section-errors}), unless both UDP sockets have [reuse_addr].
    Those then share it, as on Linux: a datagram reaches the one bound at the
    most specific address, the address itself, then the IPv4 wildcard, then the
    IPv6 one, and of those bound at the same address the latest, and once it
    closes the one before. [reuse_port] raises [Invalid_argument], and
    [setsockopt] refuses every option. The socket is closed, and its address
    free again, when the switch passed to [datagram_socket] finishes; a send, a
    receive or a shutdown on it then raises [Invalid_argument].

    A datagram reaches a socket by the rule a [connect] reaches a listener, and
    arrives whole, after every datagram sent to it before: the network never
    cuts or reorders one. A socket queues what Linux's default receive buffer
    holds, 229376 bytes on the kernel the tests record, each datagram counted
    with the memory Linux gives it: 199 datagrams of up to about 450 bytes, 99
    of 1000, 3 of 60000. Over UDP a datagram past that is lost, and the send
    returns as on Linux, which frees the memory of the datagrams a receiver
    takes only once they reach a quarter of the buffer, or the receiver took
    every datagram queued when it found none taken. On a Unix path the send
    waits until the receiver takes one, as it does while the receiver holds more
    than 10 datagrams, Linux's [max_dgram_qlen], or while the sender's own
    datagrams not yet received fill its send buffer of the same size: 299 of a
    byte, 100 of 1000 bytes, 12 of 20000, as Linux counts them. It comes with
    the sender's address as the receiver sees it: the loopback address of the
    destination's family for a wildcard sender, 127.0.0.1 for any address of
    127.0.0.0/8, and IPv4-mapped for an IPv4 sender to an IPv6 socket. One sent
    to an address no socket binds is lost, as UDP loses it. A [recv] into a
    buffer shorter than the datagram gets its first bytes, as a socket does.

    After a socket shut its send side, a send raises the connection reset that
    {!Closed} marks, as EPIPE, and so does one to a Unix-domain socket that shut
    its receive side, a send that waits for room included. A socket that shut
    its receive side still receives what is queued and, over UDP, what arrives
    after, as on Linux.

    A send without a destination raises the error that {!No_destination} marks
    over UDP, and {!Not_connected} on a Unix path, as Linux says, since no
    socket is connected. A datagram longer than 65507 bytes to an IPv4 address,
    65527 to an IPv6 one, or 212960 to a Unix path, as Linux's default send
    buffer allows, raises the error that {!Message_too_long} marks. These are
    Linux's limits: by default, macOS refuses a UDP datagram longer than 9216
    bytes and a Unix-domain one longer than 2048. One sent to UDP port 0 raises
    the error that {!Invalid_port} marks, as on Linux; macOS sends it over IPv4.

    A datagram sent from an IPv6 address to an IPv4-mapped one, or the reverse,
    raises the error that {!Network_unreachable} or {!Address_family} marks. One
    from an IPv4 socket to an IPv6 address raises the error that
    {!Address_family} marks, and so does one to a Unix path of 14 bytes or more;
    a shorter path, too short to fill a [sockaddr_in], raises the one that
    {!Invalid_address} marks, and so does a Unix path from an IPv6 socket and a
    UDP address or the empty path from a Unix-domain one, as on Linux. One from
    an IPv6 socket to an IPv4 address goes the IPv4 way, as on Linux: it is
    refused, with the error that {!Network_unreachable} marks, from an IPv6
    address other than the wildcard or an IPv4-mapped one. One from a loopback
    address to an IPv4 address outside 127.0.0.0/8, other than a multicast one,
    raises the error that {!Invalid_address} marks, as Linux cannot route it.

    A datagram that is both too long and refused for its address meets the
    errors in Linux's order. A Unix-domain socket checks the address family
    first. An IPv4 socket refuses a datagram of more than 65535 bytes first, and
    its address before the 65507-byte limit. An IPv6 socket checks the address
    first.

    {1:paths Unix paths}

    Listeners, datagram sockets and clients bound with [bind_to] share one
    namespace of Unix paths, as they share a file system: a path one holds is in
    use to the others. A [connect] at a datagram socket's path raises the error
    that {!Wrong_socket_type} marks, and so does a datagram sent to a listener's
    or a client's path. A [connect] or a datagram sent at a path no socket holds
    raises [Eio.Fs.Not_found].

    The namespace is not a file system: a socket binds a path whose directory
    does not exist, or that it may not write, where a real socket fails with
    [ENOENT] or [EACCES], since modelling them would need the file system the
    path names. A path with a NUL past its first byte names no file, as OCaml's
    [Unix] refuses it: a [listen] or a [datagram_socket] there raises [ENOENT]
    raw, a [connect], a [bind_to] or a datagram [send] [Eio.Fs.Not_found]. As on
    eio_posix, the switch passed to such a [listen] then fails to remove its
    file, with [ENOENT] too. A path of 108 bytes or more raises [ENAMETOOLONG],
    or the error that {!Name_too_long} marks ({!section-errors}).

    With [reuse_addr], a [listen] at a Unix-domain path, not at an abstract
    address, takes the path over, as Eio's backends remove the socket file there
    before they bind: the listener that held it stays open, and no [connect]
    reaches it. A listener leaves its file until the switch passed to [listen]
    finishes, as eio_linux removes it then, and until then a [connect] there is
    refused and a [listen] there finds it in use; eio_posix keeps the file of a
    listener closed by hand.

    A listener, a datagram socket or a client bound with [bind_to] at the empty
    path binds an abstract address of its own, a NUL and five hexadecimal
    digits, as Linux autobinds it: [listening_addr] reports it, an [accept] sees
    it as its client's address, and a reply sent there reaches it. An address
    that starts with a NUL is in Linux's abstract namespace, which has no files:
    a socket there leaves nothing behind when it closes. The namespace is one
    per socket type, as on Linux: a stream socket and a datagram one may bind
    the same abstract address, and a [connect] or a datagram sent where no
    socket of its type binds is refused with the connection failure that
    {!No_listener} marks.

    A closed Unix-domain datagram socket leaves its path behind, as Eio's
    backends leave the file: a datagram sent there is refused with the
    connection failure that {!No_listener} marks, and a socket bound there
    raises [EADDRINUSE], with [reuse_addr] or not, as eio_posix does; eio_linux,
    given [reuse_addr], removes the file and binds. *)
