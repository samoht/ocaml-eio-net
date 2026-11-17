(** In-memory {!Eio.Net} network.

    For hermetic tests: client and server code talk through {!Eio.Net} without
    binding OS sockets, over stream and datagram sockets.

    Where Linux and macOS differ, the network follows one kernel, chosen by
    [?os]: [`Linux], the default, or [`Macos]. The choice comes from the
    argument, never from the host, so a test sees the same network on every
    machine. {!section-macos} lists what [`Macos] changes. *)

(** {1:errors Errors}

    A bind that fails in [listen] or [datagram_socket] raises the {!Eio.Io}
    error eio_posix raises there, carrying [Eio_unix.Unix_error], so that one
    handler serves both: [EADDRINUSE], [EADDRNOTAVAIL] or [EINVAL] from
    ["bind"], and [ENAMETOOLONG] with the path for a Unix path [sun_path] cannot
    hold. A socket option the kernel refuses raises the error eio_posix raises
    ({!section-options}). Every other error is an {!Eio.Io} error marked by one
    of these codes, unclassified unless the code's doc names its class. Below,
    "raises {!Closed}" means raises the {!Eio.Io} error marked by {!Closed}, and
    likewise for each code. *)

type Eio.Exn.Backend.t +=
  | Closed
        (** The backend code carried by the connection reset a mem flow raises,
            where a TCP socket raises one:
            - A write after this end shut its send, an empty one included, as
              eio_posix sends it with sendmsg(2). On TCP the write reports a
              pending reset, so a read after it returns end of file.
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
            [End_of_file] once none is; under [`Macos] it drops them and raises
            [End_of_file] at once. As Linux then advertises no more room to the
            writer, what it reads frees none: a writer waiting on a full queue
            keeps waiting, and the bytes it writes into room left at the
            shutdown arrive and are read too. A read into an empty buffer raises
            [End_of_file], as Eio's backends do. On TCP, a write after the peer
            shut its receive side, and only that, is queued and read in the same
            way. A read, a write or a shutdown on a flow after its own close
            raises [Invalid_argument], as Eio's own sockets do. One already in
            flight finishes against the open socket, and the socket closes once
            none is, as Eio defers a descriptor's close to its users: a read
            waiting across its flow's close still gets what the peer writes. *)
  | No_listener
        (** The backend code carried by the connection failure [connect] raises
            when no listener is bound at a TCP address, or the listener closes
            while the connection waits for it, or at a Unix path a client is
            bound at, as a real socket raises [Connection_failure (Refused _)],
            and of the one a datagram [send] raises at an abstract address no
            socket of its type binds. It is also the code of the
            [Eio.Fs.Not_found] error a [connect] or a datagram [send] raises at
            a Unix path no socket binds, which has no file. *)
  | Address_in_use
        (** The code of the error a [connect] bound with [bind_to] raises at an
            address a socket holds, or at port 0 when every port of the dynamic
            range is held, where bind(2) fails with [EADDRINUSE]. A [connect]
            that fails holds the address it was bound at until its switch
            finishes, as eio_posix keeps its socket there and Linux its address;
            under [`Macos] a TCP one is free at once, as macOS frees it.
            [listen] and [datagram_socket] raise eio_posix's error instead
            ({!section-errors}). *)
  | No_destination
        (** The code of the error a UDP [send] without a destination raises, as
            a socket that is not connected fails it with [EDESTADDRREQ]. *)
  | Message_too_long
        (** The code of the error a datagram [send] raises for a datagram longer
            than a socket sends, where a socket fails with [EMSGSIZE]. *)
  | Invalid_port
        (** The code of the error a datagram [send] to UDP port 0 raises, where
            Linux fails it with [EINVAL]. *)
  | Network_unreachable
        (** The code of the error a [connect] bound to an IPv6 address raises at
            an IPv4-mapped one, and a datagram [send] from such a socket, where
            Linux fails with [ENETUNREACH]; and of the one a [connect] to a
            broadcast or multicast address raises, which no route reaches. The
            network has the loopback interface only, so a [connect] or a
            datagram [send] to an address off it raises it too, as on a host
            with loopback only, from any source: any but 127.0.0.0/8, [::1], the
            wildcard addresses and their IPv4-mapped forms, IPv4 multicast and
            broadcast aside. An IPv4-compatible address such as [::127.0.0.1]
            and an IPv6 multicast one are off it. A datagram to an IPv4
            broadcast or multicast address from a socket bound at a wildcard
            address raises it too, as that socket has no route to send it. From
            an IPv4-mapped address, a [connect] or a datagram to an IPv6 address
            off loopback raises it before {!Address_family}, as Linux finds no
            route first, and a [connect] to a link-local one raises
            {!Invalid_address}. *)
  | Address_family
        (** The code of the error a [connect] bound to an IPv4-mapped address
            raises at an IPv6 one, and a datagram [send] from such a socket,
            where a socket fails with [EAFNOSUPPORT]. *)
  | Wrong_socket_type
        (** The code of the error a datagram [send] raises at the path of a
            stream socket, and a [connect] at the path of a datagram socket,
            where a socket fails with [EPROTOTYPE]. *)
  | Not_connected
        (** The code of the error a Unix-domain datagram [send] without a
            destination raises, as Linux fails it with [ENOTCONN]. Under
            [`Macos] the send raises {!No_destination}, as macOS says
            [EDESTADDRREQ]. *)
  | Address_not_available
        (** The code of the error a [connect] with [bind_to] raises at an IP
            address no interface has, where bind(2) fails with [EADDRNOTAVAIL];
            [listen] and [datagram_socket] raise eio_posix's. A [connect]
            without [bind_to] raises it when no port of the dynamic range is
            free toward its destination ({!section-streams}), as connect(2)
            fails on Linux. The network has the loopback interface only: an IPv4
            socket binds an address of 127.0.0.0/8, the broadcast address or a
            multicast one, an IPv6 one [::1], either the wildcard address of its
            family, and an IPv6 one the IPv4-mapped form of those IPv4
            addresses, as on Linux. *)
  | Name_too_long
        (** The code of the error raised for a Unix path of 108 bytes or more,
            which Linux's [sun_path] cannot hold with its NUL, where OCaml's
            [Unix] fails with [ENAMETOOLONG]: by [connect], [bind_to] and a
            datagram [send]. [listen] and [datagram_socket] raise eio_posix's.
        *)
  | Invalid_address
        (** The code of the error a [connect] raises when its socket cannot bind
            the address [bind_to] gives: an IPv4 one for an IPv6 destination, or
            an IP one for a Unix path, where bind(2) fails with [EINVAL] on
            Linux, or binds a link-local IPv6 address, or a multicast one of
            interface or link scope, which needs a scope that Eio's addresses do
            not carry, or any IPv6 multicast one; [listen] and [datagram_socket]
            raise eio_posix's. It is also the code of the one a [connect] raises
            at a link-local address or at the empty Unix path; and of the one a
            datagram [send] raises at an address its socket cannot send to,
            described in {!section-datagrams}. *)
  | Broadcast
        (** The backend code of the [Eio.Fs.Permission_denied] error a datagram
            [send] raises at 127.255.255.255, the broadcast address of
            loopback's network, or at 255.255.255.255 from a socket bound at a
            loopback address, where Linux fails it with [EACCES] as the socket
            has no [SO_BROADCAST]. The network refuses it whatever
            [SO_BROADCAST] says. *)
  | Host_unreachable
        (** The code of the error raised under [`Macos] where macOS finds no
            route to a host, with [EHOSTUNREACH]: a [connect] to a link-local
            address, and the datagram [send]s {!section-datagrams} lists. *)

(** {1:networks Networks} *)

val v :
  ?os:[< `Linux | `Macos ] ->
  ?capacity:int ->
  ?mono:_ Eio.Time.Mono.t ->
  sw:Eio.Switch.t ->
  unit ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~os ~capacity ~mono ~sw ()] is a network following the kernel [os]
    (defaults to [`Linux]) whose [listen] and [connect] operations are paired in
    memory, and whose datagram sockets deliver to each other.
    {!section-streams}, {!section-datagrams} and {!section-paths} say how its
    sockets behave. {!section-resolution} says how [getaddrinfo] and
    [getnameinfo] answer.

    Each direction of a connection queues at most [capacity] bytes: a write
    takes what fits and waits while the queue is full, as a socket's send buffer
    makes it wait, so a test with a small capacity shows a peer that writes
    without reading. It defaults to 4 MiB over TCP and 256 KiB over a Unix path,
    at least what Linux's default buffers hold on loopback: a writer queues
    about 2.6 MiB over TCP before it waits, and at most about 250 KiB over
    AF_UNIX, less in small writes. Under [`Macos] it defaults to 1 MiB over TCP,
    what macOS's buffers grow to on loopback from about 330 KiB, and 8 KiB over
    AF_UNIX, as macOS holds. A [capacity] is what every connection queues, over
    TCP and over a Unix path, above the default too, and it bounds what a
    socket's send and receive buffers give ({!section-options}).

    On [mono], the end of a TCP connection whose FIN went first holds its
    address in TIME_WAIT for 60 seconds, as Linux holds it ({!section-streams}),
    or 30 seconds under [`Macos]. Without a clock there is no TIME_WAIT: an
    address is free once both ends close.

    On [mono], each end of a TCP connection gone silent ({!Private.silence})
    gives up on its peer as Linux does, measured over loopback. With
    [SO_KEEPALIVE] and no byte unacknowledged, it probes once [TCP_KEEPIDLE] has
    passed since the last segment it received. Its probes go [TCP_KEEPINTVL]
    apart, and it gives up one interval after the [TCP_KEEPCNT]th, or with
    [TCP_USER_TIMEOUT] at the first probe that long after that segment. A byte
    it sent unacknowledged stops the probes, and ends the connection
    [TCP_USER_TIMEOUT] later, or 948.6 seconds later, as [tcp_retries2] at 15
    does. The first read or write then raises [ETIMEDOUT]; later reads return
    end-of-file and writes raise [EPIPE]. With no keepalive and nothing
    unacknowledged, or without a clock, a wait on it lasts for ever.

    @raise Invalid_argument if [capacity] is not positive. *)

val pair :
  ?os:[< `Linux | `Macos ] ->
  ?capacity:int ->
  unit ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~os ~capacity ()] is a connected pair of in-memory stream sockets,
    following the kernel [os] (defaults to [`Linux]): bytes written on one are
    read on the other, and vice versa, preserving order. Each direction queues
    at most [capacity] bytes (defaults to 4 MiB), as for {!v}. Unlike {!v} it
    binds no address and needs no [listen] and [connect] handshake: it is the
    bare transport for a test, or a wrapper such as an adversarial flow, that
    needs two connected ends. Its ends take socket options as a connection of
    {!v} does. Blocking reads suspend the calling fiber until data arrives, so
    both ends must run under an Eio event loop; the two ends may be in different
    domains. No switch closes them: the caller does.

    @raise Invalid_argument if [capacity] is not positive. *)

(** {1:streams Streams}

    Stream sockets follow Linux, and Eio's own sockets where Eio decides: a use
    after close raises [Invalid_argument], and a close waits for the calls
    already in flight. {!section-macos} lists what [`Macos] changes.

    {2:listening Listening}

    - [listen] binds the TCP or Unix stream address it is given.
    - [listen] at TCP port [0] binds the lowest port of the dynamic range,
      49152-65535 (RFC 6335, section 6), that no listener holds.
      [listening_addr] reports it.
    - [listen] at an address a listener holds, or at port [0] with every port of
      the range held, raises [EADDRINUSE].
    - Two TCP addresses of one port overlap when they are equal or one is the
      wildcard address of the other's family. The IPv6 wildcard address overlaps
      every address of its port, as on a dual-stack socket.
    - [listen] with [reuse_port] raises [Invalid_argument].
    - Closing a listener, or finishing the switch passed to [listen] or to {!v},
      closes it and frees its own hold on its address. The connections waiting
      to be accepted are reset.
    - [accept] or [listening_addr] begun after the listener closed, whoever
      closed it, raises [Invalid_argument].
    - An [accept] already waiting when the listener closes keeps waiting, as on
      eio_posix. The listener stays open, and takes connections, until that
      [accept] returns.

    {2:connecting Connecting}

    - [connect] reaches the listener at its address, at the wildcard address of
      its family and port, or, for an IPv4 address, at the IPv6 wildcard address
      of its port, as a dual-stack listener takes it.
    - An IPv4-mapped IPv6 destination stands for the IPv4 address it maps, and a
      wildcard destination for the loopback address of its family, on both
      kernels.
    - From a socket bound at an IPv4 address other than the wildcard, the
      destination 0.0.0.0 stands for that address. From an IPv4-mapped one, [::]
      stands for ::ffff:127.0.0.1.
    - While [backlog + 1] connections wait to be accepted, [connect] waits. The
      backlog is capped at 4096 ([net.core.somaxconn]).
    - With no listener, or with one that closes while [connect] waits, [connect]
      raises {!No_listener}.

    {2:connections Connections}

    - Finishing the switch passed to [connect] closes the client end, and the
      accepted end then reads end of file.
    - Closing an accepted connection, or finishing the switch passed to
      [accept], closes it.
    - The two ends may be used from different domains: a write in one wakes a
      read waiting in the other.
    - An accepted connection's peer address is the client's. It is the loopback
      address of its destination's family (127.0.0.1 for any address of
      127.0.0.0/8), IPv4-mapped for an IPv4 client of an IPv6 listener, at a
      port of the dynamic range. A Unix-domain client binds no path and is
      [`Unix ""].

    {2:held Held addresses}

    - While it is open, each end of a TCP connection holds an address. The
      client holds its own. The accepted end, queued or accepted, holds the
      listener's port at the address the client reached, even once the listener
      closed.
    - The accepted end of a listener at a wildcard address holds that address as
      a client bound there does, so a later bind at a wildcard address of its
      port finds it in use.
    - A queued end that its listener's close resets lets go at once.
    - Without a clock, or after a reset, any other end lets go once both ends
      have closed.
    - With a clock, the end whose FIN went first, by a shutdown of its send side
      or by its close, holds its address in TIME_WAIT once closed, for 60
      seconds from the peer's FIN. The other end lets go when it closes.
    - With a clock, an end closed before the peer's FIN waits for that FIN for
      its [TCP_LINGER2] if one is set, or else 60 seconds ([tcp_fin_timeout]),
      and then lets go.
    - [listen] at an address held this way raises [EADDRINUSE], and [connect]
      bound there raises {!Address_in_use}, unless both sockets have
      [reuse_addr]. An accepted end has its listener's [reuse_addr], a client
      has none, and a listener's own hold refuses every other socket.

    {2:ports Ports of a connect}

    - [connect] without [bind_to] takes the next port down from the last one
      such a [connect] took. It starts at the top of the dynamic range and
      starts there again once past the bottom.
    - It skips a port that a listener or a [connect] with [bind_to] holds, open
      or in TIME_WAIT, and a port that another connection holds toward the same
      destination address and port. Connections to different destinations thus
      share ports: the range bounds the connections to each destination, not
      their sum.
    - With a clock, a port in TIME_WAIT toward the same destination is free for
      it again once more than a second has passed since the peer's FIN, as
      [net.ipv4.tcp_tw_reuse] has it by default on loopback. A port in FIN_WAIT2
      is not.
    - With no port left, [connect] raises {!Address_not_available}.

    {2:bind_to Connecting from a bound address}

    - [connect] with [bind_to] at its own destination, where no listener is,
      connects to itself (a simultaneous open). It reads what it writes, and
      once closed holds its address in TIME_WAIT.
    - [connect] with [bind_to] holds that address, which the listener sees as
      its peer's. Port 0 takes the highest port of the dynamic range that
      nothing holds, as a bind(2) before a connect(2) does.
    - [bind_to] at an address a listener or another client holds raises
      {!Address_in_use}. A wildcard address overlaps as for [listen], so the
      IPv6 wildcard address finds every hold on its port.
    - A client connected from a wildcard address holds the address it sends
      from, so a later bind at a wildcard address of its port finds it in use.
    - [bind_to] at an IPv6 address with an IPv4-mapped destination raises
      {!Network_unreachable}. [bind_to] at an IPv4-mapped address with an IPv6
      destination raises {!Address_family}.
    - [bind_to] of another family than the destination raises {!Address_family}
      for an IPv4 destination, with an IPv6 address or a path. It raises
      {!Invalid_address} for an IPv6 destination with an IPv4 address, and for a
      Unix path with an IP address. Under [`Macos] each of these raises
      {!Address_family}.

    {1:options Socket options}

    [setsockopt], [getsockopt] and the [options] of a [connect] take every
    option {!Eio.Net.Sockopt} names, and the [Unix] options of {!Eio_unix.Net},
    on every socket, and answer as the chosen kernel does on a host with
    loopback only, as recorded through eio_posix:

    - A socket reads back what it set, as the kernel keeps it, and the kernel's
      default otherwise. Linux doubles [SO_SNDBUF] and [SO_RCVBUF], within a
      minimum and its maximum of 7,500,000 bytes, keeps [TCP_DEFER_ACCEPT] as a
      count of retransmissions and the timeouts in milliseconds; macOS keeps a
      buffer size as given, up to 8 MiB. The defaults are the recording hosts':
      on Linux a TCP connection has a send buffer of 2,626,560 bytes and a
      receive buffer of 131,072, and [TCP_CONGESTION] is ["cubic"], ["reno"] the
      only other.
    - An option or a value the kernel refuses for the socket raises what
      eio_posix raises: [Eio.Net.E Invalid_option] for [ENOPROTOOPT], and for an
      option of Linux's under [`Macos]; [Eio.Exn.Not_available] for
      [EOPNOTSUPP], such as a TCP option on a Unix-domain socket under Linux;
      and an unclassified [Eio_unix.Unix_error] otherwise, such as [EINVAL].
      Linux refuses to turn [SO_DEBUG] on, with [EACCES], as it refuses a
      process without [CAP_NET_ADMIN]. A value Eio itself checks raises
      [Invalid_argument].
    - An accepted socket takes its listener's options as the kernel passes them
      on. On Linux a TCP one takes all but [TCP_DEFER_ACCEPT] and
      [TCP_FASTOPEN], and a Unix-domain one none. On macOS it takes all but the
      low-water marks, the timeouts and, over TCP, [TCP_KEEPIDLE].
    - [SO_SNDBUF] and [SO_RCVBUF] size the queue of each direction of a
      connection, in place of the default capacity and within one {!v} or
      {!pair} is given. On Linux over TCP it is the writer's send buffer, at
      least 32 KiB, and half the reader's receive buffer, its window; over a
      Unix path, half the writer's send buffer. On macOS over TCP it is the
      writer's send buffer and what the reader's receive buffer grows to as data
      comes, 128 KiB more than one set and 176 KiB when none is, or the send
      buffer and 16 KiB for one under 16 KiB; over a Unix path, the writer's
      send buffer. On Linux this is within 15% of what the kernel queues in
      writes of 1 KiB. On a datagram socket they replace the buffers
      {!section-datagrams} describes.
    - A TCP socket whose [SO_LINGER] is [Some 0] resets its peer as it closes,
      as both kernels abort the connection.
    - [SO_KEEPALIVE], [TCP_KEEPIDLE], [TCP_KEEPINTVL], [TCP_KEEPCNT] and
      [TCP_USER_TIMEOUT] say when an end of a silent connection gives up ({!v}).
      No probe crosses a live one.
    - [TCP_LINGER2] says how long an end closed before its peer's FIN holds its
      address ({!section-streams}).
    - Every other option is kept and read back, and has no other effect: no
      timeout ends a read, an accept does not wait for data under
      [TCP_DEFER_ACCEPT], and a read does not wait for [SO_RCVLOWAT] bytes.

    {1:datagrams Datagrams}

    Datagram sockets follow Linux. {!section-macos} lists what [`Macos] changes.

    {2:datagram_binding Binding}

    - [datagram_socket] binds the address it is given. [`UdpV4] and [`UdpV6]
      bind the wildcard address of their family, which a kernel binds on an
      unbound socket's first send.
    - An address with port [0], [`UdpV4] or [`UdpV6] takes the lowest port of
      the dynamic range that no datagram socket holds. With every port of the
      range held, it raises [EADDRINUSE], where Linux fails an unbound socket's
      first send with [EAGAIN].
    - An address that overlaps one a datagram socket holds, by the rule of
      [listen], raises [EADDRINUSE], unless both UDP sockets have [reuse_addr].
    - UDP sockets that share an address this way receive by specificity: a
      datagram reaches the one bound at the address itself, then the IPv4
      wildcard, then the IPv6 one. Of those bound at the same address it reaches
      the latest, and once that one closes the one before.
    - [datagram_socket] with [reuse_port] raises [Invalid_argument].
    - Finishing the switch passed to [datagram_socket] closes the socket and
      frees its address. A send, a receive or a shutdown on it then raises
      [Invalid_argument].

    {2:delivery Delivery}

    - A datagram reaches a socket by the rule a [connect] reaches a listener. It
      arrives whole, after every datagram sent to it before: the network never
      cuts or reorders one.
    - It comes with the sender's address as the receiver sees it: the loopback
      address of the destination's family for a wildcard sender, 127.0.0.1 for
      any address of 127.0.0.0/8, and IPv4-mapped for an IPv4 sender to an IPv6
      socket.
    - A datagram sent to an address that no socket binds is lost.
    - A [recv] into a buffer shorter than the datagram gets its first bytes.
    - A socket queues what its receive buffer holds, by default 229,376 bytes,
      each datagram counted with the memory Linux gives it: 199 datagrams of up
      to about 450 bytes, 99 of 1000 bytes, or 3 of 60,000.
    - Over UDP, a datagram past that is lost and its send returns. A receiver
      frees the memory of the datagrams it takes only once they reach a quarter
      of the buffer, or once it has taken every datagram that was queued when it
      found none taken.
    - On a Unix path, a send past that waits until the receiver takes a
      datagram. It also waits while the receiver holds more than 10 datagrams
      ([max_dgram_qlen]), or while the sender's own datagrams not yet received
      fill its send buffer, of the same size: 299 datagrams of a byte, 100 of
      1000 bytes, or 12 of 20,000.

    {2:datagram_shutdown Shutdown}

    - After a socket shut its send side, a send raises {!Closed}, as EPIPE.
    - A send to a Unix-domain socket that shut its receive side raises {!Closed}
      too, a send already waiting for room included.
    - A socket that shut its receive side still receives what is queued and,
      over UDP, what arrives after.

    {2:send_errors Errors of a send}

    - A send without a destination raises {!No_destination} over UDP and
      {!Not_connected} on a Unix path, since no socket is connected.
    - A datagram longer than 65,507 bytes to an IPv4 address, 65,527 to an IPv6
      one, or 212,960 to a Unix path raises {!Message_too_long}. The last is
      what Linux's default send buffer allows; with a smaller send buffer the
      limit is that buffer less 32 bytes. Under [`Macos] the limit is the
      socket's send buffer, by default 9216 bytes over UDP and 2048 on a Unix
      path.
    - A datagram to UDP port 0 raises {!Invalid_port}. Under [`Macos] it is
      sent, and lost.
    - A datagram from an IPv6 address to an IPv4-mapped one raises
      {!Network_unreachable}, and one from an IPv4-mapped address to an IPv6 one
      raises {!Address_family}.
    - A datagram from an IPv4 socket to an IPv6 address, or to a Unix path of 14
      bytes or more, raises {!Address_family}. To a shorter path, too short to
      fill a [sockaddr_in], it raises {!Invalid_address}.
    - A datagram from an IPv6 socket to a Unix path, or from a Unix-domain
      socket to a UDP address or the empty path, raises {!Invalid_address}.
    - A datagram from an IPv6 socket to an IPv4 address goes as from an IPv4
      socket. From an IPv6 address other than the wildcard or an IPv4-mapped
      one, it raises {!Network_unreachable}.
    - A datagram from a loopback address to an IPv4 address outside 127.0.0.0/8,
      other than a multicast one, raises {!Invalid_address}.
    - A datagram that is both too long and refused for its address meets the
      checks in Linux's order. A Unix-domain socket checks the address family
      first. An IPv4 socket refuses a datagram of more than 65,535 bytes first,
      then its address, then the 65,507-byte limit. An IPv6 socket checks the
      address first.

    {1:paths Unix paths}

    Listeners, datagram sockets and clients bound with [bind_to] share one
    namespace of Unix paths, as they share a file system: a path one holds is in
    use to the others. A [connect] at a datagram socket's path raises
    {!Wrong_socket_type}, and so does a datagram sent to a listener's or a
    client's path. A [connect] or a datagram sent at a path no socket holds
    raises [Eio.Fs.Not_found].

    The namespace is not a file system: a socket binds a path whose directory
    does not exist, or that it may not write, where a real socket fails with
    [ENOENT] or [EACCES], since modelling them would need the file system the
    path names. A path with a NUL past its first byte names no file, as OCaml's
    [Unix] refuses it: [listen], [datagram_socket], [connect], [bind_to] and a
    datagram [send] raise [Eio.Fs.Not_found]. A path of 108 bytes or more raises
    [ENAMETOOLONG], or {!Name_too_long} ({!section-errors}).

    With [reuse_addr], a [listen] at a Unix-domain path, not at an abstract
    address, takes the path over, as Eio's backends remove the socket file there
    before they bind: the listener that held it stays open, and no [connect]
    reaches it. A listener or a datagram socket removes its file as it closes,
    by hand or with its switch, as Eio's backends do: a [connect] or a datagram
    sent there then raises [Eio.Fs.Not_found], and a socket may bind the path
    again. A listener whose path was taken over removes the new listener's file
    as it closes, as unlink(2) removes whichever file is there, and no [connect]
    reaches either.

    A listener, a datagram socket or a client bound with [bind_to] at the empty
    path binds an abstract address of its own, a NUL and five hexadecimal
    digits, as Linux autobinds it: [listening_addr] reports it, an [accept] sees
    it as its client's address, and a reply sent there reaches it. An address
    that starts with a NUL is in Linux's abstract namespace, which has no files:
    a socket there leaves nothing behind when it closes. The namespace is one
    per socket type, as on Linux: a stream socket and a datagram one may bind
    the same abstract address, and a [connect] or a datagram sent where no
    socket of its type binds raises {!No_listener}.

    {1:resolution Host resolution}

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
    everywhere. Under [`Macos] each address comes as a datagram one first, as
    macOS orders them.

    [getnameinfo] names [127.0.0.1], IPv4-mapped, IPv4-compatible ([::7f00:1])
    or neither, and [::1] ["localhost"], as [/etc/hosts] does, and gives any
    other address as a number, as glibc gives one that no name server knows: an
    IPv4-compatible one in inet_ntop(3)'s dotted form, such as [::0.1.0.0]. The
    service stays a number, ["80"] where glibc says ["http"], as there is no
    services database. The host of a Unix address is ["localhost"], as there is
    no host name, where glibc gives the host's, and its service is its path up
    to the first NUL, as glibc reads a C string: an abstract address has none.
*)

(** {1:macos macOS}

    Under [`Macos] the network follows macOS where it differs from Linux, as the
    kernel traces record it through eio_posix on macOS 27.3:

    - Addresses. The loopback interface has 127.0.0.1 and [::1] only: a bind at
      another address of 127.0.0.0/8 raises [EADDRNOTAVAIL], a connect there is
      never answered and fails after 75 seconds on [mono], with the [ETIMEDOUT]
      error eio_posix raises (it waits for ever without one), and a datagram
      sent there is lost. A TCP socket binds no multicast or broadcast address,
      and a [connect] reaches none: it raises {!Address_family}; a link-local
      address raises [EADDRNOTAVAIL] from a bind and {!Host_unreachable} from a
      [connect]. A [connect] bound at its own destination raises
      {!Invalid_address}.
    - Ports. A TCP port of [0] takes the next free port of the range, counting
      up from the last one taken, for listeners and clients alike. A [connect]
      without [bind_to] takes one too: macOS shares no port between
      destinations, and holds one in TIME_WAIT toward any of them for its 30
      seconds. A bind looks up its port as the BSDs do: with [reuse_addr] only a
      socket at the same address that is not connected conflicts, a listener or
      a datagram socket; without, any socket at an overlapping address does,
      connected ones and TIME_WAIT included. An IPv6 socket at [::] or an
      IPv4-mapped address also looks at the IPv4 side, where a socket at 0.0.0.0
      does not conflict with it. A [connect] that fails frees its address at
      once.
    - Listeners. The accept queue takes [backlog] connections, 128 for a
      [backlog] of 0 or less or past [kern.ipc.somaxconn]; a [connect] past it
      waits, as on Linux.
    - Connections. A receive shutdown drops what is queued, which still fills
      the queue, and bytes that reach a TCP end after it reset the connection. A
      shutdown of the receive side of an end that reads end-of-file does
      nothing, as macOS refuses it with [ENOTCONN] and Eio ignores that. A TCP
      reset reaches a read even after end-of-file, and a write that raises it
      leaves it to the next read. A Unix-domain reset reads as end-of-file. A
      write to a Unix-domain end that shut its receive side succeeds, its bytes
      dropped. An end of a silent connection keeps probing with bytes
      unacknowledged, and gives up 18 seconds after the first of them was sent,
      as measured over loopback. A write then raises [EPIPE] and leaves
      [ETIMEDOUT] to the next read.
    - Datagrams. A shutdown does nothing, as the socket is not connected. A send
      checks the length first. Then a Unix-domain socket refuses an IP address
      with {!Address_family}; an IPv4 one a path with {!Address_not_available},
      and an IPv6 address with {!Invalid_address} from a wildcard address and
      {!Host_unreachable} otherwise; an IPv6 one an IPv4 address with
      {!Invalid_address}. A wildcard destination has no route
      ({!Host_unreachable}). From [::1] an IPv4-mapped destination is not
      available, and from an IPv4-mapped address other than the wildcard an IPv6
      one has no route. A socket bound at an IPv4-mapped address sees IPv4
      senders as IPv4. A receiver holds its receive buffer, by default 786,896
      bytes over UDP, each datagram counted by its length, and 4096 on a Unix
      path, each counted as at least 128 bytes, which fits what the kernel
      traces record for datagrams of 1 to 2000 bytes. A datagram past that is
      lost over UDP, and on a Unix path the send raises the [ENOBUFS] error
      eio_posix raises, where Linux waits; a receive frees what it takes at
      once.
    - Unix paths. [sun_path] holds 103 bytes. There is no abstract namespace: a
      path with a NUL anywhere names no file, and the empty path binds nothing,
      with [EINVAL].
    - Resolution. A service is a decimal number of at most 65535, as strtoul(3)
      reads it, and any other raises [Address_lookup_failed NONAME].
      inet_aton(3) reads a bare [0x] before the last part as 0. An IPv6 address
      takes any scope after a [%]. [getnameinfo] of a Unix address raises
      [Address_lookup_failed UNKNOWN], as eio_posix does when macOS's
      getnameinfo(3) fails.

    Where macOS's loopback delivers a segment asynchronously, the network
    delivers it at once, as if the next call came after it. *)

(**/**)

module Private : sig
  val v :
    ?os:[< `Linux | `Macos ] ->
    capacity:(int -> int) ->
    ?mono:_ Eio.Time.Mono.t ->
    sw:Eio.Switch.t ->
    unit ->
    [ `Generic ] Eio.Net.ty Eio.Resource.t
  (** [v ~capacity ~sw ()] is the network [Eio_net_mem.v ~sw ()] makes, whose
      connections queue [capacity d] each way, where [d] is the chosen kernel's
      default for their family. [capacity d] must be positive. For
      eio-net.chaos; it may change at any release. *)

  val silence : _ Eio.Net.stream_socket -> unit
  (** [silence s] cuts the connection of [s] for good, as a partition does:
      nothing crosses it either way from then on. Over TCP what is queued is
      lost, an end of stream included. On a Unix-domain connection each end
      reads what is queued to it, and a direction that had ended keeps its end.
      An end then waits for bytes until its own receive shutdown, after which it
      reads end of file. Each end's writes fill its queue, then wait until its
      own send shutdown, after which they raise the reset. Neither end's
      shutdown or close reaches the other. On the network's clock, each end of a
      TCP connection gives up as {!v} says. For eio-net.chaos; it may change at
      any release.

      @raise Invalid_argument if [s] is not a connection of this library. *)

  val connection_id : _ Eio.Net.stream_socket -> int option
  (** [connection_id s] is [Some n] if [s] is an end of a connection of this
      library, made by {!v} or {!pair}, and [None] otherwise. The two ends of a
      connection have the same [n], and no other connection has it. A wrapper
      that sees both ends, such as a fault injector, pairs them with it. For
      eio-net.chaos; it may change at any release. *)
end
