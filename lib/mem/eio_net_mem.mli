(** In-memory {!Eio.Net} network.

    This backend is intended for hermetic tests that need client code and server
    code to talk through the normal {!Eio.Net} APIs without binding OS sockets.
    It supports stream sockets and datagram sockets. *)

type Eio.Exn.Backend.t +=
  | Closed
        (** The backend code carried by the connection-reset a mem flow raises
            when a write finds its send side closed (the peer has gone away, or
            this end shut its own send). It makes that write an {!Eio.Io} error
            of the same shape a real socket raises on write-after-reset. A read
            after close raises [End_of_file] once the bytes already written have
            been read, matching a real socket's EOF. *)
  | No_listener
        (** The backend code carried by the connection failure [connect] raises
            when no listener is bound at the address, as a real socket raises
            [Connection_failure (Refused _)]. *)

val v : sw:Eio.Switch.t -> unit -> [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~sw ()] is a network whose [listen] and [connect] operations are paired
    in memory.

    A connection is closed when the switch passed to [connect] finishes, so the
    accepting end then reads EOF. The two ends of a connection may be used from
    different domains: a write in one wakes a read waiting in the other. A write
    never waits, since a connection holds whatever is written to it.

    A [listen] binds the TCP or Unix stream address it is given. One on TCP port
    [0] binds the lowest port of the dynamic range, 49152-65535 (RFC 6335,
    section 6), that no listener holds, as a socket's bind does, and
    [listening_addr] reports it; it raises [Failure] if every port of the range
    is held. A [listen] at an address a listener holds takes the address over
    from it. A listener is closed when the switch passed to {!v} finishes,
    whatever switch [listen] is given. A [connect] reaches the listener at its
    address, or at the wildcard address of its port, and waits while [128]
    connections wait to be accepted there; with no listener it raises the
    connection failure that {!No_listener} marks. An accepted connection's peer
    address is [`Unix "in-memory-client"]. Host resolution maps ["localhost"]
    and ["127.0.0.1"] with a numeric service to IPv4 loopback at that port, and
    any other host or service to no address.

    [connect] accepts, and ignores, the socket options whose effect these queues
    already have: the ones asking that writes not be coalesced and that
    acknowledgements not be delayed, neither of which happens here. It refuses
    every other option with [Invalid_argument], naming it, because ignoring one
    that would change what a caller observes is a wrong answer with no symptom.
    A caller cannot tell an ignored option from an applied one, so a test that
    must know an option took effect needs a real socket. [bind_to] is refused
    with [Invalid_argument]: there is no local address to bind. On a socket,
    [setsockopt] does nothing and [getsockopt] raises [Invalid_argument].

    A [datagram_socket] binds the address it is given: for [`UdpV4] and [`UdpV6]
    the loopback address at the lowest port of the dynamic range that no
    datagram socket holds, and for port [0] that port at its IP address. It
    refuses an address already bound, and [reuse_port], with [Invalid_argument].
    A datagram sent to it arrives whole, with the sender's address, after every
    datagram sent to it before: the network never drops, cuts or reorders one,
    and a send never waits. One sent to an address no socket binds, at its own
    address or the wildcard address of its port, is lost, as UDP loses it. A
    send without a destination raises [Invalid_argument], since no socket is
    connected. A [recv] into a buffer shorter than the datagram gets its first
    bytes, as a socket does. The socket is closed, and its address free again,
    when the switch passed to [datagram_socket] finishes; a send or a receive on
    it then raises [Invalid_argument]. *)

val pipe :
  unit ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pipe ()] is a connected pair of in-memory stream sockets: bytes written on
    one are read on the other, and vice versa, preserving order. Unlike {!v} it
    binds no address and needs no [listen] and [connect] handshake: it is the
    bare transport for a test, or a wrapper such as an adversarial flow, that
    needs two ends of a pipe. Blocking reads suspend the calling fiber until
    data arrives, so both ends must run under an Eio event loop; the two ends
    may be in different domains. No switch closes them: the caller does. *)
