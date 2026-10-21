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

    TCP stream addresses are matched exactly. A [listen] on port [0] binds the
    lowest port of the dynamic range, 49152-65535 (RFC 6335, section 6), that no
    listener holds, as a socket's bind does, and [listening_addr] reports it.
    Host resolution maps ["localhost"] and ["127.0.0.1"] to IPv4 loopback for
    the requested service port. Unix stream addresses are also accepted directly
    by [listen] and [connect].

    [connect] accepts, and ignores, the socket options whose effect these queues
    already have: the ones asking that writes not be coalesced and that
    acknowledgements not be delayed, neither of which happens here. It refuses
    every other option, naming it, because ignoring one that would change what a
    caller observes is a wrong answer with no symptom. A caller cannot tell an
    ignored option from an applied one, so a test that must know an option took
    effect needs a real socket. [bind_to] is refused: there is no local address
    to bind.

    A [datagram_socket] binds its address, or for [`UdpV4], [`UdpV6] or port [0]
    the lowest free port of the dynamic range on loopback, and refuses an
    address already bound and [reuse_port]. A datagram sent to it arrives whole,
    with the sender's address, after every datagram sent to it before: the
    network never drops, cuts or reorders one, and a send never waits. One sent
    to an address no socket binds, at its own address or the wildcard address of
    its port, is lost, as UDP loses it. A [recv] into a buffer shorter than the
    datagram gets its first bytes, as a socket does. The socket is closed, and
    its address free again, when the switch passed to [datagram_socket]
    finishes. *)

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
    may be in different domains. *)
