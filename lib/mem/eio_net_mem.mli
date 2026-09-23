(** In-memory {!Eio.Net} stream network.

    This backend is intended for hermetic tests that need client code and server
    code to talk through the normal {!Eio.Net} APIs without binding OS sockets.
    It supports stream sockets only. *)

type Eio.Exn.Backend.t +=
  | Closed
        (** The backend code carried by the connection-reset a mem flow raises
            when a write finds its send side closed (the peer has gone away, or
            this end shut its own send). It makes that write an {!Eio.Io} error
            of the same shape a real socket raises on write-after-reset, rather
            than the bare [Failure] {!Eio.Buf_write} raises internally. A read
            after close still raises [End_of_file], matching a real socket's
            EOF. *)

val v : sw:Eio.Switch.t -> unit -> [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~sw ()] is a network whose [listen] and [connect] operations are paired
    in memory.

    TCP stream addresses are matched exactly. Host resolution maps ["localhost"]
    and ["127.0.0.1"] to IPv4 loopback for the requested service port. Unix
    stream addresses are also accepted directly by [listen] and [connect].

    [connect] accepts, and ignores, the socket options whose effect these queues
    already have: the ones asking that writes not be coalesced and that
    acknowledgements not be delayed, neither of which happens here. It refuses
    every other option, naming it, because ignoring one that would change what a
    caller observes is a wrong answer with no symptom. A caller cannot tell an
    ignored option from an applied one, so a test that must know an option took
    effect needs a real socket. [bind_to] is refused: there is no local address
    to bind. *)

val pipe :
  unit ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pipe ()] is a connected pair of in-memory stream sockets: bytes written on
    one are read on the other, and vice versa, preserving order. Unlike {!v} it
    binds no address and needs no [listen] and [connect] handshake: it is the
    bare transport for a test, or a wrapper such as an adversarial flow, that
    needs two ends of a pipe. Blocking reads suspend the calling fiber until
    data arrives, so both ends must run under an Eio event loop. *)
