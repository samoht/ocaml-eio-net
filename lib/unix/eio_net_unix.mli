(** Unix adapters for frame channels, stream writes, sockets and the host's
    interface addresses. Each module is a unit of its own, so that a unit naming
    one imports only the interfaces that one names.

    Unlike the other libraries of eio-net, this one is not compiled under the
    [Hermetic] prelude: it makes the host's sockets with [Unix] and wraps
    descriptors with [Eio_unix], which the prelude refuses. It reaches a file
    only through the {!Eio.Path.t} it is handed ({!Local}). *)

module Frame = Frame
module Stream = Stream
module Local = Local
module Tcp = Tcp
module Interface = Interface

val connect_failure : exn -> Eio_net_connect.answer option
(** [connect_failure exn] is the answer a failed {!Eio.Net.connect} raising
    [exn] carries from the network, or [None] when it carries none, as a timeout
    or a local error does:

    - [`Refused] for [Eio.Net.Connection_failure (Refused _)], a reset in answer
      to the SYN (ECONNREFUSED).
    - [`Unreachable] for ENETUNREACH and EHOSTUNREACH, which Eio reports as an
      unclassified {!Eio_unix.Unix_error}: an ICMP destination unreachable, or
      the host's own routing table, says no path to the peer exists. RFC 1122
      section 4.2.3.9 makes it a hard error for an attempt in SYN-SENT.

    It is the classifier to hand a connection library that connects over a Unix
    network, in place of the portable {!Eio_net_connect.classify}. *)

val accept_failure : exn -> Eio_net_accept.answer option
(** [accept_failure exn] is the answer a failed {!Eio.Net.accept} raising [exn]
    carries from the system, or [None] when it carries none:

    - [`Aborted] for [Eio.Net.Connection_reset _], as Eio reports ECONNABORTED:
      a connection its peer reset before it was accepted.
    - [`Descriptors] for EMFILE and ENFILE: the process, or the system, has no
      descriptor left for the connection.

    It is the classifier to hand {!Eio_net_accept.run} over a Unix network, as
    [~classify:Eio_net_unix.accept_failure]: the portable
    {!Eio_net_accept.classify} reads no descriptor left as no answer, and a loop
    handed it raises that accept's error. *)
