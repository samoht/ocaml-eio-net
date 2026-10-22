(** Unix adapters for frame channels, stream writes and sockets. Each module is
    a unit of its own, so that a unit naming one imports only the interfaces
    that one names. *)

module Frame = Frame
module Stream = Stream
module Local = Local
module Tcp = Tcp

val connect_failure : exn -> [ `Refused | `Unreachable ] option
(** [connect_failure exn] is the answer a failed {!Eio.Net.connect} raising
    [exn] carries from the network, or [None] when it carries none, as a timeout
    or a local error does:

    - [`Refused] for [Eio.Net.Connection_failure (Refused _)], a reset in answer
      to the SYN (ECONNREFUSED).
    - [`Unreachable] for ENETUNREACH and EHOSTUNREACH, which Eio reports as an
      unclassified {!Eio_unix.Unix_error}: an ICMP destination unreachable, or
      the host's own routing table, says no path to the peer exists. RFC 1122
      section 4.2.3.9 makes it a hard error for an attempt in SYN-SENT.

    Linking this library registers it with {!Eio_net_connect.register}, so a
    connect that asks {!Eio_net_connect.classify} over a Unix network uses it
    with no argument. *)
