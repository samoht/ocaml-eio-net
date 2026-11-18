(** Unix adapters for frame channels, stream writes, sockets and the host's
    interface addresses. *)

module Frame = Frame
(** Frame channels over Unix datagram descriptors. *)

module Stream = Stream
(** Stream writes offloaded to a systhread. *)

module Local = Local
(** Unix-domain stream sockets. *)

module Interface = Interface
(** Interface addresses and their changes. *)

val classify_connect : exn -> Eio_net_connect.answer option
(** [classify_connect exn] is the answer a failed {!Eio.Net.connect} raising
    [exn] carries from the network, or [None] when it carries none, such as a
    timeout or a local error:

    - [`Refused] for [Eio.Net.Connection_failure (Refused _)], a reset in answer
      to the SYN (ECONNREFUSED).
    - [`Unreachable] for ENETUNREACH and EHOSTUNREACH, which Eio reports as an
      unclassified {!Eio_unix.Unix_error}: an ICMP destination unreachable, or
      the host's own routing table, says no path to the peer exists. RFC 1122
      section 4.2.3.9 makes it a hard error for an attempt in SYN-SENT.

    It is the classifier to hand a connection library that connects over a Unix
    network, in place of the portable {!Eio_net_connect.classify}. *)

val classify_accept : exn -> Eio_net.Accept.answer option
(** [classify_accept exn] is the answer a failed {!Eio.Net.accept} raising [exn]
    carries from the system, or [None] when it carries none:

    - [`Aborted] for [Eio.Net.Connection_reset _], as Eio reports ECONNABORTED:
      a connection its peer reset before it was accepted.
    - [`Aborted] for a network error pending on the new connection, which
      accept(2) on Linux passes on and tells the caller to retry: ENETDOWN,
      EPROTO, ENOPROTOOPT (which Eio reports as [Eio.Net.Invalid_option]),
      EHOSTDOWN, ENONET, EHOSTUNREACH and ENETUNREACH. The error goes with the
      connection it was pending on, and a BSD system clears the one it returns,
      so the next accept does not meet it again. EOPNOTSUPP, also on Linux's
      list, is no answer: accept(2) also returns it for every accept on a socket
      that is not a stream. Nor is EPERM, which a security module may return for
      every accept. A loop raises either rather than spin on it.
    - [`Descriptors] for EMFILE and ENFILE: the process, or the system, has no
      descriptor left for the connection.

    It is the classifier to hand {!Eio_net.Accept.run} over a Unix network, as
    [~classify:Eio_net_unix.classify_accept]: the portable
    {!Eio_net.Accept.classify} reads no descriptor left as no answer, and a loop
    handed it raises that accept's error. *)
