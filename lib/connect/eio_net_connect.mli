(** A classifier of failed connects: it tells whether the network refused the
    attempt or found no path to the peer.

    An {!type-answer} is what the network replied to a {!Eio.Net.connect}: a TCP
    reset or an ICMP error. A timeout or a local error is no answer. A
    connection library uses the classifier to tell a peer that refused, or that
    no route reaches, from one that did not reply in time.

    Eio names a refusal portably and reports every other failed connect as a
    backend error. A connection library therefore reads a failed connect with
    the classifier its caller hands it: {!classify} by default, or the
    classifier of the platform's network, such as
    [Eio_net_unix.classify_connect] for ENETUNREACH and EHOSTUNREACH. A
    classifier is passed to the code that connects, so linking a library never
    changes how another's connects are read. *)

type answer =
  [ `Refused
    (** The peer refused the attempt: ECONNREFUSED, a TCP reset in answer to the
        SYN (RFC 9293 section 3.10.7.3). *)
  | `Unreachable
    (** No path to the peer exists: ENETUNREACH or EHOSTUNREACH, a hard error
        for an attempt in SYN-SENT (RFC 1122 section 4.2.3.9). *) ]
(** The type for the answers from the network that end a connection attempt. *)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed connect carries
    as Eio's portable errors name it: [Some `Refused] for
    [Eio.Net.Connection_failure (Refused _)], and [None] otherwise, for example
    for a timeout, a local error or an error of a backend. *)
