(** The answer a failed {!Eio.Net.connect} carries from the network.

    Eio names a refusal portably and reports the rest as a backend error, so the
    library that provides a platform's network registers the classifier that
    reads its own errors, once, when it is linked. A connection library asks
    {!classify} and works the same over every network. *)

type answer =
  [ `Refused
    (** The peer refused the attempt: ECONNREFUSED, a TCP reset in answer to the
        SYN (RFC 9293 section 3.10.7.3). *)
  | `Unreachable
    (** No path to the peer exists: ENETUNREACH or EHOSTUNREACH, a hard error
        for an attempt in SYN-SENT (RFC 1122 section 4.2.3.9). *) ]
(** The type for the answers from the network that end a connection attempt. *)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed connect
    carries, or [None] when it carries none, as a timeout or a local error does.
    It asks each registered classifier in registration order and is the first
    answer that is not [None], or with none registered is [Some `Refused] for
    [Eio.Net.Connection_failure (Refused _)] and [None] otherwise: the answers
    Eio's portable errors name. *)

val register : (exn -> answer option) -> unit
(** [register f] adds [f] to the classifiers {!classify} asks, after those
    registered before it. The library that provides a platform's network calls
    it, once, from its own initialisation, so a program gets the classifier by
    linking that library: [eio-net.unix] registers
    [Eio_net_unix.connect_failure]. A program that links several such libraries
    gets each one's answers, the first registered's first. *)
