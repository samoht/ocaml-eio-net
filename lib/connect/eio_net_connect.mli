(** The answer a failed {!Eio.Net.connect} carries from the network.

    Eio names a refusal portably and reports the rest as a backend error, so a
    connection library reads a failed connect with the classifier its caller
    hands it: {!classify} by default, which reads Eio's portable errors, and
    over a platform's network the one the library that provides that network
    gives, as [Eio_net_unix.connect_failure] reads ENETUNREACH and EHOSTUNREACH.
    A classifier is handed to the code that connects: no library a program links
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
    [Eio.Net.Connection_failure (Refused _)], and [None] otherwise, as for a
    timeout, a local error or an error of a backend. It is the default
    classifier of a connection library; the classifier of a platform's network
    answers that platform's errors as well. *)
