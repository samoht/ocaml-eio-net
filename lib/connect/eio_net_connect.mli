(** The answer a failed {!Eio.Net.connect} carries from the network.

    Eio names a refusal portably and reports the rest as a backend error, so the
    library that provides a platform's network registers the classifier that
    reads its own errors, once, when it is linked. A connection library asks
    {!classify} and works the same over every network. *)

type answer = [ `Refused | `Unreachable ]
(** The type for the answers from the network that end a connection attempt: its
    peer refused it (ECONNREFUSED, a TCP reset in answer to the SYN: RFC 9293
    section 3.10.7.3), or no path to its peer exists (ENETUNREACH, EHOSTUNREACH:
    a hard error for an attempt in SYN-SENT, RFC 1122 section 4.2.3.9). *)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed connect
    carries, or [None] when it carries none, as a timeout or a local error does.
    It asks the registered classifier, or with none registered is
    [Some `Refused] for [Eio.Net.Connection_failure (Refused _)] and [None]
    otherwise: the answers Eio's portable errors name. *)

val register : name:string -> (exn -> answer option) -> unit
(** [register ~name f] makes [f] the classifier {!classify} asks. [name] is the
    library registering it. Only the library that provides a platform's network
    calls it, once, from its own initialisation, so a program gets the
    classifier by linking that library: [eio-net.unix] registers
    [Eio_net_unix.connect_failure]. A program links one such library.

    Raises [Invalid_argument] naming both libraries if a classifier is already
    registered. *)
