(** The answer a failed {!Eio.Net.accept} carries from the system.

    Eio names a connection reset portably and reports the rest as a backend
    error, so the library that provides a platform's network registers the
    classifier that reads its own errors, once, when it is linked, as
    {!Eio_net_connect} does for a connect. A listener asks {!classify} and works
    the same over every network. *)

type answer = [ `Aborted | `Descriptors ]
(** The type for the answers from the system that end one accept and leave the
    listener whole: the connection was aborted by its peer before it was
    accepted (ECONNABORTED, which Eio reports as [Eio.Net.Connection_reset]), or
    the process or the system has no descriptor left for it (EMFILE, ENFILE).
    The first asks for the next accept; the second for a descriptor freed first.
*)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed accept carries,
    or [None] when it carries none, as a closed listener or a cancellation does.
    It asks the registered classifier, or with none registered is
    [Some `Aborted] for [Eio.Net.Connection_reset _] and [None] otherwise: the
    answers Eio's portable errors name. *)

val register : name:string -> (exn -> answer option) -> unit
(** [register ~name f] makes [f] the classifier {!classify} asks. [name] is the
    library registering it. Only the library that provides a platform's network
    calls it, once, from its own initialisation: [eio-net.unix] registers
    [Eio_net_unix.accept_failure].

    Raises [Invalid_argument] naming both libraries if a classifier is already
    registered. *)
