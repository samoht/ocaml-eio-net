(** The accept loop a listener serves its connections with, and the answer a
    failed {!Eio.Net.accept} carries from the system.

    {!run} accepts and serves connections within a declared bound and bounds by
    its state what peers can make it hold: under pressure it sheds the held
    connection whose head is incomplete and has made the least progress, among
    its own when its bound is reached and among every loop of the process when
    descriptors run out. An accept its peer aborted, or one with no descriptor
    left, leaves it whole.

    Eio names a connection reset portably and reports the rest as a backend
    error, so the library that provides a platform's network registers the
    classifier that reads its own errors, once, when it is linked, as
    {!Eio_net_connect} does for a connect. {!run} asks {!classify} and works the
    same over every network. *)

(** {1:loop The loop} *)

type head
(** The type for the head of a connection {!run} holds: what its peer sends
    before it is served, as an HTTP request head or an SSH handshake through
    authentication. A head is incomplete from the accept, with no byte of it
    received, until the connection says it is {!complete}. *)

val reading : head -> (unit -> int) -> unit
(** [reading head received] says [head] is incomplete and [received ()] is the
    number of its bytes received so far, which {!run} reads when it chooses a
    connection to shed. A protocol that serves one connection several heads, as
    HTTP/1.1 does its requests, says [reading] before each. Once {!run}'s [stop]
    has resolved, [reading] sheds the connection. *)

val complete : head -> unit
(** [complete head] says [head] is complete: its connection is being served, and
    pressure does not shed it until it is {!reading} again. *)

val run :
  ?stop:'a Eio.Promise.t ->
  ?on_error:(exn -> unit) ->
  max_connections:int ->
  [> 'tag Eio.Net.listening_socket_ty ] Eio.Resource.t ->
  (head ->
  'tag Eio.Net.stream_socket_ty Eio.Resource.t ->
  Eio.Net.Sockaddr.stream ->
  unit) ->
  'a
(** [run ~stop ~on_error ~max_connections socket handler] accepts connections on
    [socket] and serves each with [handler h flow addr] in a fiber of its own,
    where [h] is the connection's {!type-head}, [flow] its socket and [addr] its
    peer's address. [flow] is closed when [handler] returns.

    What peers can make it hold is bounded by its state. Each connection holds
    one descriptor, one fiber and what [handler] holds, and at most
    [max_connections] connections are held at once. The loop is under pressure
    when every one of them is held and another peer connects: it sheds its held
    connection whose head is incomplete and has made the least progress, the
    older of two that made the same, and waits for one of its connections to end
    before it serves the next; with no incomplete head held, it waits for one
    that is being served. Descriptors are the process's, so when an accept fails
    because the process or the system has no descriptor left ({!classify}) the
    loop chooses the same way among the connections of every [run] of the
    process, on any domain, and waits for any of them to end before it accepts
    again. Choosing costs a pass over the connections the process's loops hold.
    A connection is shed by shutting its socket down, with no answer, so its
    reads end on whichever domain it runs. An accept its peer aborted is
    followed by the next. An accept with no descriptor left while no loop of the
    process holds a connection raises its error, naming that none was held to
    shed or wait for. Any other failed accept raises its error.

    [on_error] (defaults to logging a warning on the [eio-net.accept] log
    source) receives each exception a connection's [handler] raises but
    [Eio.Cancel.Cancelled], with the peer's address in its context when it is an
    [Eio.Io], and the loop goes on serving the others: a failure on the socket,
    and an error the peer caused, as a malformed frame ([Eio_net_framing.Error])
    or a stalled transfer ([Eio_net.Progress.Too_slow]), end that connection
    alone. A shed connection's is ignored. An exception [on_error] raises ends
    the loop, which raises it: [~on_error:raise] makes every failed connection
    end it.

    Its connections end with it. Without [stop] it returns only by raising. Once
    [stop] resolves it accepts no more, sheds each connection whose head is
    incomplete, at once or when it next says {!reading}, and returns [stop]'s
    value once the others have ended.

    Raises [Invalid_argument] if [max_connections] is less than [1]. *)

(** {1:failures Failed accepts} *)

type answer =
  [ `Aborted
    (** The peer aborted the connection before it was accepted: ECONNABORTED,
        which Eio reports as [Eio.Net.Connection_reset]. It asks for the next
        accept. *)
  | `Descriptors
    (** The process or the system has no descriptor left for the connection:
        EMFILE or ENFILE. It asks for a descriptor to be freed first. *) ]
(** The type for the answers from the system that end one accept and leave the
    listener whole. *)

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
