(** The accept loop a listener serves its connections with, and the answer a
    failed {!Eio.Net.accept} carries from the system.

    {!run} accepts and serves connections within a declared bound and bounds by
    its state what peers can make it hold: under pressure it sheds the held
    connection whose head is incomplete and has made the least progress, among
    its own when its bound is reached and among every loop of its {!type-pool}
    when descriptors run out. An accept its peer aborted, or one with no
    descriptor left, leaves it whole. A loop sheds the connections of another
    only when both were handed the same pool.

    Eio names a connection reset portably and reports the rest as a backend
    error, so {!run} reads a failed accept with the classifier it is handed:
    {!classify} by default, which reads Eio's portable errors, and over a
    platform's network the one the library that provides that network gives, as
    [Eio_net_unix.accept_failure] reads EMFILE and ENFILE. A classifier is
    handed to one loop, as a pool is: no library a program links changes how
    another's loop reads its failures. *)

(** {1:failures Failed accepts} *)

type answer =
  [ `Aborted
    (** The connection ended before it was accepted: its peer aborted it
        (ECONNABORTED, which Eio reports as [Eio.Net.Connection_reset]), or a
        network error was pending on it, which accept(2) on Linux passes on. It
        asks for the next accept. *)
  | `Descriptors
    (** The process or the system has no descriptor left for the connection:
        EMFILE or ENFILE. It asks for a descriptor to be freed first. *) ]
(** The type for the answers from the system that end one accept and leave the
    listener whole. *)

val classify : exn -> answer option
(** [classify exn] is the answer the exception [exn] of a failed accept carries
    as Eio's portable errors name it: [Some `Aborted] for
    [Eio.Net.Connection_reset _], and [None] otherwise, as for a closed
    listener, a cancellation or an error of a backend. It is {!run}'s default;
    the classifier of a platform's network answers that platform's errors as
    well. *)

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

type pool
(** The type for a set of loops that share their descriptors: when an accept of
    one finds none left, it sheds among the connections of them all. *)

val pool : unit -> pool
(** [pool ()] is a new pool, with no loop. Handing it to several {!run}s, on any
    domains, lets each free a descriptor held by another; a loop can shed only
    the connections of its own pool, whatever its listener's accept raises. *)

val run :
  ?stop:'a Eio.Promise.t ->
  ?on_error:(exn -> unit) ->
  ?pool:pool ->
  ?classify:(exn -> answer option) ->
  max_connections:int ->
  [> 'tag Eio.Net.listening_socket_ty ] Eio.Resource.t ->
  (head ->
  'tag Eio.Net.stream_socket_ty Eio.Resource.t ->
  Eio.Net.Sockaddr.stream ->
  unit) ->
  'a
(** [run ~stop ~on_error ~pool ~classify ~max_connections socket handler]
    accepts connections on [socket] and serves each with [handler h flow addr]
    in a fiber of its own, where [h] is the connection's {!type-head}, [flow]
    its socket and [addr] its peer's address. [flow] is closed when [handler]
    returns.

    What peers can make it hold is bounded by its state. Each connection holds
    one descriptor, one fiber and what [handler] holds, and at most
    [max_connections] connections are held at once. The loop is under pressure
    when every one of them is held and another peer connects: it sheds its held
    connection whose head is incomplete and has made the least progress, the
    older of two that made the same, and waits for one of its connections to end
    before it serves the next; with no incomplete head held, it waits for one
    that is being served.

    [classify] (defaults to {!val-classify}) reads the {!Eio.Io} exception of
    each failed accept. An accept it reads as [`Aborted] is followed by the
    next. One it reads as [`Descriptors] makes the loop choose the same way
    among the connections of every [run] of [pool] (defaults to a new pool of
    this loop alone), on any domain, and wait for any of them to end before it
    accepts again; with no connection held by any loop of [pool], it raises the
    accept's error, naming that none was held to shed or wait for. Choosing
    costs a pass over the connections the pool's loops hold. A connection is
    shed by shutting its socket down, with no answer, so its reads end on
    whichever domain it runs. Any other failed accept raises its error, as one
    with no descriptor left does over a Unix network unless [classify] is
    [Eio_net_unix.accept_failure].

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
