(** The accept loop a listener serves its connections with, and the answer a
    failed {!Eio.Net.accept} carries from the system.

    {!run} serves at most [max_connections] connections. When it is full, it
    sheds the connection whose head is incomplete and has made the least
    progress: it shuts that connection's socket down, with no answer. When the
    process runs out of descriptors, it sheds the same way among the connections
    of every loop of its {!type-pool}. An accept its peer aborted, or one with
    no descriptor left, does not end the loop. A loop sheds the connections of
    another only when both were handed the same pool.

    Eio names a connection reset portably and reports every other failed accept
    as a backend error. {!run} therefore reads a failed accept with the
    classifier it is handed: {!classify} by default, or the classifier of the
    platform's network, such as [Eio_net_unix.classify_accept] for EMFILE and
    ENFILE. A classifier belongs to one loop, and so does a pool: linking a
    library never changes how another loop reads its failures. *)

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
    [Eio.Net.Connection_reset _], and [None] otherwise, for example for a closed
    listener, a cancellation or an error of a backend. It is {!run}'s default.
*)

(** {1:loop The loop} *)

type head
(** The type for the head of a connection {!run} holds: what its peer sends
    before it is served, such as an HTTP request head or an SSH handshake up to
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

val reading_flow :
  head ->
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
(** [reading_flow head flow] is [flow], once it says [head] is {!reading} with
    the bytes read from [flow] through it as its progress: the head of a layer
    that reads [flow] itself, as a TLS handshake reads its records off the
    socket. Closing it closes [flow]. *)

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

    [run] serves at most [max_connections] connections, and one more accepted
    connection waits unserved, so it holds at most [max_connections + 1]
    descriptors. Each connection holds one descriptor, one fiber and what
    [handler] holds. When [max_connections] connections are served and another
    peer connects, [run] accepts it, sheds the served connection whose head is
    incomplete and has made the least progress (the older of two that made the
    same), and serves the new one once a connection has ended. With no
    incomplete head among them, it waits for a connection to end.

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
    [Eio_net_unix.classify_accept].

    [on_error] (defaults to logging a warning on the [eio-net.accept] log
    source) receives each exception a connection's [handler] raises but
    [Eio.Cancel.Cancelled], with the peer's address in its context when it is an
    [Eio.Io], and the loop goes on serving the others: a failure on the socket,
    and an error the peer caused, such as a malformed frame
    ([Eio_net.Framing.Error]) or a stalled transfer
    ([Eio_net.Progress.Too_slow]), end that connection alone. A shed
    connection's is ignored. An exception [on_error] raises ends the loop, which
    raises it: [~on_error:raise] makes every failed connection end it.

    Its connections end with it. Without [stop] it returns only by raising. Once
    [stop] resolves it closes [socket], so a peer that connects then is refused,
    sheds each connection whose head is incomplete, at once or when it next says
    {!reading}, and returns [stop]'s value once the others have ended.

    @raise Invalid_argument if [max_connections] is less than [1]. *)
