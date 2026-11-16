(** A stream relay: splice two flows together, or listen and forward each
    accepted connection to another address, over TCP or Unix-domain sockets.

    It serves port forwarding, such as a local proxy in front of a remote
    service or a test harness standing between a client and a server. It knows
    nothing about what travels through it: bytes are copied both ways until each
    side finishes. *)

val splice : _ Eio.Flow.two_way -> _ Eio.Flow.two_way -> unit
(** [splice a b] copies [a] to [b] and [b] to [a] concurrently until each source
    reaches end-of-file. A finished direction propagates as a half-close
    ([Eio.Flow.shutdown `Send] on the other flow), so a peer that reads until
    end-of-file sees it as soon as its counterpart is done writing. The call
    returns when both directions are done, and closes neither flow. An exception
    from either flow, such as a reset, cancels the other direction and
    propagates. *)

val forward :
  ?on_error:(exn -> unit) ->
  ?pool:Eio_net_accept.pool ->
  ?classify:(exn -> Eio_net_accept.answer option) ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  max_connections:int ->
  listen:Eio.Net.Sockaddr.stream ->
  connect:Eio.Net.Sockaddr.stream ->
  unit ->
  Eio.Net.Sockaddr.stream
(** [forward ~on_error ~pool ~classify ~sw ~net ~max_connections ~listen
     ~connect ()] listens on [listen] (with a backlog of [16], and
    [SO_REUSEADDR] for a TCP address) and, for every accepted connection, dials
    [connect] and {!splice}s the two, in an {!Eio_net_accept.run} loop on [sw].
    It is the bound address once listening (for a [listen] naming port 0, the
    port the system chose), and it serves until [sw] ends.

    The loop relays at most [max_connections] connections at once: a client past
    them waits, holding no upstream connection, until one ends. A relay reads
    nothing from a client before it relays it, so it marks each connection
    {!Eio_net_accept.complete} as it is accepted, and the loop sheds none of
    them under pressure.

    A connection that fails (the dial refused, either side dropping mid-splice)
    closes that connection's flows and is reported to [on_error] (defaults to
    logging a warning on the [eio-net.accept] log source, as
    {!Eio_net_accept.run} does); the listener itself keeps accepting. A failed
    accept is read by [classify] (defaults to {!Eio_net_accept.classify}) as
    {!Eio_net_accept.run} reads it, and one that ends the loop fails [sw]: over
    a Unix network, [~classify:Eio_net_unix.classify_accept] makes an accept
    with no descriptor left wait for a relayed connection to end rather than end
    the relay.

    [pool] (defaults to a new pool of this relay alone) is the loop's pool. When
    an accept finds no descriptor left and the relay relays nothing, a relay
    alone in its pool ends. Handed the pool of the process's other loops, it
    sheds an incomplete head that one of them holds instead. Their accepts never
    shed a relayed connection, which is complete from its accept.

    An error of {!Eio.Net.listen}, such as an address in use, propagates. A Unix
    path is in use while a file is there, whether or not a listener still serves
    it.

    @raise Invalid_argument if [max_connections] is less than [1]. *)
