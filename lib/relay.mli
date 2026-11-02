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
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?on_error:(exn -> unit) ->
  max_connections:int ->
  listen:Eio.Net.Sockaddr.stream ->
  connect:Eio.Net.Sockaddr.stream ->
  unit ->
  Eio.Net.Sockaddr.stream
(** [forward ~sw ~net ~on_error ~max_connections ~listen ~connect ()] listens on
    [listen] (with a backlog of [16] and [SO_REUSEADDR]) and, for every accepted
    connection, dials [connect] and {!splice}s the two, in an
    {!Eio_net_accept.run} loop on [sw] that relays at most [max_connections]
    connections at once: a client past them waits, holding no upstream
    connection, until one ends. A relay reads nothing from a client before it
    relays it, so it marks each connection {!Eio_net_accept.complete} as it is
    accepted, and the loop sheds none of them under pressure: it waits for one
    to end. It is the bound address once listening (for a [listen] naming port
    0, the port the system chose), and it serves until [sw] ends.

    A connection that fails (the dial refused, either side dropping mid-splice)
    closes that connection's flows and is reported to [on_error] (defaults to
    logging a warning on the [eio-net.accept] log source, as
    {!Eio_net_accept.run} does); the listener itself keeps accepting.

    Raises [Invalid_argument] if [max_connections] is less than [1]. A failure
    to listen, such as an address in use, propagates from {!Eio.Net.listen}. *)
