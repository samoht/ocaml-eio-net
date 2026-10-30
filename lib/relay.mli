(** A TCP relay: splice two flows together, or listen and forward each accepted
    connection to another address.

    It serves port forwarding, such as a local proxy in front of a remote
    service or a test harness standing between a client and a server. It knows
    nothing about what travels through it: bytes are copied both ways until each
    side finishes. *)

val splice : _ Eio.Flow.two_way -> _ Eio.Flow.two_way -> unit
(** [splice a b] copies both directions concurrently until each side reaches
    end-of-stream. A finished direction propagates as a half-close
    ([Eio.Flow.shutdown `Send] on the other flow), so a peer that reads until
    end-of-file sees it as soon as its counterpart is done writing. The call
    returns when both directions are done. *)

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
    [listen] and, for every accepted connection, dials [connect] and {!splice}s
    the two, in an {!Eio_net_accept.run} loop on [sw] that relays at most
    [max_connections] connections at once: a client past them waits, holding no
    upstream connection, until one ends. A relay serves from the accept, so it
    sheds none. It is the bound address once listening (for a [listen] naming
    port 0, the port the system chose), and it serves until [sw] ends.

    A connection that fails on a socket (the dial refused, either side dropping
    mid-splice) closes that connection's flows and is reported to [on_error]
    (default: a warning on this module's log source); the listener itself keeps
    accepting. *)
