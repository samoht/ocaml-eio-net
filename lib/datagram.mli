(** An addressed datagram channel: one {!recv} is one whole datagram together
    with its source address, and one {!send} puts one datagram on the wire
    toward an explicit or the configured destination.

    Datagram protocols that need the peer address use it: a UDP server that
    answers whoever wrote to it, or a receiver that locks onto the peer that
    opened an exchange. A protocol that does not care about addresses uses the
    source-blind {!Frame} channel instead. *)

type t = {
  recv : unit -> Eio.Net.Sockaddr.datagram * string;
  send : Eio.Net.Sockaddr.datagram option -> Rope.t -> unit;
}
(** An addressed datagram channel.

    {!recv} is one whole received datagram as a fresh string, together with the
    address it came from.

    [send (Some dst) rope] sends one datagram to [dst]; [send None rope] sends
    to the channel's configured destination (or the connected peer). The payload
    travels as a {!Rope.t} so a zero-copy rope leaf reaches the wire through
    [writev] without being concatenated into a whole-datagram string. *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a datagram channel is an I/O handle with
    no inspectable state. *)

val of_socket :
  ?max_frame:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_socket ?max_frame ?dst socket] presents a datagram socket as an
    addressed channel. Datagrams are received from any source, up to [max_frame]
    bytes (default 65536); [send None] sends to [dst] (omit [dst] for a
    connected socket).

    {!recv} raises [Invalid_argument] on a datagram longer than [max_frame]
    rather than silently truncating it: a datagram socket otherwise reports a
    truncated receive as if it were complete, and a short read is
    indistinguishable from a short datagram. The channel stays usable after the
    refusal; the oversized datagram is consumed.

    Raises [Invalid_argument] if [max_frame] is not positive. *)

val equal_addr : Eio.Net.Sockaddr.datagram -> Eio.Net.Sockaddr.datagram -> bool
(** [equal_addr a b] is [true] iff [a] and [b] are the same datagram address:
    equal IP address and port, or equal Unix socket path. A receiver that locks
    onto the source of a datagram compares later sources with this. *)
