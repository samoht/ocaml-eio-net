(** An addressed datagram channel: one {!recv} is one whole datagram together
    with its source address, and one {!send} puts one datagram on the wire
    toward an explicit or the configured destination.

    Datagram protocols that need the peer address use it: a UDP server that
    answers whoever wrote to it, or a receiver that locks onto the peer that
    opened an exchange. A protocol that does not care about addresses uses the
    source-blind {!Frame} channel instead.

    {b Errors and ownership.} A receive or a send raises the {!Eio.Io} error of
    the socket, {!Too_long} among them for a datagram longer than the channel
    takes, and never [End_of_file], since a datagram socket has no end. A
    cancellation passes through. A channel does not own its socket: it never
    closes it, closing it is the caller's, and a receive or send after that
    raises what the socket raises. *)

type t
(** The type for addressed datagram channels. *)

type Eio.Exn.Backend.t +=
  | Too_long of { max : int }
        (** The backend error of the unclassified {!Eio.Io} error a receive
            raises on a datagram longer than [max] bytes, the channel's
            [max_frame]: a peer sent it, so it is an I/O failure, not a
            programming error. *)

val of_socket :
  ?max_frame:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_socket ~max_frame ~dst socket] presents [socket] as an addressed
    channel. Datagrams are received from any source, up to [max_frame] bytes
    (defaults to [65536]); {!send} without [~dst] sends to [dst] (omit it for a
    connected socket).

    {!recv} raises the {!Eio.Io} error {!Too_long} on a datagram longer than
    [max_frame] rather than silently truncating it: a datagram socket otherwise
    reports a truncated receive as if it were complete, and a short read is
    indistinguishable from a short datagram. The channel stays usable after the
    refusal; the oversized datagram is consumed. Concurrent receives run one at
    a time, as they share one buffer; sends share nothing and run at once.

    Raises [Invalid_argument] if [max_frame] is not positive. *)

val recv : t -> Eio.Net.Sockaddr.datagram * string
(** [recv c] is the next whole datagram received on [c], as a fresh string,
    together with the address it came from. *)

val send : t -> ?dst:Eio.Net.Sockaddr.datagram -> Cstruct.t list -> unit
(** [send c ~dst bufs] sends the concatenation of [bufs] as one datagram on [c]
    to [dst] (defaults to the channel's configured destination, or the connected
    peer). The buffers go to the socket as they are, one vector each, so a
    datagram held in several buffers is not copied into one. They must not
    change until [send] returns. *)

val equal_addr : Eio.Net.Sockaddr.datagram -> Eio.Net.Sockaddr.datagram -> bool
(** [equal_addr a b] is [true] iff [a] and [b] are the same datagram address:
    equal IP address and port, or equal Unix socket path. A receiver that locks
    onto the source of a datagram compares later sources with this. *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a datagram channel is an I/O handle with
    no inspectable state. *)
