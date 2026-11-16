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
    cancellation passes through. A channel does not own its socket: closing it
    is the caller's job, and a receive or send after that raises what the socket
    raises. *)

type t
(** The type for addressed datagram channels. *)

type Eio.Exn.Backend.t +=
  | Too_long of { max_length : int }
        (** The backend error of the unclassified {!Eio.Io} error for a datagram
            longer than [max_length] bytes, received or to send. A receive
            raises it because a peer sent the datagram, so it is an I/O failure,
            not a programming error; a transport that bounds the datagrams it
            sends raises it too. *)

val too_long : max_length:int -> exn
(** [too_long ~max_length] is the {!Eio.Io} error with backend code
    [Too_long { max_length }], as {!recv} raises it. A transport that bounds its
    own datagrams raises it. *)

val of_socket :
  ?max_length:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_socket ~max_length ~dst socket] presents [socket] as an addressed
    channel. Datagrams are received from any source, up to [max_length] bytes
    (defaults to [65536]); {!send} without [~dst] sends to [dst] (omit it for a
    connected socket).

    {!recv} raises the {!Eio.Io} error {!Too_long} on a datagram longer than
    [max_length] rather than silently truncating it: a datagram socket otherwise
    reports a truncated receive as if it were complete, and a short read is
    indistinguishable from a short datagram. The channel stays usable after the
    refusal; the oversized datagram is consumed. Concurrent receives run one at
    a time, as they share one buffer; sends share nothing and run at once.

    Raises [Invalid_argument] if [max_length] is not positive. *)

val recv : t -> Eio.Net.Sockaddr.datagram * string
(** [recv c] is the next whole datagram received on [c], as a fresh string,
    together with the address it came from. *)

val send : ?dst:Eio.Net.Sockaddr.datagram -> t -> Cstruct.t list -> unit
(** [send ~dst c bufs] sends the concatenation of [bufs] as one datagram on [c]
    to [dst] (defaults to the channel's configured destination, or the connected
    peer). The buffers go to the socket as they are, one vector each, so a
    datagram held in several buffers is not copied into one. They must not
    change until [send] returns. *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a datagram channel is an I/O handle with
    no inspectable state. *)
