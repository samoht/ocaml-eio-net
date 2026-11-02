(** A frame channel: a bidirectional packet link where one {!recv} is one frame
    and one {!send} puts one frame on the wire.

    Frame-based protocols such as CCSDS transfer frames, AX.25 and Ethernet read
    and write over it, independently of how the bytes move: a network interface,
    an Eio datagram socket or an in-memory pipe. A protocol codec layers over it
    by decoding what {!recv} returns and encoding into {!send}; the channel
    itself is untyped bytes. *)

type t = {
  recv : unit -> string;
  recv_batch : unit -> string list;
  recv_slice : unit -> Bytesrw.Bytes.Slice.t;
  send : Cstruct.t list -> unit;
  send_batch : Cstruct.t list list -> unit;
}
(** A frame channel.

    {!recv} is one whole received frame as a fresh string. {!recv_batch} is a
    non-empty bounded batch, read with one multi-message system call where the
    transport allows and otherwise [[recv ()]]. {!recv_slice} is one frame as a
    view (an offset and a length into owned bytes), so a decode layer strips its
    header by advancing the offset rather than copying; it is the inbound
    counterpart of {!send}'s buffers. The view is valid only until the next
    receive (bytesrw's slice contract), so a layer that retains the payload past
    that must copy it; an empty frame yields {!Bytesrw.Bytes.Slice.eod}.

    {!send} takes an outgoing frame as a list of buffers, as [Eio.Flow.write]
    does, and the frame is their concatenation. A transport that writes vectors
    hands the buffers to [writev] as they are, so a frame held in several
    buffers is not copied into one; a sender with a string passes
    [[Cstruct.of_string s]]. The buffers must not change until {!send} returns.

    {!send_batch} sends a burst of frames, coalescing them into a single
    [writev] where the transport allows (software GSO): one system call for the
    whole burst instead of one per frame. A transport that cannot coalesce (a
    datagram socket, where each frame is its own datagram) falls back to sending
    them one at a time. *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a frame channel is an I/O handle with no
    inspectable state. *)

val v : recv:(unit -> string) -> send:(Cstruct.t list -> unit) -> t
(** [v ~recv ~send] is a frame channel from a receive and a send function.
    {!recv_batch} returns one frame and {!send_batch} sends the burst one frame
    at a time; a transport that can batch overrides these operations. *)

val of_datagram_socket :
  ?max_frame:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_datagram_socket ?max_frame ?dst socket] presents a datagram socket as a
    frame channel: each datagram is one frame. Frames are sent to [dst] (omit
    for a connected socket) and received from any source, up to [max_frame]
    bytes (default 65536).

    {!recv} raises [Invalid_argument] on a datagram longer than [max_frame]
    rather than silently truncating it (the channel stays usable after the
    refusal). A protocol that needs the source address of each datagram uses the
    addressed {!Datagram} channel instead. *)

exception Error of Framing.error
(** A pure framing refusal, including a partial prefix/payload at EOF or an
    attempt to resume a write after failed/cancelled IO. *)

val of_length_prefixed_flow :
  ?framing:Framing.Config.t -> _ Eio.Flow.two_way -> t
(** Interpret {!Framing.Decoder} and {!Framing.Encoder} over [flow]. The pure
    default is an unsigned network-order uint32 length, up to 1 MiB. Every batch
    is validated before any prefix or payload is written. Reads and writes are
    independently serialised; a failed/cancelled write refuses future writes,
    since a partial frame may already be on the wire. A clean boundary EOF
    raises [End_of_file]; truncation and invalid lengths raise {!Error}.
    Buffered input and pending frames obey the pure limits. *)

val pair : unit -> t * t
(** [pair ()] is two in-memory frame channels where a frame sent on one is
    received on the other, and vice versa. For hermetic tests of a frame
    protocol without binding a socket. Blocking {!recv} suspends the calling
    fiber until a frame arrives, so both ends must run under {!Eio_main.run}. *)
