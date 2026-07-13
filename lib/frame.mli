(** A frame channel: a bidirectional packet link where one {!recv} is one frame
    and one {!send} puts one frame on the wire.

    This is the carrier that frame-based protocols read and write over -- CCSDS
    transfer frames, AX.25, Ethernet -- independent of how the bytes actually
    move: a netif, an Eio datagram socket, or an in-memory pipe. A protocol
    codec layers over it by decoding what {!recv} returns and encoding into
    {!send}; the channel itself is untyped bytes. *)

type t = {
  recv : unit -> string;
  recv_slice : unit -> Bytesrw.Bytes.Slice.t;
  send : Rope.t -> unit;
  send_batch : Rope.t list -> unit;
}
(** A frame channel.

    {!recv} returns one whole received frame as a fresh string. {!recv_slice}
    returns it as a view (offset + length into owned bytes) so a decode layer
    strips its header by advancing the offset rather than copying -- the inbound
    dual of {!send}'s rope. The view is valid only until the next receive
    (bytesrw's slice contract), so a layer that retains the payload past that
    must copy it; an empty frame yields {!Bytesrw.Bytes.Slice.eod}.

    {!send} takes an outgoing frame as a {!Rope.t} so a payload carried as a
    zero-copy rope leaf reaches the wire through [writev] without being
    concatenated into a whole-frame string; a sender that already has a string
    wraps it with [Rope.of_string] (O(1)).

    {!send_batch} sends a burst of frames, coalescing them into a single
    [writev] where the transport allows (software GSO) -- one syscall for the
    whole burst instead of one per frame. A transport that cannot coalesce (a
    datagram socket, where each frame is its own datagram) falls back to sending
    them one at a time. *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a frame channel is an I/O handle with no
    inspectable state. *)

val v : recv:(unit -> string) -> send:(Rope.t -> unit) -> t
(** [v ~recv ~send] is a frame channel from a receive and a send function.
    {!send_batch} sends the burst one frame at a time; a transport that can
    coalesce builds its channel through {!of_length_prefixed_flow} instead. *)

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

val of_length_prefixed_flow :
  ?max_frame:int -> ?prefix_bytes:int -> _ Eio.Flow.two_way -> t
(** [of_length_prefixed_flow flow] presents a byte-stream flow as a frame
    channel by delimiting each frame with a big-endian length prefix of
    [prefix_bytes] bytes (default 4): {!send} writes the length then the
    payload, and {!recv} reads one length then exactly that many payload bytes.
    Use [prefix_bytes:2] for a 16-bit prefix (e.g. DNS-over-TCP, a utun link).

    {!send_batch} coalesces a burst into one [writev] -- each frame's prefix and
    rope leaves back to back -- so a sender that produces frames in bursts pays
    one syscall per burst (software GSO).

    {!recv} raises [End_of_file] when the stream closes at a frame boundary (or
    mid-frame), and [Invalid_argument] if a length prefix exceeds [max_frame]
    (default 1 MiB), so a hostile peer cannot force an unbounded read. A frame
    may be empty. *)

val pair : unit -> t * t
(** [pair ()] is two in-memory frame channels where a frame sent on one is
    received on the other, and vice versa. For hermetic tests of a frame
    protocol without binding a socket. Blocking {!recv} suspends the calling
    fiber until a frame arrives, so both ends must run under {!Eio_main.run}. *)
