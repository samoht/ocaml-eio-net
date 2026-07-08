(** A frame channel: a bidirectional packet link where one {!recv} is one frame
    and one {!send} puts one frame on the wire.

    This is the carrier that frame-based protocols read and write over -- CCSDS
    transfer frames, AX.25, Ethernet -- independent of how the bytes actually
    move: a netif, an Eio datagram socket, or an in-memory pipe. A protocol
    codec layers over it by decoding what {!recv} returns and encoding into
    {!send}; the channel itself is untyped bytes. *)

type t = { recv : unit -> string; send : Rope.t -> unit }
(** A frame channel. {!recv} returns one whole received frame; {!send} takes an
    outgoing frame as a {!Rope.t} so a payload carried as a zero-copy rope leaf
    reaches the wire without being concatenated into a whole-frame string. A
    sender that already has a string wraps it with [Rope.of_string] (O(1)). *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a frame channel is an I/O handle with no
    inspectable state. *)

val v : recv:(unit -> string) -> send:(Rope.t -> unit) -> t
(** [v ~recv ~send] is a frame channel from a receive and a send function. *)

val of_datagram_socket :
  ?max_frame:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_datagram_socket ?max_frame ?dst socket] presents a datagram socket as a
    frame channel: each datagram is one frame. Frames are sent to [dst] (omit
    for a connected socket) and received from any source, up to [max_frame]
    bytes (default 65536). *)

val of_length_prefixed_flow : ?max_frame:int -> _ Eio.Flow.two_way -> t
(** [of_length_prefixed_flow flow] presents a byte-stream flow as a frame
    channel by delimiting each frame with a 4-byte big-endian length prefix:
    {!send} writes the length then the payload, and {!recv} reads one length
    then exactly that many payload bytes.

    {!recv} raises [End_of_file] when the stream closes at a frame boundary (or
    mid-frame), and [Invalid_argument] if a length prefix exceeds [max_frame]
    (default 1 MiB), so a hostile peer cannot force an unbounded read. A frame
    may be empty. *)

val pair : unit -> t * t
(** [pair ()] is two in-memory frame channels where a frame sent on one is
    received on the other, and vice versa. For hermetic tests of a frame
    protocol without binding a socket. Blocking {!recv} suspends the calling
    fiber until a frame arrives, so both ends must run under {!Eio_main.run}. *)
