(** A frame channel: a bidirectional packet link where one {!recv} is one frame
    and one {!send} puts one frame on the wire.

    Frame-based protocols such as CCSDS transfer frames, AX.25 and Ethernet read
    and write over it, independently of how the bytes move: a network interface,
    an Eio datagram socket or an in-memory pipe. A protocol codec layers over it
    by decoding what {!recv} returns and encoding into {!send}; the channel
    itself is untyped bytes.

    {b Errors and ownership.} What a receive raises depends on the transport:
    over a datagram socket, the {!Eio.Io} error of a failed receive, the
    {!Datagram.Too_long} one among them, and never [End_of_file], since a
    datagram socket has no end; over a length-prefixed flow, [End_of_file] at
    the end of input at a frame boundary, [Eio_net_framing.Error] for a frame
    the input cuts short or a length out of range, and the {!Eio.Io} error of
    the flow; over a {!pair}, nothing, since a pair has no end-of-file: a
    receive waits until a frame arrives, and a fiber waiting on a peer that has
    gone is cancelled to end it. A send raises the {!Eio.Io} error of the
    transport, and over a length-prefixed flow [Eio_net_framing.Error] as
    {!of_length_prefixed_flow} says. A cancellation passes through every
    transport. A channel does not own its flow or socket: it never closes it,
    closing it is the caller's, and a receive or send after that raises what the
    flow or socket raises. *)

type t
(** The type for frame channels. *)

(** {1:channels Channels} *)

val v :
  ?recv_batch:(unit -> string list) ->
  ?recv_slice:(unit -> Bytesrw.Bytes.Slice.t) ->
  ?send_batch:(Cstruct.t list list -> unit) ->
  recv:(unit -> string) ->
  send:(Cstruct.t list -> unit) ->
  unit ->
  t
(** [v ~recv_batch ~recv_slice ~send_batch ~recv ~send ()] is a frame channel
    whose {!recv} calls [recv], whose {!send} calls [send], and with:
    - {!recv_batch} calling [recv_batch] (defaults to [[recv ()]], one frame).
    - {!recv_slice} calling [recv_slice] (defaults to a slice of a copy of
      [recv ()], since the string [recv] returns may be held elsewhere).
    - {!send_batch} calling [send_batch] (defaults to sending the frames one at
      a time with [send]).

    A transport that can batch, or that owns the bytes it receives, passes the
    optional functions. They must keep the contracts of {!recv_batch} and
    {!recv_slice}: a batch is non-empty, a slice stays valid until the next
    receive, and the slice of an empty frame is {!Bytesrw.Bytes.Slice.eod}. *)

val of_datagram_socket :
  ?max_frame:int ->
  ?dst:Eio.Net.Sockaddr.datagram ->
  _ Eio.Net.datagram_socket ->
  t
(** [of_datagram_socket ~max_frame ~dst socket] presents [socket] as a frame
    channel: each datagram is one frame. Frames are sent to [dst] (omit it for a
    connected socket) and received from any source, up to [max_frame] bytes
    (defaults to [65536]). {!send_batch} sends its frames one datagram at a
    time.

    {!recv}, {!recv_batch} and {!recv_slice} raise the {!Eio.Io} error
    {!Datagram.Too_long} on a datagram longer than [max_frame] rather than
    silently truncating it; the oversized datagram is consumed and the channel
    stays usable. Concurrent receives run one at a time, as
    {!Datagram.of_socket} runs them; sends run at once. A protocol that needs
    the source address of each datagram uses the addressed {!Datagram} channel
    instead.

    Raises [Invalid_argument] if [max_frame] is not positive. *)

val of_length_prefixed_flow :
  ?framing:Eio_net_framing.Config.t -> _ Eio.Flow.two_way -> t
(** [of_length_prefixed_flow ~framing flow] presents a byte stream as a frame
    channel: each frame is a length prefix and a payload, as {!Eio_net_framing}
    reads and writes them with [framing] (defaults to
    [Eio_net_framing.Config.v ()]: a uint32 network-order length, payloads up to
    1 MiB). It buffers at most {!Eio_net_framing.Config.buffer_size} bytes of
    input. Receives are serialised among themselves, and so are sends, so a
    batch reaches the wire whole even when fibers send concurrently.

    {!recv}, {!recv_batch} and {!recv_slice} raise [End_of_file] at the end of
    input at a frame boundary, and [Eio_net_framing.Error] if the input ends
    inside a frame ([Truncated _]) or a prefix declares a length outside the
    range of [framing] ([Invalid_length _]). After such an error, or any
    exception that interrupts a frame after part of it was consumed (a
    cancellation), every later receive raises [Eio_net_framing.Error] with that
    error, or [Aborted] for another exception, rather than read the middle of a
    frame as a prefix.

    {!send} and {!send_batch} write a batch with one [Eio.Flow.write]. They
    raise [Eio_net_framing.Error (Invalid_length _)] before writing anything if
    a payload's length is outside the range of [framing], and the channel stays
    usable. Once a write has raised, part of a frame may be on the wire, so
    every later send raises [Eio_net_framing.Error Aborted]. *)

val pair : unit -> t * t
(** [pair ()] is two in-memory frame channels where a frame sent on one is
    received on the other, in order, and vice versa. Each direction queues
    without bound: a send copies the frame and never blocks. A receive suspends
    the calling fiber until a frame arrives, so the ends run under an Eio
    scheduler, on one domain or two. *)

(** {1:receiving Receiving} *)

val recv : t -> string
(** [recv c] is the next whole frame received on [c], as a fresh string. *)

val recv_batch : t -> string list
(** [recv_batch c] is a non-empty bounded batch of the next frames received on
    [c], in order, read with one multi-message system call where the transport
    allows. Otherwise it is [[recv c]]. *)

val recv_slice : t -> Bytesrw.Bytes.Slice.t
(** [recv_slice c] is the next frame received on [c] as a view: an offset and a
    length into owned bytes, so a decode layer strips its header by advancing
    the offset rather than copying. It is the inbound counterpart of {!send}'s
    buffers. The slice is {{!Bytesrw.Bytes.Slice.validity}valid} only until the
    next receive on [c], so a layer that retains the payload past that must copy
    it. An empty frame is {!Bytesrw.Bytes.Slice.eod}. *)

(** {1:sending Sending} *)

val send : t -> Cstruct.t list -> unit
(** [send c bufs] sends one frame on [c], the concatenation of [bufs], as
    [Eio.Flow.write] takes its buffers. A transport that writes vectors hands
    [bufs] to [writev] as they are, so a frame held in several buffers is not
    copied into one: a length-prefixed flow over a socket and an Eio datagram
    socket do, while a {!pair} copies each frame, and so does
    [Eio_net_unix.Frame], into its batch buffers. A sender with a string [s]
    passes [[Cstruct.of_string s]]. The buffers must not change until [send]
    returns. *)

val send_batch : t -> Cstruct.t list list -> unit
(** [send_batch c frames] sends the frames of [frames] on [c], in order, each
    frame the concatenation of its buffers as with {!send}, in as few system
    calls as the transport allows: a stream transport writes the whole batch
    with one vectored write, and a datagram transport sends one datagram per
    frame, several in one system call where it can. If [frames] is empty,
    nothing is sent. The buffers must not change until [send_batch] returns. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] prints an opaque placeholder: a frame channel is an I/O handle with no
    inspectable state. *)
