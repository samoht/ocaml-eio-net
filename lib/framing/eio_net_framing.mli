(** Length-prefixed frames over Eio flows.

    A frame is an unsigned network-order length followed by that many payload
    bytes; the length excludes its own prefix. [Uint16_be] is the LENGTH field
    of
    {{:https://www.rfc-editor.org/rfc/rfc4571.html#section-2}RFC 4571 section 2},
    zero-length frame included, and [Uint32_be] is the default. What a payload
    means belongs to the protocol above.

    A {{!Config}configuration} bounds the payload lengths a stream accepts, so a
    reader made with [~max_size:(Config.buffer_size c)] holds at most that many
    bytes, whatever length a peer declares. {!read} takes one frame from an
    {!Eio.Buf_read.t}, which reads its flow until the frame is whole. {!encode}
    lays out a batch of frames for one vectored write without copying the
    payloads, and {!write} writes it. Neither keeps state between calls:
    [Eio_net.Frame.of_length_prefixed_flow] adds the serialisation and the
    failure state a stream needs. *)

(** {1:errors Length fields and errors} *)

(** The type for length fields: an unsigned network-order (big-endian) integer.
*)
type length_field =
  | Uint16_be  (** Two bytes, lengths up to [65535]. *)
  | Uint32_be  (** Four bytes, lengths up to [4294967295]. *)

(** The type for the parts of a frame. *)
type part =
  | Prefix  (** The length field. *)
  | Payload  (** The bytes the length field counts. *)

(** The type for framing errors. *)
type error =
  | Invalid_length of {
      length : int64;
          (** The length: declared by a prefix, unsigned and exact, or of a
              payload to encode. *)
      minimum : int;  (** The configuration's {!Config.min_length}. *)
      maximum : int;  (** The configuration's {!Config.max_length}. *)
    }  (** A length outside the configured range. *)
  | Truncated of {
      part : part;  (** The part the input ends in. *)
      expected : int;  (** The length of that part, in bytes. *)
      received : int;  (** The bytes of that part the input holds. *)
    }  (** Input that ends inside a frame. *)
  | Aborted
      (** A read or a write failed partway through a frame, so the stream
          refuses further reads or writes in that direction. The functions of
          this module never return or raise it; a stream such as
          [Eio_net.Frame.of_length_prefixed_flow] does. *)

val pp_error : error Fmt.t
(** [pp_error] formats an error with the lengths it carries. *)

val equal_error : error -> error -> bool
(** [equal_error e0 e1] is [true] iff [e0] and [e1] are the same error with the
    same lengths. *)

exception Error of error
(** The exception for the errors raised by {!read} and {!write}. *)

(** {1:config Configurations} *)

(** Framing configurations. *)
module Config : sig
  type t
  (** The type for framing configurations: a length field and the inclusive
      range of payload lengths a stream accepts. *)

  val v :
    ?length_field:length_field ->
    ?min_length:int ->
    ?max_length:int ->
    unit ->
    t
  (** [v ~length_field ~min_length ~max_length ()] is the configuration of
      frames whose length is a [length_field] (defaults to
      {!constructor-Uint32_be}) and whose payloads are [min_length] (defaults to
      [0]) to [max_length] (defaults to the least of 1 MiB, the capacity of
      [length_field] and [Sys.max_string_length], so [1048576] for
      {!constructor-Uint32_be} and [65535] for {!constructor-Uint16_be}) bytes
      long.

      Raises [Invalid_argument] if [min_length] is negative, if [max_length] is
      below [min_length], or if [max_length] exceeds the capacity of
      [length_field] or [Sys.max_string_length]. *)

  val length_field : t -> length_field
  (** [length_field c] is the length field of [c]. *)

  val min_length : t -> int
  (** [min_length c] is the shortest payload [c] accepts, in bytes. *)

  val max_length : t -> int
  (** [max_length c] is the longest payload [c] accepts, in bytes. *)

  val prefix_length : t -> int
  (** [prefix_length c] is the length of the prefix of [c], in bytes: [2] for
      {!constructor-Uint16_be} and [4] for {!constructor-Uint32_be}. *)

  val buffer_size : t -> int
  (** [buffer_size c] is [max_length c + prefix_length c], the longest frame [c]
      accepts. It is the [~max_size] to give {!Eio.Buf_read.of_flow} for
      {!read}. *)

  val equal : t -> t -> bool
  (** [equal c0 c1] is [true] iff [c0] and [c1] have the same length field and
      range. *)

  val pp : t Fmt.t
  (** [pp] formats a configuration as its length field and its range, as in
      [uint32be length:0-1048576]. *)
end

(** {1:reading Reading} *)

val read : Config.t -> Eio.Buf_read.t -> string
(** [read c r] is the payload of the next frame of [r], a frame of [c]. It reads
    the flow of [r] until the frame is whole and consumes the frame from [r],
    and nothing past it. The payload of a zero-length frame is [""]. [read c] is
    an {!Eio.Buf_read.parser}.

    It raises:
    - [End_of_file] if the input of [r] ends before the first byte of a frame: a
      clean end at a frame boundary.
    - [Error (Truncated _)] if the input of [r] ends inside a frame. [expected]
      is the length of the prefix or of the payload the input ends in and
      [received] the bytes of it the input holds, which [read] consumes from
      [r].
    - [Error (Invalid_length _)] as soon as the prefix declares a length outside
      the range of [c]. [r] has consumed the prefix, and read its flow only
      until the prefix was whole: no payload byte is requested.
    - [Eio.Buf_read.Buffer_limit_exceeded] if a payload is longer than the
      maximum size of [r]. A reader whose maximum size is at least
      {!Config.buffer_size}[ c] never raises it.

    An exception from the flow of [r], cancellation included, propagates. If it
    interrupts a payload, [r] has consumed its prefix, so a later [read] would
    take payload bytes for a prefix. *)

(** {1:writing Writing} *)

val encode :
  ?prefixes:Cstruct.t ->
  Config.t ->
  Cstruct.t list list ->
  (Cstruct.t list, error) result
(** [encode ~prefixes c batch] is the frames of the payloads of [batch], in
    order, as one list of buffers for a vectored write. A payload is the
    concatenation of its buffers, and each payload follows its length prefix.
    The prefixes of the batch share one buffer: [prefixes] (defaults to a new
    buffer) if it holds at least [Config.prefix_length c] bytes per payload, a
    new one otherwise. Neither [prefixes] nor the payload buffers are copied, so
    none of them may change until the result is written. An empty payload [[]]
    is a frame of its prefix alone, and the empty batch is [Ok []].

    This is [Error (Invalid_length _)] for the first payload of [batch] whose
    length is outside the range of [c], and then no frame of [batch] is encoded.
*)

val write :
  ?prefixes:Cstruct.t ->
  Config.t ->
  _ Eio.Flow.sink ->
  Cstruct.t list list ->
  unit
(** [write ~prefixes c sink batch] writes the frames of
    [encode ~prefixes c batch] to [sink] with one {!Eio.Flow.write}. The empty
    batch writes nothing.

    Raises [Error (Invalid_length _)] before writing anything if
    [encode c batch] is an error. An exception from [sink] propagates, and part
    of the batch may then have been written. *)
