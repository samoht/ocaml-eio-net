(** Bounded length-prefixed frames, independent of an IO runtime. *)

(** An unsigned network-order length, excluding its own prefix. *)
type length_field = Uint16_be | Uint32_be

(** The incomplete part of a frame. *)
type part = Prefix | Payload

(** Length refusal, invalid IO observation, incomplete frame, or write lifecycle
    refusal. Unsigned wire lengths stay exact even on a narrow native integer.
*)
type error =
  | Invalid_length of { length : int64; minimum : int; maximum : int }
  | Invalid_chunk of { length : int; requested : int }
  | Truncated of { part : part; expected : int; received : int }
  | Busy
  | Aborted

val pp_error : error Fmt.t
(** Format a framing refusal with its observed lengths. *)

val equal_error : error -> error -> bool
(** Structural equality of framing errors. *)

module Config : sig
  type t
  (** Immutable framing limits. *)

  val v :
    ?length_field:length_field ->
    ?min_length:int ->
    ?max_length:int ->
    unit ->
    t
  (** Network-order uint32 by default, minimum zero, maximum 1 MiB (clamped to
      the selected field's capacity and the platform's string limit).
      @raise Invalid_argument on an invalid or unrepresentable range. *)

  val length_field : t -> length_field
  (** The selected unsigned length representation. *)

  val min_length : t -> int
  (** The smallest accepted payload length. *)

  val max_length : t -> int
  (** The largest accepted payload length. *)

  val prefix_length : t -> int
  (** The number of length-prefix bytes. *)

  val buffer_size : t -> int
  (** Maximum payload plus prefix: a bound for an interpreting buffered reader.
  *)

  val pp : t Fmt.t
  (** Format the length representation and accepted range. *)

  val equal : t -> t -> bool
  (** Structural equality of configurations. *)
end

val encode : Config.t -> Rope.t -> (Rope.t, error) result
(** Validate and prepend the unsigned length without copying payload leaves. *)

val encode_batch : Config.t -> Rope.t list -> (Rope.t, error) result
(** Validate every frame before returning any encoded batch. Failure emits no
    bytes, and does not consume or change any supplied payload. *)

module Decoder : sig
  type t
  (** One bounded in-progress frame and its terminal outcome. *)

  (** Complete or partial read, final bytes at EOF, or failed/cancelled IO. Each
      read supplies at most the most recently requested number of bytes.
      [Data ""] is refused; an empty final read is [Eof ""]. *)
  type input = Data of string | Eof of string | Abort

  (** The next read instruction, accepted frame, or terminal outcome. *)
  type event = Read of int | Frame of string | End_of_stream | Failed of error

  val v : Config.t -> t
  (** Start a fresh framed-stream direction. *)

  val handle : t -> input -> t
  (** Feed an observed read. A refused prefix never requests its payload.
      Terminal failure cannot be resumed with more input. *)

  val next_event : t -> t * event
  (** Consume one completed frame, report the next exact read, or report the
      terminal outcome. Every {!constructor-Read} is strictly positive. Accepted
      frames are delivered before a subsequent EOF or abort. *)

  val pending_bytes : t -> int
  (** Bytes retained in the current prefix or frame, bounded by
      {!Config.buffer_size}; failure releases incomplete input. *)
end

module Encoder : sig
  type t
  (** One outstanding write and its terminal outcome. *)

  (** The physical write completed, or failed/cancelled after it started. *)
  type completion = Sent | Failed

  val v : Config.t -> t
  (** Start a fresh framed-stream direction. *)

  val outgoing : t -> Rope.t list -> t * (Rope.t, error) result
  (** Prepare one validated batch. Only one may be outstanding; rejection leaves
      the current state unchanged. *)

  val complete : t -> completion -> t
  (** Complete the outstanding write exactly once. A failed/cancelled write
      permanently refuses further output, since a partial prefix or payload may
      already have reached the peer. Completion cannot reopen a closed writer.
  *)

  val pending_bytes : t -> int
  (** Encoded bytes of the one outstanding write. *)
end
