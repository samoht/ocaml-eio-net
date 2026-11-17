(** A source that hands out its chunks one at a time, so that a reader sees its
    input split exactly at the chunk boundaries. *)

val split : int list -> string -> string list
(** [split sizes s] cuts [s] into chunks whose lengths cycle through [sizes],
    which are positive. *)

val reader : Eio_net_framing.Config.t -> string list -> Eio.Buf_read.t
(** [reader config chunks] reads [chunks] one at a time, skipping empty ones,
    with a buffer of [Eio_net_framing.Config.buffer_size config] bytes. *)
