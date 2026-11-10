(** The design's probes: a run that keeps a timer going, the budget, and a
    download that resumes. *)

val addr : Eio.Net.Sockaddr.stream
(** [addr] is the server's address. *)

val ticking : unit -> unit
(** [ticking ()] runs a daemon that ticks every second beside a client that
    reads with no timeout. *)

val transfer : ?budget:int -> int -> int -> unit -> unit
(** [transfer ~budget n size ()] downloads [n] writes of [size] bytes. *)

val resume : ?replay:string -> ?bug:[ `None | `Stale ] -> unit -> unit
(** [resume ~replay ~bug ()] downloads 600 bytes, resuming after a reset. With
    [`Stale] the client resumes right once, then reuses that offset. *)
