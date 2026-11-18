(** One direction of a connection: the bytes written and not yet read, and how
    its two ends see each other's shutdowns, closes and resets, as TCP or
    AF_UNIX has them under the kernel it follows. Its two ends may be used from
    two domains. *)

type timer = { now : unit -> Mtime.t; sleep_until : Mtime.t -> unit }
(** The clock a network was given: TCP's timers run on it. *)

val timer : _ Eio.Time.Mono.t -> timer
(** [timer clock] runs on [clock]. *)

type t
(** A direction. *)

exception Expired
(** Raised by a read or a write whose deadline passed while it waited. *)

val v : os:Kernel.os -> local:bool -> timer:timer option -> capacity:int -> t
(** [v ~os ~local ~timer ~capacity] queues at most [capacity] bytes, over
    AF_UNIX if [local] and TCP otherwise. A TCP direction keeps its times on
    [timer]. *)

val os : t -> Kernel.os
(** [os t] is the kernel [t] follows. *)

val local : t -> bool
(** [local t] is [true] over AF_UNIX. *)

val timed : t -> bool
(** [timed t] is [true] if [t] keeps its times on a clock. *)

val now : t -> Mtime.t
(** [now t] is the time on [t]'s clock, or the earliest time without one. *)

val wake : t -> unit
(** [wake t] wakes every waiter, which looks again. *)

val refuse : t -> unit
(** [refuse t] makes every later write raise the reset. *)

val resize : t -> int -> unit
(** [resize t n] queues at most [n] bytes from now on. *)

val write :
  ?until:(unit -> Mtime.t option) -> t -> back:t -> Cstruct.t list -> int
(** [write ~until t ~back bufs] queues what fits of [bufs] and is how many bytes
    it took, waiting while the queue is full, and until [until ()] at most,
    after which it raises {!Expired}. [back] is the other direction, which a
    lost write resets. *)

val read : ?until:(unit -> Mtime.t option) -> t -> Cstruct.t -> int
(** [read ~until t buf] takes queued bytes into [buf], waiting for some, and
    raises [End_of_file] or the reset once the stream ends. *)

val reset : t -> unit
(** [reset t] resets the reader once it has read what is queued. *)

val shut : t -> unit
(** [shut t] is the writer's FIN. *)

val shut_receive : t -> unit
(** [shut_receive t] shuts the reader's receive side. *)

val open_window : t -> unit
(** [open_window t] advertises the room the bytes a TCP reader dropped took, as
    its FIN does. *)

val refused : t -> bool
(** [refused t] is [true] once [t] refuses its writer's bytes. *)

val abort : t -> unit
(** [abort t] resets [t]: its reader reads the reset, and its writer's bytes are
    refused. *)

val receive_ended : t -> bool
(** [receive_ended t] is [true] once the reader reads end-of-file when nothing
    is queued. *)

val fin_sent : t -> unit
(** [fin_sent t] says the reader shut its send side. *)

val silence : t -> unit
(** [silence t] cuts [t] for good, as a partition does. *)

val timing : t -> (Mtime.t * Mtime.t * Mtime.t option) option
(** [timing t] is, for a silent TCP direction on a clock, when it went silent,
    when its writer last sent a byte before, and when it wrote the oldest byte
    not acknowledged. *)

val leave : t -> bool
(** [leave t] is the reader's close, and is [true] if it left bytes unread,
    which answer the writer with a reset. *)
