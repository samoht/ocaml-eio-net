(** The wrappers that ask about each operation and inject the faults. *)

type Eio.Exn.Backend.t += Injected_fault

val v :
  ?max_ops:int ->
  ?os:[< `Linux | `Macos ] ->
  ?split:(Op.t -> bool) ->
  ?charge:(unit -> unit) ->
  clock:_ Eio.Time.clock ->
  outage:bool ->
  (Op.t -> Fault.t option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~max_ops ~os ~charge ~clock ~outage f net] is [net] with [f] asked about
    each operation, as {!Eio_net_chaos.wrap} documents. [outage] is what
    {!outage} says of it. Each network operation, a listen, a send, a lookup or
    a close included, first calls [charge] (defaults to none). Once [max_ops]
    (defaults to none) operations [f] was asked about were made, every operation
    waits for ever, and {!overrun} holds. Those of which [split] (defaults to
    none) holds do not count: {!splits} does. *)

val pair :
  ?os:[< `Linux | `Macos ] ->
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~os ~clock f] is {!Eio_net_chaos.pair}. *)

val outage : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [outage net] is the [outage] [net] was made with by {!v}, and [false] for
    any other network. *)

val livelocked : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [livelocked net] is [true] iff an operation of [net] failed after many
    others in a row while the clock stood still. *)

val livelock_message : string
(** [livelock_message] says what {!val-livelocked} means, for a report. *)

val overrun : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [overrun net] is [true] iff [net] reached its [max_ops]. *)

val splits : [ `Generic ] Eio.Net.ty Eio.Resource.t -> int
(** [splits net] is the operations of [net] its [split] held of. *)

val activity :
  [ `Generic ] Eio.Net.ty Eio.Resource.t ->
  Eio.Private.Trace.id ->
  Op.kind option * Op.t option
(** [activity net tid] is, for the fiber of trace id [tid], the kind of the
    operation of [net] it is in, if any, and the last one it made, if any. Only
    a network made with a [charge] keeps them. *)

val threshold : int
(** [threshold] is the failures of one operation on one target, while the clock
    stands still, after which it raises [Failure]: [10_000]. *)
