(** The wrappers that ask about each operation and inject the faults. *)

type Eio.Exn.Backend.t += Injected_fault

val v :
  ?bound:int ->
  clock:_ Eio.Time.clock ->
  outage:bool ->
  (Op.t -> Fault.t option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~bound ~clock ~outage f net] is [net] with [f] asked about each
    operation, as {!Eio_net_chaos.wrap} documents. [outage] is what {!outage}
    says of it. Once more than [bound] (defaults to none) connects, accepts and
    receives were made while [clock] stood still, every operation waits for
    ever, and {!overrun} says how many. *)

val pair :
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~clock f] is {!Eio_net_chaos.pair}. *)

val outage : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [outage net] is the [outage] [net] was made with by {!v}, and [false] for
    any other network. *)

val livelocked : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [livelocked net] is [true] iff an operation of [net] failed after many
    others in a row while the clock stood still. *)

val livelock_message : string
(** [livelock_message] says what {!val-livelocked} means, for a report. *)

val overrun : [ `Generic ] Eio.Net.ty Eio.Resource.t -> int option
(** [overrun net] is the connects, accepts and receives [net] made while its
    clock stood still, once past its bound. *)

val threshold : int
(** [threshold] is the failures of one operation on one target, while the clock
    stands still, after which it raises [Failure]: [10_000]. *)
