(** The wrappers that ask about each operation and inject the faults. *)

type Eio.Exn.Backend.t += Injected_fault

val v :
  clock:_ Eio.Time.clock ->
  outage:bool ->
  (Op.t -> Fault.t option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~clock ~outage f net] is [net] with [f] asked about each operation, as
    {!Eio_net_chaos.wrap} documents. [outage] is what {!outage} says of it. *)

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
