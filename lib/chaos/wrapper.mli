(** The wrappers that ask about each operation and inject the faults. *)

type Eio.Exn.Backend.t += Injected_fault

val v :
  ?os:[< `Linux | `Macos ] ->
  ?chaos:Eio_chaos.t ->
  clock:_ Eio.Time.clock ->
  outage:bool ->
  (Op.t -> (Fault.t * int) option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~os ~chaos ~clock ~outage f net] is [net] with [f] asked about each
    operation, as {!Eio_net_chaos.wrap} documents. [f] answers a fault and the
    bytes of a read or a write that pass before it: the operation moves those,
    and the fault falls at the next one of that end. [outage] is what
    {!is_outage} says of it. With [chaos], the network of a run: each network
    operation, a listen, a send, a lookup or a close included, takes a turn of
    the run; a hang report names the operation a fiber waits in; a read or a
    write records its {!Eio_chaos.places}; and an operation that keeps failing
    while the clock stands still stops the run as a hang. *)

val pair :
  ?os:[< `Linux | `Macos ] ->
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~os ~clock f] is {!Eio_net_chaos.pair}. *)

val is_outage : [ `Generic ] Eio.Net.ty Eio.Resource.t -> bool
(** [is_outage net] is the [outage] [net] was made with by {!v}, and [false] for
    any other network. *)
