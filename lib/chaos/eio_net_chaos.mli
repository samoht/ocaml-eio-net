(** Adversarial network capabilities for testing Eio protocol adapters.

    Wraps an ordinary Eio flow, datagram socket, or {!Eio.Net.t} and injects
    read, write, datagram and connect faults on a deterministic, inspectable
    schedule: a test either pins an exact scenario or drives a seeded one and
    replays it byte for byte. The in-memory transport is reused from
    {!Eio_net_mem}; this layer only adds the adversarial behaviour. *)

type Eio.Exn.Backend.t +=
  | Injected_fault
        (** The backend code carried by every connection error this layer
            raises, so an injected [Connection_reset] or [Connection_failure] is
            an {!Eio.Io} error of the same shape a real socket would raise. *)

(** Adversarial stream flows: dribbled and coalesced reads, stalls, short
    writes, and a mid-stream reset or end-of-file. *)
module Flow : sig
  type read =
    | Deliver of int
        (** Hand up at most [n] buffered bytes on this read ([n <= 0] means all
            buffered): the dribble / short-read fault. *)
    | Coalesce of int
        (** Do [n] base reads, accumulate them, then hand the whole buffer up:
            several transport reads become one application read. Stops early at
            end-of-file. *)
    | Stall of int
        (** {!Eio.Fiber.yield} [n] times, then deliver all buffered bytes. A
            cooperative stall: the scheduler runs other fibers in between, so a
            reader that stalls with no other runnable work trips
            {!Eio_mock.Backend}'s deadlock detector. *)
    | Reset  (** Raise a connection-reset instead of delivering. *)
    | Eof
        (** End the read direction: this read and every later one raise
            [End_of_file]. *)

  type write =
    | Accept of int
        (** Accept at most [n] of the offered bytes on this write ([n <= 0]
            means all): the short-write fault. The accepted bytes reach the
            peer; the caller sees the short count and must retry the rest. *)
    | Fail  (** Raise a connection-reset instead of writing. *)

  (** A flow's fault schedule: pass-through, an exact per-call script, or a
      seeded generator. The value is inspectable ({!Schedule.pp}) so a test can
      print exactly what it pinned or fuzzed. *)
  module Schedule : sig
    type t

    val none : t
    (** [none] injects no faults: reads and writes pass straight through. *)

    val script : ?reads:read list -> ?writes:write list -> unit -> t
    (** [script ~reads ~writes ()] uses [reads.(i)] on the [i]th read and
        [writes.(i)] on the [i]th write, passing through once a list is
        exhausted. This pins an exact scenario. *)

    val seeded :
      ?dribble:int ->
      ?stall:int ->
      ?short_write:int ->
      ?reset_after:int ->
      ?reset_kind:[ `Reset | `Eof ] ->
      ?fail_write_after:int ->
      int ->
      t
    (** [seeded seed] derives faults from [seed]. Each read is capped at
        [1..dribble] bytes (default: uncapped) or, with probability driven by
        the seed, stalled [1..stall] scheduler turns. Each write is capped at
        [1..short_write] bytes. After [reset_after] delivered bytes the next
        read faults ([reset_kind], default [`Reset]); after [fail_write_after]
        written bytes the next write raises a reset. Read faults and write
        faults each depend only on [seed], not on how reads and writes
        interleave. *)

    val pp : t Fmt.t
    (** [pp] prints the schedule's kind and parameters. *)
  end

  type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  (** A wrapped stream socket, usable anywhere a {!Eio.Flow.two_way} or
      {!Eio.Net.stream_socket} is expected. *)

  val wrap : Schedule.t -> _ Eio.Net.stream_socket -> flow
  (** [wrap schedule sock] is [sock] with [schedule]'s faults injected on its
      reads and writes. Shutdown, close, and socket options pass through to
      [sock]. *)

  val pair : ?client:Schedule.t -> ?server:Schedule.t -> unit -> flow * flow
  (** [pair ~client ~server ()] is a connected in-memory flow pair (reusing
      {!Eio_net_mem}'s transport) with the client end wrapped by [client] and
      the server end by [server] (each {!Schedule.none} by default). Bytes
      written on one arrive on the other. Both ends must run under an Eio event
      loop ({!Eio_mock.Backend.run} suffices). *)
end

(** Adversarial datagram sockets: loss, duplication, reordering, truncation,
    garbage injection, and a stalled receive, all on the incoming path. *)
module Datagram : sig
  type fault =
    | Pass  (** Deliver the next datagram unchanged. *)
    | Drop of int
        (** Drop [n] datagrams from the base, then deliver the one after them.
        *)
    | Dup  (** Deliver the next datagram now and again on the following recv. *)
    | Reorder
        (** Hold the next datagram back and deliver the one after it first; the
            held datagram surfaces on the following recv. *)
    | Truncate of int
        (** Deliver only the first [n] bytes of the next datagram. *)
    | Garble of int
        (** Deliver [n] seed-derived bytes in place of the next datagram. *)
    | Delay of int
        (** {!Eio.Fiber.yield} [n] times, then deliver the next datagram. *)

  (** A datagram socket's recv-path fault schedule. *)
  module Schedule : sig
    type t

    val none : t
    (** [none] injects no faults. *)

    val script : fault list -> t
    (** [script events] applies [events.(i)] on the [i]th receive, passing
        through once exhausted. Pins an exact scenario. *)

    val seeded :
      ?loss:float ->
      ?dup:float ->
      ?reorder:float ->
      ?truncate:float ->
      ?garble:float ->
      ?delay:int ->
      int ->
      t
    (** [seeded seed] draws one fault per receive from [seed]: with the given
        probabilities a datagram is dropped, duplicated, reordered, truncated,
        or garbled, else (with a seed-driven chance when [delay > 0]) the
        receive stalls [1..delay] turns, else it passes through. *)

    val pp : t Fmt.t
    (** [pp] prints the schedule's kind and parameters. *)
  end

  type socket = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t

  val wrap : Schedule.t -> _ Eio.Net.datagram_socket -> socket
  (** [wrap schedule sock] is [sock] with [schedule]'s faults injected on
      {!Eio.Net.recv}. Sends pass through: the faults model the lossy, hostile
      network a receiver observes. Shutdown, close, and socket options pass
      through to [sock]. *)
end

(** An adversarial {!Eio.Net.t} for dialers and connection pools: connects
    return adversarial flows, connects can be refused, timed out, or slowed, and
    a keep-alive-style listener drops after a bounded number of accepts. *)
module Net : sig
  type connect =
    | Open  (** Connect normally; the flow is wrapped by the flow schedule. *)
    | Refuse  (** Raise a connection-refused error. *)
    | Timeout  (** Raise a connection-timeout error. *)
    | Slow of int  (** {!Eio.Fiber.yield} [n] times, then connect. *)

  (** A network's fault schedule: the per-flow schedule applied to every
      connected and accepted flow, an optional per-connect script, and an
      optional accept budget. *)
  module Schedule : sig
    type t

    val v :
      ?flow:Flow.Schedule.t ->
      ?connect:connect list ->
      ?max_accepts:int ->
      unit ->
      t
    (** [v ~flow ~connect ~max_accepts ()] applies [flow] (default
        {!Flow.Schedule.none}) to every connected and accepted flow, uses
        [connect.(i)] on the [i]th connect (passing through once exhausted),
        and, if [max_accepts] is set, closes the listener and raises
        [End_of_file] from every accept past that many. *)

    val seeded :
      ?dribble:int ->
      ?stall:int ->
      ?short_write:int ->
      ?reset_after:int ->
      ?reset_kind:[ `Reset | `Eof ] ->
      ?fail_write_after:int ->
      ?connect:connect list ->
      ?max_accepts:int ->
      int ->
      t
    (** [seeded seed] is {!v} with the per-flow schedule set to
        {!Flow.Schedule.seeded} of [seed] and the given knobs. *)

    val pp : t Fmt.t
    (** [pp] prints the per-flow schedule, connect-fault count, and accept
        budget. *)
  end

  type t = [ `Generic ] Eio.Net.ty Eio.Resource.t

  val wrap : Schedule.t -> _ Eio.Net.t -> t
  (** [wrap schedule net] is [net] with [schedule]'s faults injected: connects
      and accepts return {!Flow.wrap}ped flows, connect faults fire per
      [schedule], and the listener honours [max_accepts]. Datagram sockets are
      not carried (use {!Datagram.wrap}); [getaddrinfo] and [getnameinfo] pass
      through. *)

  val v : sw:Eio.Switch.t -> Schedule.t -> t
  (** [v ~sw schedule] is {!wrap} over a fresh in-memory network
      ({!Eio_net_mem.v}): a self-contained adversarial network for tests that
      need no real sockets. *)
end
