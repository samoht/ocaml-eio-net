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
  (** The type for the fault injected on one read. *)
  type read =
    | Deliver of int
        (** Hand up at most [n] buffered bytes on this read ([n <= 0] means all
            buffered): the dribble / short-read fault. *)
    | Coalesce of int
        (** Do [n] base reads, accumulate them, then hand the whole buffer up:
            several transport reads become one application read. Stops early at
            end-of-file, which this read raises only when nothing is buffered.
        *)
    | Stall of int
        (** {!Eio.Fiber.yield} [n] times, then deliver all buffered bytes. A
            cooperative stall: the scheduler runs other fibers in between, so a
            reader that stalls with no other runnable work trips
            [Eio_mock.Backend]'s deadlock detector. *)
    | Reset  (** Raise a connection-reset instead of delivering. *)
    | Eof
        (** End the read direction: this read and every later one raise
            [End_of_file]. *)

  (** The type for the fault injected on one write. *)
  type write =
    | Accept of int
        (** Accept at most [n] of the offered bytes on this write ([n <= 0]
            means all): the short-write fault. The accepted bytes reach the
            peer; the caller sees the short count and must retry the rest. *)
    | Fail  (** Raise a connection-reset instead of writing. *)

  (** A flow's fault schedule: pass-through, an exact per-call script, or a
      seeded generator. {!Schedule.pp} prints the call that builds it, so a test
      can report what it pinned or fuzzed and a reader rebuild it. Each flow a
      schedule wraps runs it from its start. *)
  module Schedule : sig
    type t
    (** The type for flow fault schedules. *)

    val none : t
    (** [none] injects no faults: reads and writes pass straight through. *)

    val script : ?reads:read list -> ?writes:write list -> unit -> t
    (** [script ~reads ~writes ()] injects the [i]th element of [reads]
        (defaults to [[]]) on the [i]th read and the [i]th element of [writes]
        (defaults to [[]]) on the [i]th write of at least one byte, and passes
        through once a list is exhausted. *)

    val seeded :
      ?dribble:int ->
      ?stall:int ->
      ?short_write:int ->
      ?reset_after:int ->
      ?reset_kind:[ `Reset | `Eof ] ->
      ?fail_write_after:int ->
      int ->
      t
    (** [seeded ~dribble ~stall ~short_write ~reset_after ~reset_kind
         ~fail_write_after seed] derives faults from [seed]:
        - Each read is, with even odds when [stall] (defaults to [0]) is
          positive, a {!Stall} of [1] to [stall] scheduler turns, and otherwise
          a {!Deliver} of [1] to [dribble] bytes (defaults to [0], uncapped).
        - Each write is an {!Accept} of [1] to [short_write] bytes (defaults to
          [0], uncapped).
        - The first read once [reset_after] bytes (defaults to never) have been
          delivered is a [reset_kind] (defaults to [`Reset]) fault: {!Reset} for
          [`Reset] and {!Eof} for [`Eof]. No read before it delivers a byte past
          [reset_after], so the fault lands at that byte exactly.
        - The first write once [fail_write_after] bytes (defaults to never) have
          been written is a {!Fail}.

        Read faults and write faults each depend only on [seed], not on how
        reads and writes interleave. The bytes a read delivers also depend on
        those the base holds when it runs, so they repeat only when the bytes
        arrive the same way, as on a single-domain scheduler over
        {!Eio_net_mem}. *)

    val pp : t Fmt.t
    (** [pp] formats a schedule as the call that builds it, every parameter
        given: {!val-none}, [script ~reads:[...] ~writes:[...] ()] with each
        fault, or
        [seeded ~dribble ~stall ~short_write ~reset_after ~reset_kind
         ~fail_write_after seed] with [reset_after] and [fail_write_after] left
        out when they are never. The text builds the schedule again where
        [Eio_net_chaos.Flow] and {!Flow.Schedule} are open. *)
  end

  type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  (** The type for wrapped stream sockets, usable anywhere a {!Eio.Flow.two_way}
      or {!Eio.Net.stream_socket} is expected. *)

  val wrap : Schedule.t -> _ Eio.Net.stream_socket -> flow
  (** [wrap schedule sock] is [sock] with [schedule]'s faults injected on its
      reads and writes. Shutdown, close, and socket options pass through to
      [sock]. *)

  val pair : ?client:Schedule.t -> ?server:Schedule.t -> unit -> flow * flow
  (** [pair ~client ~server ()] is a connected pair of in-memory flows, the ends
      of {!Eio_net_mem.pipe}, with the client end wrapped by [client] (defaults
      to {!Schedule.none}) and the server end by [server] (defaults to
      {!Schedule.none}). Bytes written on one arrive on the other. Both ends
      must run under an Eio scheduler ([Eio_mock.Backend.run] suffices), and no
      switch closes them. *)
end

(** Adversarial datagram sockets: loss, duplication, reordering, truncation,
    garbage injection, and a stalled receive, all on the incoming path. *)
module Datagram : sig
  (** The type for the fault injected on one receive. *)
  type fault =
    | Pass  (** Deliver the next datagram unchanged. *)
    | Drop of int
        (** Drop [n] datagrams from the base, then deliver the one after them.
            {!Schedule.script} refuses a negative [n]. *)
    | Dup  (** Deliver the next datagram now and again on the following recv. *)
    | Reorder
        (** Hold the next datagram back and deliver the one after it first; the
            held datagram surfaces on the following recv. A receive cancelled
            while it waits for the one after keeps the held datagram. *)
    | Truncate of int
        (** Deliver only the first [n] bytes of the next datagram, none if [n]
            is negative. *)
    | Garble of int
        (** Deliver [n] bytes drawn from the schedule's seed, [0] for a script,
            in place of the next datagram, with its source address. *)
    | Delay of int
        (** {!Eio.Fiber.yield} [n] times, then deliver the next datagram. *)

  (** A datagram socket's recv-path fault schedule. *)
  module Schedule : sig
    type t
    (** The type for datagram fault schedules. *)

    val none : t
    (** [none] injects no faults. *)

    val script : fault list -> t
    (** [script events] injects the [i]th element of [events] on the [i]th
        receive that takes a datagram from the base, and passes through once
        [events] is exhausted. A datagram held back by {!Dup} or {!Reorder} is
        delivered on the receive after, with no fault.

        Raises [Invalid_argument] if [events] has a {!Drop} of a negative count.
    *)

    val seeded :
      ?loss:float ->
      ?dup:float ->
      ?reorder:float ->
      ?truncate:float ->
      ?garble:float ->
      ?delay:int ->
      int ->
      t
    (** [seeded ~loss ~dup ~reorder ~truncate ~garble ~delay seed] draws one
        fault per receive from [seed]: with probability [loss], [dup],
        [reorder], [truncate] and [garble] (each defaults to [0.]) a datagram is
        dropped, duplicated, reordered, truncated to fewer than [8] bytes, or
        replaced with fewer than [16] garbled bytes; otherwise, with even odds
        when [delay] (defaults to [0]) is positive, the receive stalls [1] to
        [delay] scheduler turns, and else it passes through.

        Raises [Invalid_argument] if one of [loss], [dup], [reorder], [truncate]
        and [garble] is outside [[0., 1.]], NaN included, or if they sum to more
        than [1.]. *)

    val pp : t Fmt.t
    (** [pp] formats a schedule as the call that builds it, every parameter
        given: {!val-none}, [script [...]] with each fault, or
        [seeded ~loss ~dup ~reorder ~truncate ~garble ~delay seed], each
        probability as a float literal with the fewest digits that read back as
        it. The text builds the schedule again where [Eio_net_chaos.Datagram]
        and {!Datagram.Schedule} are open. *)
  end

  type socket = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t
  (** The type for wrapped datagram sockets. *)

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
  (** The type for the fault injected on one connect. *)
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
    (** The type for network fault schedules. *)

    val v :
      ?flow:Flow.Schedule.t ->
      ?connect:connect list ->
      ?max_accepts:int ->
      unit ->
      t
    (** [v ~flow ~connect ~max_accepts ()] applies [flow] (defaults to
        {!Flow.Schedule.none}) to every connected and accepted flow, injects the
        [i]th element of [connect] (defaults to [[]]) on the [i]th connect of
        the network and passes through once it is exhausted, and closes a
        listener and raises [End_of_file] from each of its accepts past
        [max_accepts] (defaults to no bound). An accept counts once it returns a
        connection, so a cancelled one uses none of the budget. *)

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
    (** [seeded ~dribble ~stall ~short_write ~reset_after ~reset_kind
         ~fail_write_after ~connect ~max_accepts seed] is
        [v ~flow ~connect ~max_accepts ()] with [flow] the
        {!Flow.Schedule.seeded} schedule of [seed] and the flow arguments. *)

    val pp : t Fmt.t
    (** [pp] formats a schedule as the call that builds it:
        [v ~flow:(...) ~connect:[...] ~max_accepts ()], with the flow schedule
        as {!Flow.Schedule.pp} prints it but qualified, as in
        [Flow.Schedule.seeded ...], and [max_accepts] left out when there is no
        bound. The text builds the schedule again where [Eio_net_chaos],
        [Eio_net_chaos.Net] and {!Net.Schedule} are open. *)
  end

  type t = [ `Generic ] Eio.Net.ty Eio.Resource.t
  (** The type for wrapped networks. *)

  val wrap : Schedule.t -> _ Eio.Net.t -> t
  (** [wrap schedule net] is [net] with [schedule]'s faults injected: connects
      and accepts return {!Flow.wrap}ped flows, connect faults fire per
      [schedule], and the listener honours [max_accepts]. Datagram sockets are
      not carried: [datagram_socket] raises [Invalid_argument], and
      {!Datagram.wrap} wraps one from [net]. [getaddrinfo] and [getnameinfo]
      pass through. *)

  val v : sw:Eio.Switch.t -> Schedule.t -> t
  (** [v ~sw schedule] is {!wrap} over a fresh in-memory network
      ({!Eio_net_mem.v}): a self-contained adversarial network for tests that
      need no real sockets. *)
end
