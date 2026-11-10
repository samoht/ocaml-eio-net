(** Network faults, for tests of Eio programs.

    {!check} runs a workload on an in-memory network that fails in a different
    way on each run: a read returns one byte or several messages at once, a
    connection is reset or ends midway, a peer stalls, a send buffer fills, a
    connect is refused or times out, a datagram is lost, duplicated, reordered
    or replaced with noise. It chooses the scenarios from what the workload
    does.

    {2 Exploration}

    A check runs the workload once with no fault and records its operations. Its
    scenarios are positions in that run, in two tiers:

    - the linear tier always runs whole. On each end of each connection it cuts
      the bytes around the ends of the first three of the peer's writes, its
      last, and each whose length differs from the one before; it fails each
      connect, accept and datagram; and it runs each lasting condition. It grows
      with the structure of the exchange, not with its bytes;
    - the sampled tier spends the rest, at most half as much as the linear tier
      and at least 200 runs: the other write ends, and pairs of faults, the
      second at a position of the run the first one changed. That is where a
      retry, or a second connection after a reset, gets tested.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the workload or raise [~budget].

    {2 Contracts}

    {b At most two faults per run}, a 50 ms stall included. A program that tries
    three times always succeeds, so [run] may demand success. A broker that fans
    a message out to three subscribers may demand that one of them gets
    everything: two faults reach two subscribers at most.

    {b Every program on the network meets the faults}, the test's own peers
    included. Write test servers with [Eio.Net.run_server ~on_error:ignore],
    catch [Eio.Io _ | End_of_file] in test clients, and parse strictly in both:
    a peer that takes a partial line for a whole one turns an injected end of
    stream into a wrong answer.

    {b Assert latency to catch a missing timeout.} A connect that times out
    takes 133 seconds, as on Linux, and then fails: a program without a connect
    timeout waits for it and still succeeds. A test that bounds its program's
    latency reads [env#clock] before and after.

    {2 Failures and replay}

    A check is deterministic. A failure raises [Failure] with the scenario, the
    tier that found it, the operations of the failing run, and the
    [~replay:"..."] argument that runs that scenario alone. A replay fails if
    the workload's first run is no longer the one it names, or if the check no
    longer has the scenario.

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night reads its own:
    {[
    let check run =
      let budget = Option.map int_of_string (Sys.getenv_opt "CHAOS_BUDGET") in
      Eio_net_chaos.check ?budget run
    ]}

    {2 Runs}

    Each run has its own event loop, inside or outside another one. Its clocks
    start at [0.] and move only when every fiber waits, so a timeout of an hour
    takes no time. A run in which every fiber waits and no timer is due fails as
    a hang, and so does one still running after an hour of its clock, and one in
    which 10000 network operations fail in a row while its clock stands still,
    as a client that retries a refused connect at once does in the outage. *)

type env =
  < net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t >
(** The type for what a workload runs with: a fresh in-memory network and the
    run's clocks. [Eio_fs.Chaos.env] has the same shape, with [fs] in place of
    [net]. *)

(** {1:checks Checks} *)

val check : ?budget:int -> ?replay:string -> (env -> unit) -> unit
(** [check ~budget ~replay run] calls [run] many times, each with a fresh
    in-memory network of {!Eio_net_mem}. The first run has no fault.

    The linear tier tries each of these alone:

    - at each structural position of what one end of a connection reads, the
      connection is reset, or ends, at both ends as on TCP; or the peer stalls
      for good;
    - at each structural position of what one end writes, the connection is
      reset;
    - each connect is refused, times out after 133 seconds, or finds its host
      unreachable after 3 seconds; each accepted connection is reset before its
      first byte, as when the client left first;
    - each datagram received is lost, duplicated, held back behind the next, or
      replaced with noise from the same sender;
    - on the first 4 KiB of each end, every read returns one byte, or every
      write takes one byte; every read waits 5 ms and takes what several writes
      sent; every connection queues 4 KiB each way, the least a Linux socket
      buffer holds, so a writer whose reader does not read blocks;
    - the outage: every connect is refused and every datagram lost, for the
      whole run. {!outage} is then [true], so [run] checks what its program does
      when it gives up.

    {!Fault.Emfile} is not among them: {!Eio.Net.run_server} stops on it, so a
    test that wants it asks for it with {!wrap}.

    A run passes if [run] returns. [run] catches the errors its program may
    raise and checks the results itself: any exception that escapes it fails the
    check, an injected [Eio.Io] included. So does a hang, a first run that makes
    no network operation, and two first runs that make different ones.

    [budget] (defaults to [2_000]) bounds the runs. [replay] runs only the
    scenario a failure named.

    @raise Failure with a report of the first failing run. *)

val outage : env -> bool
(** [outage env] is [true] iff [env] is that of the outage run of {!check}. *)

(** {1:blocks Building blocks} *)

type Eio.Exn.Backend.t +=
  | Injected_fault
        (** The backend code carried by the connection errors a wrapper raises,
            so an injected [Connection_reset] or [Connection_failure] is an
            {!Eio.Io} error of the shape a socket raises. *)
  | Emfile_fault
        (** The backend code of the error an accept raises under
            {!Fault.Emfile}, printed as [EMFILE]: the process has no descriptor
            left. A classifier that matches [Eio_unix.Unix_error] does not see
            it. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Connect
    | `Accept
    | `Read  (** A read from a connection. *)
    | `Write  (** A write to a connection. *)
    | `Recv  (** A datagram taken by a receive. *) ]
  (** The type for operation kinds. *)

  type t = {
    index : int;  (** The operation's place among all of them, from [1]. *)
    kind : kind;
    socket : int;
        (** The connection or datagram socket, numbered from [1] in the order
            the network made them, a refused connect included. *)
    side : [ `Client | `Server ];
        (** The end of a connection: the one that connected, or the one that
            accepted. A datagram socket is a [`Client]. *)
    offset : int;
        (** The bytes this end read or wrote before, the datagrams this socket
            took before, or the connects or accepts before. *)
    length : int;
        (** The bytes a read asks for or a write offers, or the size of the
            buffer a datagram is received into. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and reports. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  (** The type for faults. One that does not apply to an operation does nothing.
  *)
  type t =
    | Short of int  (** A read or a write moves at most [max 1 n] bytes. *)
    | Stall of float
        (** The operation waits that many seconds of the clock, then runs. *)
    | Hang
        (** The operation never completes; only a cancel ends it. A read from a
            silent peer does that; a connect never does, as the kernel gives up
            on it, so {!check} hangs reads only. A datagram taken by a receive
            that hangs goes to the next receive. *)
    | Reset
        (** The connection is reset at both ends: reads and writes raise
            [Connection_reset] from now on, and the bytes not yet read are lost.
            On an accept, the accept succeeds and the connection it returns is
            reset, as Linux hands over one whose client left first. On a
            connect, it is a refusal, as a reset answering the handshake is. *)
    | Eof
        (** The connection ends at both ends: reads raise [End_of_file] from now
            on, writes raise [Connection_reset], and the bytes not yet read are
            lost. On an accept, the connection it returns has ended. *)
    | Refuse  (** A connect fails with [Connection_failure (Refused _)]. *)
    | Timeout
        (** A connect fails after 133 seconds of the clock, as a lost handshake
            does on Linux, with the error eio_posix raises then:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ETIMEDOUT,
             "connect-in-progress", "")), _)]. Eio does not classify it as a
            [Connection_failure]. *)
    | Unreachable
        (** A connect fails after 3 seconds of the clock, as one to a host that
            does not answer its address lookup does on Linux, with
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (EHOSTUNREACH,
             "connect-in-progress", "")), _)]. *)
    | Emfile
        (** An accept fails with {!Emfile_fault}: the process has no descriptor
            left. The connection it took is reset, as if its client had left.
            {!check} does not try it, as {!Eio.Net.run_server} stops on it. *)
    | Drop  (** The datagram is lost; the receive waits for the next one. *)
    | Dup  (** The datagram arrives now and again on the next receive. *)
    | Reorder  (** The datagram arrives after the next one. *)
    | Garble of int
        (** [n] bytes of noise arrive in place of the datagram, from the same
            sender. *)

  val pp : t Fmt.t
  (** [pp] formats a fault for logs and reports. *)
end

val wrap :
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [wrap ~clock f net] is [net] with [f] asked about each operation before it
    runs, on the connections and the datagram sockets it makes. The operation
    meets the fault [f] answers; [clock] times stalls and timeouts. [f] may also
    act: count or log. [getaddrinfo], [getnameinfo] and datagram sends pass
    through.

    The network, its connections and its datagram sockets may be used from
    several domains at once. [f] is called by one of them at a time, in the
    order of {!Op.index}, while a lock is held: it must not wait, and must not
    use the network.

    A reset or an end breaks both ends of a connection when [net] is an
    {!Eio_net_mem} network, which tells the wrapper which accepted socket a
    connect made. On another network, the two ends are two sockets to the
    wrapper, numbered apart, and a fault breaks only the end that meets it.

    After 10000 operations that fail in a row while [clock] stands still, the
    next raises [Failure], as the program that makes them spins for ever, and
    after 20000 an operation waits for ever.

    A read or a write after the flow's own close raises [Invalid_argument]
    before [f] is asked, as Eio's own sockets do. A close drops the bytes a read
    has not handed up yet, and the datagrams a socket holds back. *)

val pair :
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~clock f] is a connected pair of in-memory streams, the client end
    first, with [f] asked about each read and write of either end, as {!wrap}
    asks: [socket] is [1]. A fuzzer drives it from its input. *)
