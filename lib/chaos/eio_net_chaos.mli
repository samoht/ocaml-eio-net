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
      last, and the first of each length class, a length rounded down to a power
      of two; it fails each connect, accept and datagram; and it runs each
      lasting condition. It grows with the structure of the exchange, not with
      its bytes;
    - the sampled tier spends the rest, at most half as much as the linear tier
      and at least 200 runs: the other write ends, and pairs of faults, the
      second at a position of the run the first one changed. That is where a
      retry, or a second connection after a reset, gets tested.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the workload or raise [~budget].

    {2 Contracts}

    {b At most two faults per run.} A program that tries three times always
    succeeds, so [run] may demand success. A broker that fans a message out to
    three subscribers may demand that one of them gets everything: two faults
    reach two subscribers at most. The second fault of a pair may be a mild one,
    a read or a write that moves one byte, or a connect or a datagram held back
    for 50 ms, and it counts as one of the two. A lasting condition runs alone
    and counts as one fault, though it reaches every connection of the run.

    {b Every program on the network meets the faults}, the test's own peers
    included. Write test servers with [Eio.Net.run_server ~on_error:ignore] and
    a timeout on each read, as a client may stall for good; catch
    [Eio.Io _ | End_of_file] in test clients; and parse strictly in both: a peer
    that takes a partial line for a whole one turns an injected end of stream
    into a wrong answer. Under [Eio.Fiber.both] or [Eio.Fiber.all], two fibers
    that each meet the fault, as both directions of one connection do, fail
    together with [Eio.Exn.Multiple]: catch the errors inside each fiber, or
    catch [Eio.Exn.Multiple] too.

    {b Ports are held as on Linux.} The end of a connection that closes first
    holds its port for 60 seconds of the run's clock, in TIME_WAIT. A server
    that closes its connections and listens again on the same port within a run
    fails with an address in use, as it would on Linux: listen with
    [~reuse_addr:true], as a real server does.

    {b Assert latency to catch a missing timeout.} A connect that times out
    takes 133 seconds, as on Linux, and then fails: a program without a connect
    timeout waits for it and still succeeds. A test that bounds its program's
    latency reads [env#clock] before and after.

    {b What a check does not catch.} A bug that needs a datagram delayed with
    others, or for longer than 50 ms, may pass: the linear tier delays one
    datagram at a time. A program that never fails the calls still waiting on a
    connection that died passes unless the workload waits for every call and
    checks how each ended.

    {2 Failures and replay}

    A check is deterministic. A failure raises [Failure] with the scenario, the
    tier that found it, the operations of the failing run, and the
    [~replay:"..."] argument that runs that scenario alone. A block of up to
    eight operations repeated three times or more shows its first and last
    instances, and a report longer than that keeps its first and last hundred
    lines and those around its first fault, so its size is bounded by the
    workload's structure rather than its bytes. The scenario is found by its
    position and fault, so a replay still runs after a fix that changes the
    workload's first run, such as the size of its reads: a failure then says the
    scenario was re-matched, and a re-matched replay that passes returns as any
    passing check does, saying nothing. A replay whose scenario the check no
    longer has fails.

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night reads its own:
    {[
    let check run =
      let budget = Option.map int_of_string (Sys.getenv_opt "CHAOS_BUDGET") in
      Eio_net_chaos.check ?budget run
    ]}

    {2 Runs}

    Each run has its own event loop, inside or outside another one. A check runs
    to its end before it returns, so it blocks the event loop it is called from:
    the other fibers of that loop wait for it. Its clocks start at [0.] and move
    only when every fiber waits, so a timeout of an hour takes no time. A run in
    which every fiber waits and no timer is due fails as a hang, and so does one
    still running after an hour of its clock, and one in which the same network
    operation fails 10000 times on the same target, a connect to one address or
    a read on one connection, while its clock stands still, as a client that
    retries a refused connect at once does in the outage. Failures on distinct
    targets, as a scan of many ports makes, do not count together. *)

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
      connection is reset at both ends, as on TCP, or the peer closes it, or
      stalls for good;
    - at each structural position of what one end writes, the connection is
      reset, or this end closes it, as a process killed in the middle of a
      message does;
    - each connect is refused, times out after 133 seconds, or finds no route to
      its host; each accepted connection is reset before its first byte, as when
      the client left first;
    - each datagram received is lost, duplicated, held back behind the next,
      replaced with noise from the same sender, or delivered 50 ms late, alone;
    - every read returns one byte until its end has read 4 KiB, or every write
      takes one byte until its end has written 4 KiB; every read waits 5 ms and
      takes what several writes sent; every connection queues 32 KiB each way,
      so a writer whose reader does not read blocks. With both socket buffers at
      their minimum, Linux holds as little as about 4 KiB each way over TCP and
      2 KiB over AF_UNIX, so 32 KiB blocks no writer it could not;
    - the outage: every connect is refused and every datagram lost, for the
      whole run. {!outage} is then [true], so [run] checks what its program does
      when it gives up.

    {!Fault.Emfile} is not among them: {!Eio.Net.run_server} stops on it, so a
    test that wants it asks for it with {!wrap}.

    A Unix-domain socket meets only what AF_UNIX can do, as it is reliable and
    ordered (unix(7)). A connect may be refused, but neither times out nor finds
    no route; an accepted connection may have been closed by its client; a peer
    may close, crash or stall, and its reader still gets every byte whose write
    returned; reads may take what several writes sent. A small write arrives
    whole, so there are no one-byte reads or writes, and a cut lands between two
    writes. No datagram is lost, duplicated, reordered or garbled, and the
    outage refuses connects only.

    A run passes if [run] returns. [run] catches the errors its program may
    raise and checks the results itself: any exception that escapes it fails the
    check, an injected [Eio.Io] included. So does a hang, a first run that makes
    no network operation, and two first runs that make different ones.

    [budget] (defaults to [2_000]) bounds the runs. [replay] runs only the
    scenario a failure named.

    @raise Failure with a report of the first failing run.
    @raise Invalid_argument if [budget] is less than [1]. *)

val outage : env -> bool
(** [outage env] is [true] iff [env] is that of the outage run of {!check}. *)

(** {1:blocks Building blocks} *)

type Eio.Exn.Backend.t +=
  | Injected_fault
        (** The backend code carried by the connection errors a wrapper raises,
            so an injected [Connection_reset] or [Connection_failure] is an
            {!Eio.Io} error of the shape a socket raises. *)

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
    family : [ `Inet | `Unix ];
        (** The address family of the socket: TCP or UDP over IP, or a
            Unix-domain socket, which loses, reorders and garbles nothing. *)
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
  (** The type for faults. One that does not apply to an operation does nothing,
      as one that AF_UNIX cannot produce on a Unix-domain socket: [Timeout] and
      [Unreachable] on a connect, and [Drop], [Dup], [Reorder] and [Garble] on a
      datagram. [Reset] on the accept of a Unix-domain connection is a client
      that closed, as [Eof]. *)
  type t =
    | Short of int
        (** A read or a write moves at most [max 1 n] bytes. A write cut short
            found the socket's buffer full, so the next write on that end waits,
            as on eio_posix, and other fibers run meanwhile. *)
    | Stall of float
        (** The operation waits that many seconds of the clock, then runs. *)
    | Hang
        (** The operation waits until a cancel ends it, as a read from a silent
            peer does; a connect never does, as the kernel gives up on it, so
            {!check} hangs reads only. It holds that one operation: a read after
            the cancel gets the bytes the hung one would have, a datagram taken
            by a receive that hangs goes to the next receive, and a connection
            taken by an accept that hangs to the next accept. *)
    | Reset
        (** The connection is reset at both ends, and the bytes not yet read are
            lost, except on a Unix-domain connection: AF_UNIX keeps in the
            reader's queue what a returned write sent, so each end reads that
            first. At each end the first read or write that follows raises
            [Connection_reset]; later reads return end of file and writes keep
            raising it, as on Linux and macOS. On an accept, the accept succeeds
            and the connection it returns is reset, as Linux hands over one
            whose client left first. On a connect, it is a refusal, as a reset
            answering the handshake is. *)
    | Eof
        (** One end closes, as a FIN does: no byte already written is lost. On a
            read, the peer closed: the read returns what the peer wrote before,
            then end of file. On a write, this end closed: the write raises
            [Connection_reset], and the peer reads the bytes written before,
            then end of file. On an accept, the client closed. After it, the end
            that closed reads end of file and its writes raise
            [Connection_reset]; the other end's first write goes through, its
            bytes lost, and the next ones raise [Connection_reset], as on Linux
            after the peer closes. On a Unix-domain connection the end that
            closed reads what is queued to it first. *)
    | Refuse  (** A connect fails with [Connection_failure (Refused _)]. *)
    | Timeout
        (** A connect fails after 133 seconds of the clock, as a lost handshake
            does on Linux, with the error eio_posix raises then:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ETIMEDOUT,
             "connect-in-progress", "")), _)]. Eio does not classify it as a
            [Connection_failure]. *)
    | Unreachable
        (** A connect fails at once, as one with no route to its host does, with
            the error eio_posix raises then:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENETUNREACH, "connect",
             "")), _)]. *)
    | Emfile
        (** An accept fails with the error eio_posix raises when the process has
            no descriptor left:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (EMFILE, "accept", "")), _)],
            which [Eio_net_unix.accept_failure] reads as [`Descriptors]. As on
            Linux, the connection stays queued and its client sees nothing, and
            every accept on the network fails the same way until the program
            frees a descriptor, by closing a connection, socket or listener of
            the network. The next accept then returns that connection, with its
            data. {!check} does not try it, as {!Eio.Net.run_server} stops on
            it. *)
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
    use the network. If [f] raises, the operation raises that exception and does
    nothing else: an accept leaves its connection queued for the next, and a
    receive its datagram. The operation's index and its socket number stay used.

    A fault reaches both ends of a connection when [net] is an {!Eio_net_mem}
    network, which tells the wrapper which accepted socket a connect made. On
    another network the wrapper sees the two ends of a connection as two
    sockets, numbered apart, and a fault shuts down the socket of the end that
    meets it: a reset on a read gives the real peer end of file, and a reset on
    an accept gives the real client [ECONNRESET] once it sends. An end of stream
    on a read shuts this end's receive side, and the real peer sees nothing: on
    Linux this end reads what is queued, then end of file, and on macOS it reads
    end of file at once, as macOS drops what is queued.

    After the same operation fails 10000 times on the same target while [clock]
    stands still, with no success there in between, the next attempt raises
    [Failure], as the program that makes them spins for ever, and after 20000 it
    waits for ever.

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
