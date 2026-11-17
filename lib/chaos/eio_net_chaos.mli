(** Network faults, for tests of Eio programs and of Eio networks.

    [Eio_net_chaos] has two checks, one function each:

    - {!run} checks that an Eio program copes with a network that fails. It runs
      the program many times on in-memory networks that fail, and after each
      fault starts it again on the same network, healed, as a machine restarts a
      service.
    - {!val-conformance} checks that an Eio network answers as Linux, macOS or
      Windows does.

    Both raise [Failure] with a report when the check fails, as an assertion
    does, so they are the body of a test in any test runner. {!run} is
    {!Eio_chaos.run} on one device, {!val-network}: a program that also needs a
    disk is tested with [Eio_chaos.run [ Eio_fs_chaos.disk (); network () ]]. A
    test that wants one fault at one place builds it from the
    {{!blocks}building blocks}: {!wrap} for a network, {!pair} for two connected
    streams.

    {!run}, {!val-conformance} and {!wrap} take [?os], as {!Eio_net_mem.v} does:
    [`Linux], the default, [`Macos] or [`Windows]. It picks the answers expected
    and the kernel's timings the faults follow. The choice comes from the
    argument, never from the host, so a test checks the same thing on every
    machine.

    {!val-conformance} replays two kinds of recording: {e sequences}, each an
    order of a few socket operations, and {e scenarios}, longer programs written
    by hand, such as the reuse of a port or a name lookup. A report of {!run}
    names its runs as scenarios too: a fault at a place of the first run.

    How a check picks its {{!exploration}faults}, the {{!contracts}contracts} a
    program meets, its {{!replay}reports and replay} and its {{!runs}runs} come
    after. *)

type env = Eio_chaos.env
(** The type for what a program or a network under test runs with: the
    capabilities of [Eio.Stdenv], each simulated, so code written against the
    accessors of [Eio.Stdenv] takes it unchanged. {!Eio_chaos.type-env} says
    what each is.

    - [net] is an in-memory network of {!Eio_net_mem}, the one that fails, and
      [fs] and [cwd] refuse each operation, unless another device gives them, as
      the disk of eio-fs does;
    - [clock] and [mono_clock] start at [0.], and a wait takes no time
      ({{!runs}runs});
    - [secure_random] and the default [Random] state draw from a fixed seed,
      whatever [~seed]: the first run and each run with a fault draw the same,
      and a restart draws others, as a new process does;
    - [stdin] is empty, and [stdout], [stderr] and [Eio.traceln] are kept in
      memory: a report shows the last 2 KiB the program printed;
    - [domain_mgr] runs its work in the calling fiber, on the run's own
      scheduler;
    - [process_mgr] starts nothing: [spawn] and [pipe] raise [Eio.Io] with
      [Not_available];
    - [debug] is Eio's, [backend_id] is ["chaos"], and [chaos] is the run's
      {!Eio_chaos.t}, for a device the program builds over others.

    A program that reaches the system other than through [env], by [Unix], the
    standard library's channels, [Random.self_init], [Domain.spawn] or
    [Sys.command], escapes the check. Built with
    {{:https://github.com/samoht/ocaml-hermetic}hermetic} ([open! Hermetic]),
    such a call does not compile, so what differs between two runs can only be
    state the program kept in memory from an earlier one. Without it, a direct
    call escapes, and when it makes two runs with no fault differ in the kinds
    of operation they make, the check fails with a report that names the first
    difference and its likely causes. *)

(** {1:program Testing a program} *)

val run :
  ?os:[< Eio_chaos.os ] ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  ?setup:(env -> unit) ->
  ?inspect:(env -> unit) ->
  (env -> unit) ->
  unit
(** [run ~os ~budget ~seed ~replay ~setup ~inspect app] runs [app] once with no
    fault on an in-memory network that answers as [os] (defaults to [`Linux])
    does and holds what [setup] (defaults to doing nothing) left there. It then
    runs [app] again many times, each time on a new network prepared the same
    way, with one fault:

    - a connection is reset at both ends, as on TCP, or one end closes it in the
      middle of a message, or it goes silent for good, as when the peer's host
      loses power;
    - a connect is refused, times out after 133 seconds, or finds no route to
      its host; an accepted connection is reset before its first byte, as when
      the client left first;
    - a datagram is lost, duplicated, held back behind the next, replaced with
      noise from the same sender, or delivered 50 ms late;
    - for the whole run, every read returns one byte, or every write takes one
      byte, until its end has moved 4 KiB; or every read waits 5 ms and takes
      what several writes sent; or every connection queues 32 KiB each way, or
      the kernel's default for its family if that is smaller, so a writer whose
      reader does not read blocks;
    - the outage, for the whole run: every connect is refused and every datagram
      lost. {!is_outage} is then [true].

    [app] is everything on the network: a client under test comes with the
    server it talks to, and a server with a client. Every one of them meets the
    faults. A Unix-domain socket meets only what AF_UNIX can do, as it is
    reliable and ordered (unix(7)): a connect may be refused, but neither times
    out nor finds no route; a peer may close, crash or hang, and its reader
    still gets every byte whose write returned; reads may take what several
    writes sent, and a write of up to 32 KiB arrives whole. No datagram is lost,
    duplicated, reordered or garbled, and the outage refuses connects only.

    After each of those runs, [run] runs [app] again with no fault, on the same
    network, healed, as a machine restarts a service. The network keeps what the
    run left: a port still held after a close, in TIME_WAIT.

    {b How the faults are picked.} [run] learns the operations of [app] from its
    first run. It tries every fault above at every operation of that run, but
    for the many connections alike, of which it takes the first three, the last
    and one of each shape, and the many reads, writes and datagrams of one end,
    of which it takes the first three, the last and one of each size. A cut in
    the middle of a stream falls where the peer's writes end. Then it spends a
    fortieth of [budget] (defaults to [20_000]), or half as many runs as it has
    run if that is more, on runs drawn at random from [seed] (defaults to [0]):
    the other connections, reads, writes and datagrams, and two faults in a run,
    the second anywhere in the run the first one changed, such as the second
    connection after a reset. Another [seed] draws other runs.
    {!section-exploration} says more.

    {!Fault.No_descriptor} is not among the faults: {!Eio.Net.run_server} stops
    on it, so a test that wants it asks for it with {!wrap}.

    {b When it passes.} [run] passes iff:

    - the first run returns, and a second run with no fault makes the same kinds
      of operation in the same order. Where only their connections differ, the
      check goes on, and a report says where;
    - no run hangs or raises an exception other than [Eio.Io], [End_of_file] and
      [Eio.Time.Timeout], alone or together in an [Eio.Exn.Multiple], an
      assertion of [app] included. A program with no timeout hangs on a silent
      peer;
    - after each fault, [inspect] (defaults to doing nothing), called on the
      healed network, returns;
    - each restart returns: the program can listen and connect again.

    An [Eio.Io] error, an [End_of_file] or an [Eio.Time.Timeout] that escapes a
    run with a fault neither passes nor fails it: the network did fail, so what
    decides is whether the program works again once the network does, which the
    restart checks. A program that must succeed despite a fault says so with its
    own assertion, and {!is_outage} tells it when it may give up.

    Under [`Macos] a connect that times out takes 75 seconds, TIME_WAIT lasts
    30, and a Unix-domain crash reads as end of file at both ends, as macOS
    reports no reset on AF_UNIX. Under [`Windows] it takes 21 seconds, as
    Windows documents its two SYN retransmissions, which no trace records, and
    there is no TIME_WAIT. The networks live in memory and their clocks jump to
    the next timer, so a check of a small program takes milliseconds.

    @raise Failure
      with a report of the first failing run: the fault, the operations of the
      run, the error, what the program printed, and the line to paste that runs
      that case again, as
      [Eio_net_chaos.run ~replay:"net/85d85f49/connect:1.timeout" app]. [replay]
      reruns that one case, and its restart, whatever the [seed] that found it.
      To find it, a replay may first run [app] a few times, so the failing run
      is the last one.
    @raise Invalid_argument
      if [budget] is not positive, or [replay] is not a replay string of [run].
*)

val network : unit -> Eio_chaos.device
(** [network ()] is the network of {!run} as a device of {!Eio_chaos.run}, to
    test a program on it and other simulated devices at once: an in-memory host
    with a loopback interface that answers as the [~os] of the check does, with
    the faults above. A restart keeps the ports held in TIME_WAIT; a power cut,
    which another device may bring, keeps nothing. [run app] is
    [Eio_chaos.run [ network () ] app], but for the name its reports give. *)

val is_outage : env -> bool
(** [is_outage env] is [true] iff [env] is that of the outage run of {!run}. A
    program that must succeed under any single fault may give up only then. It
    is [false] in every other run, in a restart, in [inspect], and for an [env]
    no run of {!run} made. *)

(** {1:conformance Testing a network} *)

val conformance :
  ?os:[< Eio_chaos.os ] ->
  ?under:Eio_chaos.device list ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  (sw:Eio.Switch.t -> env -> _ Eio.Net.t) ->
  unit
(** [conformance ~os net] runs each sequence of socket operations recorded on
    the kernel of [os] (defaults to [`Linux]) on a fresh network [net ~sw env],
    and compares each answer with the kernel's. [sw] ends with the sequence, so
    what [net] holds under it is released there.

    Each sequence runs in an event loop of its own, as a run of {!run} does,
    with a {!type-env} of its own whose devices are simulated: a network built
    on capabilities, such as a userspace stack that needs a clock or entropy for
    its ports and sequence numbers, takes them from it, and its answers are the
    same on every run. Its [net] is an in-memory network that follows [os], for
    a network that carries its traffic over another one, as a tunnel does. The
    host's own network does not pass through [conformance]: only the host's
    event loop drives it.

    {[
    let check_mem () =
      Eio_net_chaos.conformance (fun ~sw env ->
          Eio_net_mem.v ~clock:env#mono_clock ~sw ())
    ]}

    A network is a whole {!Eio.Net.t}: the two ends of each connection are made
    by its own [listen] and [connect], so a sequence sees what a close at one
    end does at the other. The sequences cover, over TCP, UDP and Unix-domain
    sockets, on IPv4 and IPv6: every order of up to four of listen, connect,
    accept, read, write, shutdown and close, and of up to three datagram
    operations; then binds, backlogs and the reuse of an address after a close,
    Unix paths, name resolution and socket options.

    An answer is a result, "would wait" for an operation that does not complete
    at once, or an error. Each step has the set of answers [os] allows: the one
    the kernel gave and, where the answer depends on the host's configuration
    (its addresses, name server, services database or host name) or on how the
    kernel counts a buffer's memory, the answers other hosts give. An answer
    passes iff it is in the set. Errors compare by the class Eio gives them
    ([Connection_reset], [Connection_failure], [Not_found]), else by their
    errno, and an error of a backend that carries no errno is any error.
    {!Eio_net_mem.v} passes for the [os] it is given.

    [conformance] runs every sequence, whatever the earlier ones answered. Of a
    sequence it reports the first answer outside its set only, as the later ones
    depend on it. It leaves out the sequences only a prepared host can answer:
    those that wait 60 seconds for a port to be free again, those that use up
    every port, and those that send off the loopback interface; and, under
    [`Macos], those whose answer depends on timing, of which the traces hold one
    answer: 21 scenarios, and the TCP sequences that read or write after an end
    shut its receive side while bytes or an end of stream were on their way to
    it, or after bytes went to an end that had shut it or closed, as whether the
    reset macOS then sends has arrived depends on when the operation runs; and,
    under [`Windows], the scenarios eio-net.mem does not model (socket options'
    values, the addresses a connect or a datagram reaches across families, the
    edge addresses and the names getnameinfo gives). Under [`Windows] an answer
    where eio_windows breaks Eio's contract (a refused connect that never
    returns, a read that waits after a receive shutdown, an unclassified
    [WSAESHUTDOWN], a datagram receive that raises [EMSGSIZE], a write of no
    bytes on a closed flow, a raw [Unix.Unix_error], lookups, abstract Unix
    paths and [reuse_addr] on a Unix path) is allowed the answer the contract
    gives, and the report names each such defect it allowed. Unix paths are made
    in the current directory.

    An operation is "would wait" if it has not completed after 64 turns of the
    event loop, as the recorder took it, and a sequence that has not ended after
    10,000 turns answers "timeout". Each turn takes 1 us of a simulated clock,
    so eio-net.mem passes in about three seconds.

    {b Over other devices.} A network built over other devices, as a tunnel over
    another network, takes them from its {!type-env}: [under] (defaults to none)
    are the devices it holds, with a quiet {!val-network} as its [net] unless
    one of them is a network. Its sequences and scenarios first run as above,
    each on healthy devices. Then, if [under] is not empty, [conformance] is
    {!Eio_chaos.run} of a program that runs 32 of the scenarios, drawn from
    [seed] (defaults to [0]), in one event loop, as the sequences and scenarios
    are too many to replay at each of its runs, with [budget] and [replay] as
    {!run} takes them, and the faults the devices of [under] allow. It passes
    iff each run does: until a device fails, every answer is the kernel's; once
    one failed ({!Eio_chaos.faulted}), an answer may differ, as the faults
    eio-net injects also lose, duplicate, reorder and garble datagrams and
    silence peers, but it names no error a network does not give, such as
    [Not_found] or [Invalid_argument], and the run does not hang. The restart
    replays nothing, as a network keeps its ports across it and the scenarios
    name fixed ports. This checks less than with no fault: a network that loses
    what it was given passes there.

    @raise Failure
      if an answer is outside its set, with a report ({!Eio_chaos.fail}): a
      table with a row for each step and answers that differ, the step, the
      first sequence that gave them, its operations up to the one that differs,
      and how many more did, the answer allowed, or the set of them, and the one
      given, as [connect],
      [tcp4 accept; close S; close S; connect (and 21,360 more)], [ok] and
      [Refused]; then how many passed, differed and were left out, and why. Past
      the first hundred rows it gives the count alone. A step names its end of
      the connection: [C] the one that connected, [S] the one that accepted. *)

(** {1:blocks Building blocks} *)

type Eio.Exn.Backend.t +=
  | Injected_fault
        (** The backend code carried by the connection errors a wrapper raises,
            so an injected [Connection_reset] or [Connection_failure] is an
            {!Eio.Io} error of the shape a socket raises. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Connect  (** A connect, a refused one included. *)
    | `Accept  (** An accept of a connection. *)
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
            datagram received. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and reports, as [#4 write 1s @0+5]: its
      {!index}, its kind, its {!socket} and side, [c] for the end that connected
      and [s] for the one that accepted, then [@] its {!offset} and, for a read
      or a write, [+] its {!length}. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  (** The type for faults. One that does not apply to an operation does nothing,
      such as one that AF_UNIX cannot produce on a Unix-domain socket: [Timeout]
      and [Unreachable] on a connect, and [Drop], [Dup], [Reorder], [Garble] and
      [Hang] on a datagram. [Reset] on the accept of a Unix-domain connection is
      a client that closed, as [Eof]. *)
  type t =
    | Short of int
        (** A read or a write moves at most [max 1 n] bytes. A write cut short
            found the socket's buffer full, so the next write on that end waits,
            as on eio_posix, and other fibers run meanwhile. *)
    | Stall of float
        (** The operation waits that many seconds of the clock, then runs. *)
    | Hang
        (** On a connection, a read or a write that meets it silences the
            connection for good, as when the peer's host loses power or a
            partition cuts the path, and on AF_UNIX as when the peer's process
            hangs: from then on nothing crosses it either way, and no close or
            reset comes. Each end reads what is queued to it on a Unix-domain
            connection, as AF_UNIX queues at the reader what a returned write
            sent (unix(7)), and nothing more over IP, not even an end of stream
            already sent, then waits. Its writes fill its buffer, then wait.
            Neither end's shutdown or close reaches the other. A wait ends with
            a cancel or the end's own shutdown: a read then returns end of file,
            and a write raises [Connection_reset]. Of later faults on the
            connection, only {!constructor-Short} and {!constructor-Stall} act.
            Over TCP the end's kernel also gives up, with [ETIMEDOUT]: after its
            keepalive probes, or once a byte it sent goes unacknowledged for
            [TCP_USER_TIMEOUT], or for 948.6 s on Linux and 18 s on macOS
            ({!Eio_net_mem.v}). An end with no keepalive and nothing
            unacknowledged waits for ever, so a program with no timeout of its
            own hangs. {!run} silences a connection at a read. On a network
            other than {!Eio_net_mem}, where the wrapper does not see the peer,
            only this end's reads go silent.

            On a connect, an accept or a receive, the operation waits until a
            cancel ends it; a connect never does in {!run}, as the kernel gives
            up on it. It holds that one operation: a datagram taken by a receive
            that hangs goes to the next receive, and a connection taken by an
            accept that hangs to the next accept. A receive on a Unix-domain
            datagram socket does not hang: the datagram it took was queued when
            its send returned. *)
    | Reset
        (** Over IP, the connection is reset at both ends, and the bytes not yet
            read are lost. At each end the first read or write that follows
            raises [Connection_reset]; later reads return end of file and writes
            keep raising it, as on Linux and macOS. A Unix-domain connection has
            no reset to send: it is the crash of one end, the peer on a read and
            this end on a write, which closes its socket. AF_UNIX keeps in the
            reader's queue what a returned write sent, so each end reads that
            first. The other end then reads [Connection_reset] once, if the
            crashed end left bytes unread, as AF_UNIX reports it, then end of
            file; the crashed end reads end of file. The crashed end's code
            still runs, so it still reads what was queued to it: a program may
            hold that what its peer's returned writes sent arrives. The writes
            of both raise [Connection_reset], as AF_UNIX's EPIPE, which leaves
            the reset to the reads. On a network other than {!Eio_net_mem}, each
            end of a Unix-domain connection reads what is queued, then
            [Connection_reset] once. On an accept, the accept succeeds and the
            connection it returns is reset, as Linux hands over one whose client
            left first. On a connect, it is a refusal, as a reset answering the
            handshake is. *)
    | Eof
        (** One end closes, as a FIN does: no byte already written is lost. On a
            read, the peer closed: the read returns what the peer wrote before,
            then end of file. On a write, this end closed: the write raises
            [Connection_reset], and the peer reads the bytes written before,
            then end of file. On an accept, the client closed. After it, the end
            that closed reads end of file and its writes raise
            [Connection_reset]; the other end's first write goes through, its
            bytes lost, and the next ones raise [Connection_reset], as on Linux
            after the peer closes. On a Unix-domain connection each of its
            writes raises it, as AF_UNIX raises EPIPE, and the end that closed
            reads what is queued to it first. *)
    | Refuse  (** A connect fails with [Connection_failure (Refused _)]. *)
    | Timeout
        (** A connect fails after 133 seconds of the clock, 75 under [`Macos] or
            21 under [`Windows], as a lost handshake does, with the error
            eio_posix raises then:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ETIMEDOUT,
             "connect-in-progress", "")), _)]. Eio does not classify it as a
            [Connection_failure]. *)
    | Unreachable
        (** A connect fails at once, as one with no route to its host does, with
            the error eio_posix raises then:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENETUNREACH, "connect",
             "")), _)]. *)
    | No_descriptor
        (** An accept fails with the error eio_posix raises when the process has
            no descriptor left:
            [Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (EMFILE, "accept", "")), _)],
            which [Eio_net.Accept.classify] reads as [`Descriptors]. As on
            Linux, the connection stays queued and its client sees nothing, and
            every accept on the network fails the same way until the program
            frees a descriptor, by closing a connection, socket or listener of
            the network, or by ending the switch that holds one. The next accept
            then returns that connection, with its data. {!run} does not try it,
            as {!Eio.Net.run_server} stops on it. *)
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
  ?os:[< Eio_chaos.os ] ->
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  _ Eio.Net.t ->
  [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [wrap ~os ~clock f net] is [net] with [f] asked about each operation before
    it runs, on the connections and the datagram sockets it makes. The operation
    meets the fault [f] answers; [clock] times stalls and timeouts. [f] may also
    act: count or log. [getaddrinfo], [getnameinfo] and datagram sends pass
    through. [os] (defaults to [`Linux]) is the kernel whose rules the faults
    follow, as {!run} says: pass the one [net] follows.

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

    After the same operation fails 10,000 times on the same target while [clock]
    stands still, with no success there in between, the next attempt raises
    [Failure], as the program that makes them spins for ever, and after 20,000
    it waits for ever. An end of file counts only against the connection it
    ended: a read of it again and again spins, a reconnect after each does not.

    A read or a write after the flow's own close raises [Invalid_argument]
    before [f] is asked, as Eio's own sockets do, and a read under way as
    another fiber closes the flow hands up what arrives, as on eio_posix. A read
    takes from the socket no more than it returns, so the bytes not yet read
    stay in the socket's queue, where a shutdown or a close finds them. A close
    drops the datagrams a socket holds back, and so does the end of the switch a
    socket was made under, which closes it. With an [f] that answers [None],
    each operation does what it does on [net], but for that guard. *)

val pair :
  ?os:[< Eio_chaos.os ] ->
  clock:_ Eio.Time.clock ->
  (Op.t -> Fault.t option) ->
  [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
  * [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
(** [pair ~os ~clock f] is a connected pair of in-memory streams, the client end
    first, with [f] asked about each read and write of either end, as {!wrap}
    asks: [socket] is [1]. Its streams follow [os] (defaults to [`Linux]), as
    {!Eio_net_mem.pair} takes it. A fuzzer drives it from its input. *)

val seeded :
  ?dribble:int ->
  ?stall:int ->
  ?short_write:int ->
  ?reset_after:int ->
  ?reset_kind:[ `Reset | `Eof ] ->
  ?fail_write_after:int ->
  int ->
  Op.t ->
  Fault.t option
(** [seeded ~dribble ~stall ~short_write ~reset_after ~reset_kind
     ~fail_write_after seed] is a fault function for one end of a connection,
    drawn from [seed], as a fuzzer whose input is a seed drives {!pair}: each
    read moves at most [1..dribble] bytes (default: as many as it asks for) or,
    one time in [stall + 1], first waits a {!Fault.Stall} of no time, which lets
    the other fibers run; each write moves at most [1..short_write] bytes. A
    read past [reset_after] bytes meets [reset_kind] ([`Reset] by default, or
    [`Eof]), and a write past [fail_write_after] bytes a reset. Reads and writes
    draw from [seed] apart, so the faults of each do not depend on how the two
    interleave. Connects, accepts and receives meet none. *)

val by_side :
  client:(Op.t -> Fault.t option) ->
  server:(Op.t -> Fault.t option) ->
  Op.t ->
  Fault.t option
(** [by_side ~client ~server op] asks [client] about [op] when it is an
    operation of the end that connected, and [server] when it is one of the end
    that accepted. *)

(** {1:exploration Exploration}

    A check runs the program once with no fault and records its operations: its
    connects, accepts, reads, writes and receives, in order. Its scenarios are
    places in that run, chosen from its structure.

    The structural elements of a sequence are its first three, its last, and the
    first of each class. The length class of a length is that length rounded
    down to a power of two.

    - The {e structural connections} of a run are those of its connections,
      classed by their address family and the kinds and length classes of the
      operations of each end: a refused connect is a connection of its own.
    - The {e structural reads and writes} of an end are those of its reads and
      of its writes, classed by length, and the {e structural datagrams} of a
      datagram socket are those of the datagrams it receives, classed by length.
    - A read or a write may also be cut inside, at a {e place}: a read at the
      end of each write of the peer whose bytes it returns, and over IP the byte
      either side of it, as TCP splits and joins segments; a write over IP at
      the byte after its start and the byte before its end, and the byte before
      the start of the next one. A place is structural when the write that made
      it is.

    The scenarios come in two tiers:

    - the linear tier always runs whole. On each end of each structural
      connection it fails each structural read and write at its start and at
      each structural place, and it fails the connection's connect and accept.
      On each datagram socket it fails each structural datagram. It runs each
      lasting condition. It grows with the structure of the exchange, not with
      its bytes, its clients or its messages;
    - the sampled tier spends a fortieth of the budget, or half as much as the
      linear tier if that is more: the faults the linear tier left, on other
      connections, reads, writes and places, and pairs of faults, the second at
      an operation of the run the first one changed. That is where a retry, or a
      second connection after a reset, gets tested. When it has fewer runs than
      scenarios of a kind, it splits them, in order, into as many groups of
      neighbours as it has runs, and runs one scenario of each group, drawn from
      a seed. Each draw may land on any scenario of its group, so a pattern that
      repeats along the scenarios, as the faults tried at each place do, hides
      no kind of fault from it, where taking every k-th scenario may meet only
      one. A larger budget tries more only once a fortieth of it passes half the
      linear tier, so raising it a little may change nothing.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the program or raise [~budget]. The
    first run stops as soon as the tier needs more, so a program that polls for
    ever costs no more than the budget allows, and the report shows its trace up
    to there. *)

(** {1:contracts Contracts}

    {b At most two faults per run.} A program that tries three times always
    succeeds, so [app] may demand success. A broker that fans a message out to
    three subscribers may demand that one of them gets everything: two faults
    reach two subscribers at most. The second fault of a pair may be a mild one,
    a read or a write that moves one byte, or a connect or a datagram held back
    for 50 ms, and it counts as one of the two. A lasting condition runs alone
    and counts as one fault, though it reaches every connection of the run.

    {b Every program on the network meets the faults}, the test's own peers
    included. A connection may go silent for good, as when a host loses power,
    and no close or reset then ends a wait on it, only the kernel's keepalive or
    retransmission timeouts: give each read a timeout, and each write that may
    fill a socket's buffer, in test servers and clients alike. Write test
    servers with [Eio.Net.run_server ~on_error:ignore], and parse strictly: a
    peer that takes a partial line for a whole one turns an injected end of
    stream into a wrong answer.

    {b Let time pass between retries.} A program that reconnects at once, with
    no sleep, spins under the outage, and fails as a busy loop or a hang
    ({!section-runs}).

    {b Ports are held as on Linux.} The end of a connection that closes first
    holds its port for 60 seconds of the run's clock, in TIME_WAIT. A server
    that listens again on the same port, within a run or in the restart that
    follows it, fails with an address in use, as it would on Linux: listen with
    [~reuse_addr:true], as a real server does.

    {b A restart is a fresh process.} It runs on the network the fault left,
    healed, and so does [inspect]: neither may read state an earlier run left in
    memory. They share the run's event loop, its clock and its limits
    ({!section-runs}).

    {b Each run makes the same kinds of operation.} A check aims a fault at an
    operation by its kind and its rank among the operations of that kind, never
    by an address, so the ports and socket paths a program draws may change from
    run to run, and a report says where two runs with no fault used other
    connections, on its [diverged] line. Two runs with no fault that differ in
    the kinds of operation they make, or in their order, fail the check:
    {!type-env} says what can make them differ.

    {b Assert latency to catch a missing timeout.} A connect that times out
    takes 133 seconds, as on Linux, and then fails: a program without a connect
    timeout waits for it and still succeeds. A test that bounds its program's
    latency reads [env#clock] before and after.

    {b What a check does not catch.} A bug that needs a datagram delayed with
    others, or for longer than 50 ms, may pass: the linear tier delays one
    datagram at a time. A program that never fails the calls still waiting on a
    connection that died passes unless the program waits for every call and
    checks how each ended. A garbled datagram is random bytes, so a parser that
    rejects most noise but accepts some may pass. *)

(** {1:replay Reports and replay}

    A failing run raises [Failure] with the fault, the operations of the failing
    run, how it ended when its [inspect] or restart failed, what it printed, and
    the [Eio_net_chaos.run ~replay:"..." app] call that runs that scenario
    alone, which passes [~os:`Macos] for a check under macOS; a failing run with
    no fault, with its operations. Each operation shows the fault it met, and a
    fault inside a read or a write says after how many of its bytes it fell.
    Repeated operations are folded and a long report is cut around its first
    fault.

    The scenario is found by the operation it names and its fault, so a replay
    still runs after a fix that changes the program's first run: its report then
    says the scenario was re-matched, and a re-matched replay that passes
    returns as any passing check does, saying nothing. A replay whose scenario
    the check no longer has fails, and one whose scenario is not of the form a
    report names is refused.

    The sampled tier draws its scenarios from a seed, the same one on every
    check unless [~seed] gives another, so a check meets the same faults each
    time. A replay names its scenario, not the draw that found it, and the
    program's entropy does not depend on [~seed]: the replay line needs no
    [~seed].

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night, or other faults in the sampled tier, reads its
    own:
    {[
    let check app =
      let int name = Option.map int_of_string (Sys.getenv_opt name) in
      Eio_net_chaos.run ?budget:(int "CHAOS_BUDGET") ?seed:(int "CHAOS_SEED")
        app
    ]} *)

(** {1:runs Runs}

    Each run, with its restart, has its own event loop, inside or outside
    another one. A check runs to its end before it returns, so it blocks the
    event loop it is called from: the other fibers of that loop wait for it.

    Each run's clocks start at [0.] and move to the next timer when every fiber
    waits, so a wait takes no time. Each turn, a fiber resumed by a yield, an
    I/O completion or a timer, also moves them by 1 us, as CPU time passes
    between two, and so does each network operation, one that never waits
    included: a listen, a send, a lookup or a close. A call of
    [Eio_unix.run_in_systhread] runs its function at once, in the run's own
    thread, so the run stays deterministic. Five guards stop a run that does not
    end:

    - a run in which every fiber waits and no timer is due fails as a hang. When
      the program's own fiber is in no network operation, as a switch whose body
      returned waits on the fibers it forked, the report names up to five of the
      others by the operation each waits in and the last one it made;
    - a retry that never waits meets the spin guard: a run fails as a hang when
      the same network operation fails 10,000 times on the same target while its
      clock stands still, as a loop that retries at once makes them: a connect
      to one address, or a read or a write on one accepted connection or on any
      of the connections a client made to one address. Faults on distinct
      targets, as a scan of many ports makes, do not count together, and an end
      of file is an answer: it counts only against the connection it ended;
    - a run that makes 100,000 connects, accepts, reads, writes and receives
      fails as a busy loop, or as too large for a check, so a run's operations
      stay bounded. The reads and writes a one-byte condition splits do not
      count: a report of a run stopped while they ran says how many there were;
    - a run stops at 10,000,000 turns. If no fiber waited 1 ms or more for a
      timer in the last 10,000,000, it fails as a busy loop: a poll by yields,
      or by sleeps shorter than 1 ms, that never ends. Otherwise it fails as a
      busy loop or as too large for a check: a check repeats the run hundreds of
      times, so it refuses a longer one, as it refuses one past its budget.
      Fibers on timers take turns too: a thousand on a heartbeat of 100 ms reach
      10,000,000 in 1000 s of clock;
    - a run still going after an hour of its clock fails as a hang: a poll that
      sleeps 1 ms or more between its tries and never ends. *)

(**/**)

(* For this package's tests: the allowances of [conformance], those a network's
   answers needed, and [conformance] on the host's own network. *)
module Private : sig
  val conformance_allowances : (Eio_chaos.os * string) list
  (** [conformance_allowances] is each allowance of {!val-conformance}, by the
      kernel it applies to and why it holds. *)

  val conformance_defects : string list
  (** [conformance_defects] is each eio_windows defect {!val-conformance} allows
      under [`Windows], by its finding. *)

  val conformance_allowed :
    Eio_chaos.os -> (sw:Eio.Switch.t -> env -> _ Eio.Net.t) -> string list
  (** [conformance_allowed os net] is why each allowance the answers of [net]
      needed holds, as {!val-conformance} replays the traces of [os] on it. *)

  val conformance_in_loop :
    ?os:[< Eio_chaos.os ] -> (sw:Eio.Switch.t -> _ Eio.Net.t) -> unit
  (** [conformance_in_loop ~os net] is {!val-conformance} with each sequence in
      the caller's event loop, for the host's own network, which only the host's
      event loop drives: the test of the kernel's traces runs it there. *)
end

(**/**)
