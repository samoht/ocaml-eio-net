# eio-net

An in-memory network for [Eio](https://github.com/ocaml-multicore/eio),
to test what a program does when a connection drops or a peer goes silent.
It also has frame channels, which send and receive one whole message at a
time.

A test gets a network of its own and uses it through `Eio.Net`, as it would
the host's, so the code under test does not change. A client and a server
talk in one test, with no free port to find on the host:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c = Eio.Net.connect ~sw net addr in
  let s, _ = Eio.Net.accept ~sw l in
  Eio.Flow.copy_string "world" c;
  Eio.Flow.shutdown c `Send;
  Eio.Flow.read_all s;;
- : string = "world"
```

You would reach for eio-net when:

- a test needs a client and a server to talk and should leave the host's
  network alone ([a disposable network](#a-disposable-network));
- you want to know whether a program recovers after a reset, a timeout or a
  lost datagram ([testing a program](#testing-a-program));
- you wrote a network for Eio, such as a tunnel, and want it to answer as a
  real one does ([testing a network](#testing-a-network));
- a protocol exchanges whole messages over a socket or a byte stream
  ([frame channels](#frame-channels)).

## Installation

<!-- $MDX skip -->
```sh
opam install eio-net
```

eio-net needs OCaml 5.4 or later and Eio 1.7 or later.

## Usage

### A disposable network

`Eio_net_mem.v ~clock ~sw ()` is a new network whose sockets live in memory.
Two calls give two networks that share no socket. Its TCP timers run on
`clock`, so with a mock clock a test waits for no real time.

It has TCP, UDP and Unix-domain sockets, with the buffers and backlogs of a
real kernel. A port stays held for a while after a close, as on a real host.

It answers as Linux does by default. With `~os` it answers as macOS or
Windows does, whatever the host, with the answers recorded on real machines.
[MODEL.md](MODEL.md) says how closely it follows Linux and macOS, and
[`eio_net_mem.mli`](lib/mem/eio_net_mem.mli) says what each operating system
changes and where the simulation stops.

### Testing a program

`Eio_net_chaos.run` takes the program as a function of its environment, the
value `Eio_main.run` would give it. It runs the program many times, each time
on a fresh in-memory network at `env#net`. A client is tested together with a
server to talk to:

```ocaml
# let app ~reuse_addr (env : Eio_net_chaos.env) =
    Eio.Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
    let l = Eio.Net.listen ~sw ~reuse_addr ~backlog:5 env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
            Eio.Flow.copy_string "hello\n" flow));
    let get () =
      let flow = Eio.Net.connect ~sw env#net addr in
      let r = Eio.Buf_read.of_flow ~max_size:64 flow in
      let line = Eio.Buf_read.take_while (( <> ) '\n') r in
      Eio.Buf_read.char '\n' r;
      line
    in
    match Eio.Time.with_timeout env#clock 5.0 (fun () -> Ok (get ())) with
    | Ok "hello" | Error `Timeout -> ()
    | Ok line -> Fmt.failwith "got %S" line;;
val app : reuse_addr:bool -> Eio_net_chaos.env -> unit = <fun>
# Eio_net_chaos.run (app ~reuse_addr:true);;
- : unit = ()
```

The first run has no fault. Each later run meets one or two, such as a
connection reset, a refused connect or a lost datagram. In some runs the peer
goes silent and sends nothing more. After each of those runs, the program
starts again with no fault on the same network, which works again but still
holds what the faulty run left. The check fails if a run hangs, if the
program raises something the network cannot explain, or if the restart fails.
It then raises `Failure` with a report, so the call is the body of a test in
any test runner.

A program with no timeout hangs on a silent peer, so the client above gives
up after five seconds. The clock is simulated, so the test does not wait that
long. A server that listens without `~reuse_addr:true` cannot restart,
because the connection the fault ended still holds its port:

<!-- $MDX os_type<>Win32,non-deterministic=output -->
```ocaml
# match Eio_net_chaos.run (app ~reuse_addr:false) with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_net_chaos.run: the restart failed: the program, run again with no fault on what the faulty run left, raised Eio.Io Unix_error (Address already in use, "bind", "")
  faults:   #2 read 1c @0+64 ends
  ended:    the faulty run raised End_of_file
  error:    Eio.Io Unix_error (Address already in use, "bind", "")
  trace:
    #1 connect 1c @0
    #2 read 1c @0+64  <- ends
    #3 accept 1s @0
  replay:   Eio_net_chaos.run ~replay:"net/29d1c4cd/read:1.eof" app
- : unit = ()
```

The last line of the report, pasted into the test, runs that one case again.

A program that also uses the disk is tested on both at once, with
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos) and
[eio-fs](https://github.com/samoht/ocaml-eio-fs):
`Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] app`. The
eio-chaos README says how the faults are chosen and what a pass means.
[`eio_net_chaos.mli`](lib/chaos/eio_net_chaos.mli) documents the rest, such
as how to test a server that must survive a client's reset.

### Testing a network

`Eio_net_chaos.conformance` is for a network of your own, such as a tunnel
over another network. It takes a function that makes a fresh network. It runs
short sequences of socket operations on that network and compares each answer
with the one recorded on a real machine:

```ocaml
# Eio_net_chaos.conformance (fun ~sw env ->
      Eio_net_mem.v ~clock:env#mono_clock ~sw ());;
- : unit = ()
```

The sequences cover the short orders of listen, connect, accept, read, write,
shutdown and close, over TCP, UDP and Unix-domain sockets. The check looks at
what each call returns, and reports each step whose answer the kernel does
not allow. It compares with Linux by default, and with macOS or Windows with
`~os`. With `~under`, a network built over other devices, such as a tunnel,
is also checked while those devices fail.

### Frame channels

`Eio_net.Frame` is a channel on which one receive returns one whole message,
called a frame, however many reads it takes. It runs over a datagram socket,
or over a byte stream on which each frame follows its length:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  let a, b = Eio_net_mem.pair () in
  let tx = Eio_net.Frame.of_length_prefixed_flow a in
  let rx = Eio_net.Frame.of_length_prefixed_flow b in
  Eio_net.Frame.send tx [ Cstruct.of_string "hello" ];
  Eio_net.Frame.send tx [ Cstruct.of_string "world" ];
  let first = Eio_net.Frame.recv rx in
  (first, Eio_net.Frame.recv rx);;
- : string * string = ("hello", "world")
```

For a server, `Eio_net.Accept.run` is an accept loop that serves at most a
given number of connections. When it is full, it drops the connection whose
request has made the least progress. [`frame.mli`](lib/frame.mli) and [`accept.mli`](lib/accept.mli)
document both.

## Libraries

- `eio-net` has `Eio_net`, helpers for any Eio network, such as frame
  channels and an accept loop that bounds its connections.
- `eio-net.mem` has `Eio_net_mem`, the in-memory network.
- `eio-net.chaos` has `Eio_net_chaos`, the two checks above.
- `eio-net.addr` has `Eio_net_addr`, which converts
  [Ipaddr](https://github.com/mirage/ocaml-ipaddr) addresses to Eio's and
  back.
- `eio-net.unix` has `Eio_net_unix`, the socket calls that Eio lacks, such as
  batched datagrams and the host's interface addresses. It is not built on
  Windows.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Related work

`conformance` checks a network against recordings of real kernels, where
[Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/) checked a specification of
TCP against traces of real kernels. The
[socket cases](test/oracle/README.md) adapt [gVisor](https://gvisor.dev/)'s
socket tests.

## Licence

ISC, see [LICENSE.md](LICENSE.md).
