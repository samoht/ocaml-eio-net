# eio-net

An in-memory network for [Eio](https://github.com/ocaml-multicore/eio),
to test code under network faults, and frame channels to build programs on.

Each test gets a fresh network of its own, used through the ordinary
`Eio.Net` API, so the code under test does not change.
`Eio_net_chaos.run` is chaos engineering on one machine: it runs a program
again and again, each time with a different network fault, and checks that
it starts again on the network the fault left
([How it works](#how-it-works)). `Eio_net_chaos.device` checks that an Eio
network answers as Linux or macOS does. The frame channels, in which one
receive is one frame, come with an accept loop and a relay.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-net
```

## An in-memory network

`Eio_net_mem.v ~sw ()` is a new network with a loopback interface. Two calls
share no socket.

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c = Eio.Net.connect ~sw net addr in
  let s, _ = Eio.Net.accept ~sw l in
  Eio.Flow.copy_string "world" c;
  Eio.Flow.shutdown c `Send;
  Eio.Flow.read_all s;;
- : string = "world"
```

## Testing a program

`Eio_net_chaos.run` checks how a program that talks through `Eio.Net` copes
when the network fails. It takes the program as a function of its
environment and runs it many times in memory. A client is tested together
with a server to talk to, and a server with a client:

```ocaml
# let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080);;
val addr : [> `Tcp of [> `V4 ] Eio.Net.Ipaddr.t * int ] =
  `Tcp ("\127\000\000\001", 8080)
# let app ~reuse_addr (env : Eio_net_chaos.env) =
    Eio.Switch.run @@ fun sw ->
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

The `env` has the capabilities of `Eio.Stdenv`, each simulated
([How it works](#how-it-works)), so a `main` written against `Eio.Stdenv`
runs unchanged. `Eio_net_chaos.network ()` is the same network as a part of
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos), to test a program on
it and other simulated devices at once. A program that also writes files is
tested on the disk of [eio-fs](https://github.com/samoht/ocaml-eio-fs) in the
same check, whose runs fail the disk, the network, or both:
`Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] app`.

The first run has no fault. Each later run meets one fault:

- a connection is reset, or the peer closes it midway;
- a peer goes silent for good, as when its host loses power;
- a connect is refused or times out;
- a read returns one byte, or a write fills the socket's buffer;
- a datagram is lost, duplicated or reordered.

After each of those runs, the check starts the program again with no fault, on
the same network. The network has healed, but it keeps what the first run
left, such as a port that is still held after a close.

The check fails if a run hangs, if it raises anything other than `Eio.Io`,
`End_of_file` or `Eio.Time.Timeout`, or if the restart does not end normally.
One of those three that escapes a run with a fault is not a failure in itself:
the network did fail, so the restart decides. A program with no timeout hangs
on a silent peer, and a server that listens without `~reuse_addr:true` cannot
restart:

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

A report gives the fault, the operations of the run, what the program printed,
if anything, and one line to paste that runs that case again; to find it, a
replay may run the program a few times first, and the failing run is the
last. An operation reads `#3 accept 1s @0`: the third of the run, an accept on socket
1, at the end that accepted (`c` is the end that connected), after 0 earlier
accepts; a read or a write adds the bytes it asks for or offers, as in
`#2 read 1c @0+64`, 64 bytes from offset 0.

A server that dies when one client resets passes too, because its restart
works. To catch that, the program asserts that a later request succeeds, as
`assert (get () = "hello")` after the one that met the fault. A run meets at
most two faults, so the third try always succeeds, except under the outage,
which `Eio_net_chaos.is_outage env` names.
[`eio_net_chaos.mli`](lib/chaos/eio_net_chaos.mli) says how the faults are
picked and what a program may and may not do.

## Testing a network

`Eio_net_chaos.device` checks that an Eio network of your own, such as a
userspace stack or a tunnel, answers as Linux or macOS does. It takes a
function that makes a fresh network and runs on it the sequences of socket
operations that were recorded on a real Linux and a real macOS kernel:

```ocaml
# Eio_net_chaos.device (fun ~sw env ->
      Eio_net_mem.v ~clock:env#mono_clock ~sw ());;
- : unit = ()
```

The sequences cover TCP, UDP and Unix-domain sockets, on IPv4 and IPv6:

- every order of up to four of listen, connect, accept, read, write, shutdown
  and close;
- every order of up to three datagram operations;
- binds, backlogs and the reuse of an address after a close;
- Unix socket paths, name resolution and socket options.

The check compares the answers of socket calls over loopback as `Eio.Net`
returns them (a result, an operation that would wait, an error class or an
errno), not the segments on the wire. Each step has the set of answers that
the kernel allows. The check runs every sequence, then reports each step
whose answer is outside its set, once for all the sequences that gave it,
as in this line, where `C` is the end that connected and `S` the one that
accepted:

`connect: expected ok, got Refused, in 21,361 sequences, e.g. tcp4 accept; close S; close S; connect`

Pass ``~os:`Macos`` to compare with macOS.

A network built over another one, such as a tunnel, is checked over a faulty
one too with `~under:[ Eio_net_chaos.network () ]`: on a healthy network its
answers must be the kernel's, and once the network under it failed, it must
not hang or give an error no network gives.

The in-memory network of eio-net (`Eio_net_mem.v`) passes this check for both
systems, in about three seconds. Three kinds of recorded sequence are left
out, because only a prepared host can answer them: those that wait a minute
for a port to be free again, those that use up every port, and those that send
off the loopback interface. Under macOS, so are those whose answer depends on
when a reset arrives.

## How it works

### Chaos engineering, on one machine

`Eio_net_chaos.run` is the check of
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos#chaos-engineering-on-one-machine)
on the network alone, and its README says how the faults are picked and what
a pass means. On the network:

- **steady state:** the program returns, and when it is restarted on the
  network a fault left, healed, it starts and works again;
- **faults:** each fault the real kernel can produce at each socket call the
  program made, and an outage;
- **result:** either the hypothesis held for every fault tried, or the first
  counter-example and one line that replays it.

### What is simulated

The program receives each device as an Eio capability, through `env`. The
package replaces each capability with a simulated one; the program is not
changed.

- The network is a host with a loopback interface only, with the kernel's
  socket behaviour (buffers, backlogs, TIME_WAIT, timeouts, Unix socket
  files). No packets exist: nothing below the sockets API is modelled.
- The clocks jump to the next timer, `secure_random` and `Random` are
  seeded, the standard streams are in memory, a domain manager runs its work
  on the run's own scheduler, and no process starts.

A program built with [hermetic](https://github.com/samoht/ocaml-hermetic)
(`open! Hermetic`) cannot escape the check: the compiler rejects direct
`Unix`, standard-library I/O, entropy, domains and processes, so each run
replays exactly, unless the program keeps state in memory from one run to the
next. Without hermetic, a direct call escapes the check, and when it makes
two runs differ, the report names the first difference and its likely
cause. The same holds for a network under `device`.

### What is guaranteed

1. With no fault, the simulated network answers each call as Linux or macOS
   does through Eio, as recorded on real kernels; the Limitations list the
   known differences.
2. Only faults the real kernel can return at that call are injected. So
   every fault reported can happen on a real machine.
3. Completeness is not guaranteed. Faults fall only on the calls the program
   made, at most two per run. A pass means no counter-example was found there.

`device` checks guarantee 1 for an implementation of your own: each call's
answer against the recorded kernel answers. It checks nothing about what goes
on the wire.

## Libraries

- `eio-net` has `Eio_net`: frame channels over datagram sockets and byte
  streams, with the length-prefix codec of a byte stream, an accept loop
  that holds at most a given number of connections, a relay, and a guard
  that ends a transfer that moves too few bytes a second.
- `eio-net.mem` has `Eio_net_mem`, the in-memory network.
  [MODEL.md](MODEL.md) says how closely it follows each kernel.
- `eio-net.chaos` has `Eio_net_chaos`, the two checks above and the faults
  they are made of, so a program that uses only the network links none of
  it.
- `eio-net.connect` classifies failed connects.
- `eio-net.addr` converts [Ipaddr](https://github.com/mirage/ocaml-ipaddr)
  addresses to Eio's and back.
- `eio-net.unix` binds batched datagrams, Unix-domain sockets and the host's
  interfaces.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Limitations

- The network follows one kernel at a time, Linux, or macOS with
  ``~os:`Macos``: code that runs on both needs a check under each
  ([`Eio_net_mem`](lib/mem/eio_net_mem.mli)).
- Under macOS, a TCP answer over loopback can depend on timing, and the traces
  hold one recording of each, so `device` leaves those sequences out
  ([`Eio_net_chaos.device`](lib/chaos/eio_net_chaos.mli)).

## Related work

The kernel traces follow the idea of
[Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/), which checked a
specification of TCP against traces of real kernels, and the
[socket cases](test/oracle/README.md) adapt [gVisor](https://gvisor.dev/)'s
socket tests.

## Licence

ISC, see [LICENSE.md](LICENSE.md).
