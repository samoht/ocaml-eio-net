# eio-net

Network tests and channels for [Eio](https://github.com/ocaml-multicore/eio).

`eio-net.chaos` answers two questions, with one call each: does an Eio
program cope with a network that fails, and does an Eio network answer as
Linux or macOS does? Both run on `eio-net.mem`, an in-memory network that
follows either kernel. The package also has the frame channels and the accept
loop that programs build on.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-net
```

## Testing a program

Your program talks to the network through `Eio.Net`. What does it do when the
network fails? `Eio_net_chaos.run` takes the program as a function of its
environment and runs it many times on a network in memory. The program needs
no change. A client is tested together with a server to talk to, and a server
with a client:

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
the network did fail, so the restart decides. A program with no timeout hangs on a silent peer, and a
server that listens without `~reuse_addr:true` cannot restart:

```ocaml
# match Eio_net_chaos.run (app ~reuse_addr:false) with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_net_chaos.run: the restart failed: the workload, run again with no fault on the same network, did not return
  faults:   accept of 1s at 0 is reset
  ended:    the faulty run raised Eio.Io Net Connection_reset Injected_fault (eio-net.chaos)
  error:    Eio.Io Unix_error (Address already in use, "bind", "")
  trace:
    #1 connect 1c @0
    #2 read 1c @0+64
    #3 accept 1s @0  <- is reset
  replay:   Eio_net_chaos.run ~replay:"net/29d1c4cd/accept1s@0.reset" app
- : unit = ()
```

A failure report gives the fault, the operations of the run and one line to
paste, which runs that case alone.
[`eio_net_chaos.mli`](lib/chaos/eio_net_chaos.mli) says how the faults are
picked and what a program may and may not do.

## Testing a network

You have written an Eio network: a userspace stack, a tunnel, a mock. Does it
answer as Linux does? `Eio_net_chaos.device` takes a function that makes a
fresh network and runs on it the sequences of socket operations that were
recorded on a real Linux and a real macOS kernel:

```ocaml
# Eio_net_chaos.device (fun ~sw env ->
      Eio_net_mem.v ~mono:env#mono_clock ~sw ());;
- : unit = ()
```

The sequences cover TCP, UDP and Unix-domain sockets, on IPv4 and IPv6:

- every order of up to four of listen, connect, accept, read, write, shutdown
  and close;
- every order of up to three datagram operations;
- binds, backlogs and the reuse of an address after a close;
- Unix socket paths, name resolution and socket options.

Each step has the set of answers that the kernel allows. The check runs every
sequence, then reports each step whose answer is outside its set, as
`read S 1: expected one of {EOF}, got would wait`, with a summary. Pass
``~os:`Macos`` to compare with macOS.

The in-memory network of eio-net (`Eio_net_mem.v`) passes this check for both
systems. Three kinds of recorded sequence are left out, because only a
prepared host can answer them: those that wait a minute for a port to be free
again, those that use up every port, and those that send off the loopback
interface.

## Libraries

- `eio-net.chaos`: `run` and `device`, and the fault wrapper they use.
- `eio-net.mem`: the in-memory network. [MODEL.md](MODEL.md) says how
  closely it follows each kernel.
- `eio-net`: frame channels over datagram sockets and byte streams, a relay,
  and a guard on a transfer's progress.
- `eio-net.framing`: the length-prefix codec of a frame channel.
- `eio-net.accept`: an accept loop that holds at most a given number of
  connections.
- `eio-net.connect` and `eio-net.addr`: failed connects and addresses.
- `eio-net.unix`: batched datagrams, Unix-domain sockets and the host's
  interfaces.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Limitations

- `eio-net.mem` is a host with a loopback interface only, and follows one
  kernel at a time. Run a check under each `?os` you ship on.
- A run meets at most two faults, so `run` misses a bug that needs three.
- Under macOS, a TCP answer over loopback can depend on timing, and the traces
  hold one recording of each.

## Licence

ISC, see [LICENSE.md](LICENSE.md). The kernel traces follow the idea of
[Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/), which checked a
specification of TCP against traces of real kernels, and the
[socket cases](test/oracle/README.md) adapt
[gVisor](https://gvisor.dev/)'s socket tests.
