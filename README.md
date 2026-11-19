# eio-net

An in-memory network for [Eio](https://github.com/ocaml-multicore/eio),
to test code under network faults, and frame channels to build programs on.

The network is used through the ordinary `Eio.Net` API, so the code under
test does not change. On top of it, `Eio_net_chaos.run` runs a program
again and again, each time with a different network fault, and checks that
the program starts again on what the fault left. `Eio_net_chaos.conformance`
checks that a network of your own answers as Linux, macOS or Windows does.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-net
```

## An in-memory network

`Eio_net_mem.v ~clock ~sw ()` is a new network with a loopback interface,
whose TCP timers run on the monotonic clock `clock`:

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

It has the sockets of a real kernel, with TCP, UDP and Unix-domain sockets,
buffers, backlogs and ports that stay held after a close. It answers as Linux
does by default, and as macOS or Windows with `~os`, as recorded on real
kernels. No packets exist: nothing below the sockets API is simulated.

## Testing a program

`Eio_net_chaos.run` takes the program as a function of its environment and
runs it many times on a fresh network. A client is tested together with a
server to talk to:

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

The first run has no fault. Each later run meets one or two, such as a
connection reset, a peer that goes silent, a refused connect, a short read,
or a lost datagram. After each, the program restarts with no fault, on the
same network, healed but still holding what the faulty run left. The check
fails if a run hangs, if the program raises something the network cannot
explain, or if the restart fails.

So a program with no timeout hangs on a silent peer, and a server that
listens without `~reuse_addr:true` cannot restart, because its old port is
still held:

<!-- $MDX os_type<>Win32 -->
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

The report ends with one line that replays the case, and the
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos) README says more about
how faults are chosen and what a pass means.

`Eio_net_chaos.network ()` is the same network as a device of eio-chaos, to
test a program on several simulated devices at once, such as the network and
the disk of [eio-fs](https://github.com/samoht/ocaml-eio-fs).
[`eio_net_chaos.mli`](lib/chaos/eio_net_chaos.mli) documents the rest,
including how to read a report and how to test a server that must survive a
client's reset.

## Testing a network

`Eio_net_chaos.conformance` checks that an Eio network of your own, such as a
userspace stack or a tunnel, answers as the kernel does. It takes a function
that makes a fresh network, and replays on it sequences of socket operations
recorded on real Linux, macOS and Windows machines:

```ocaml
# Eio_net_chaos.conformance (fun ~sw env ->
      Eio_net_mem.v ~clock:env#mono_clock ~sw ());;
- : unit = ()
```

The sequences cover every short order of listen, connect, accept, read,
write, shutdown and close, over TCP, UDP and Unix-domain sockets, on IPv4 and
IPv6. The check compares what each call returns, not what goes on the wire,
and reports each step whose answer the kernel does not allow. A network built
over another one, such as a tunnel, can also be checked while the network
under it fails.

## Libraries

- `eio-net` has `Eio_net`: frame channels, in which one receive is one
  frame, over datagram sockets and byte streams, with an accept loop that
  caps its connections and a relay.
- `eio-net.mem` has `Eio_net_mem`, the in-memory network.
  [MODEL.md](MODEL.md) says how closely it follows each kernel.
- `eio-net.chaos` has `Eio_net_chaos`, the two checks above.
- `eio-net.addr` converts [Ipaddr](https://github.com/mirage/ocaml-ipaddr)
  addresses to Eio's and back.
- `eio-net.unix` binds batched datagrams, Unix-domain sockets and the host's
  interfaces. It is not built on Windows.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Related work

The kernel traces follow the idea of
[Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/), which checked a
specification of TCP against traces of real kernels, and the
[socket cases](test/oracle/README.md) adapt [gVisor](https://gvisor.dev/)'s
socket tests.

## Licence

ISC, see [LICENSE.md](LICENSE.md).
