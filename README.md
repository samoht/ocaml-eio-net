# eio-net

Network channels and test networks for
[Eio](https://github.com/ocaml-multicore/eio).

- `Eio_net_chaos.check` runs a workload on an in-memory network under the
  faults of a real one.
- On a frame channel, each receive returns one frame, over a datagram socket,
  a length-prefixed byte stream or an in-memory pair.
- An accept loop bounds the connections it holds.

## Installation

<!-- $MDX skip -->
```sh
$ opam install eio-net
```

## Chaos tests

A real network does what the loopback sockets of a test rarely do, so the bugs
that depend on it stay hidden:

- a read returns a few bytes of a message, or several messages at once;
- a peer resets the connection, ends the stream midway or goes silent for
  good;
- a connect is refused or slow;
- a datagram is lost, duplicated or reordered.

`Eio_net_chaos.check` runs a workload on an in-memory network many times. Each
run meets a different fault, chosen from what the workload does.

In this example a client asks a server for a line. It has a timeout and tries
three times. The server also bounds how long it waits for the request, since a
client may go silent for good. The workload checks what the client got and how
long it took:

```ocaml
# let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080);;
val addr : [> `Tcp of [> `V4 ] Eio.Net.Ipaddr.t * int ] =
  `Tcp ("\127\000\000\001", 8080)
# let strict_line r =
    let line = Eio.Buf_read.take_while (( <> ) '\n') r in
    Eio.Buf_read.char '\n' r;
    line;;
val strict_line : Eio.Buf_read.t -> string = <fun>
# let get ~timeout (env : Eio_net_chaos.env) =
    let once () =
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net addr in
      Eio.Flow.copy_string "GET\n" flow;
      strict_line (Eio.Buf_read.of_flow flow ~max_size:64)
    in
    let rec attempt n =
      match
        if timeout then Eio.Time.with_timeout_exn env#clock 1.0 once
        else once ()
      with
      | line -> Some line
      | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
          if n < 3 then attempt (n + 1) else None
    in
    attempt 1;;
val get : timeout:bool -> Eio_net_chaos.env -> string option = <fun>
# let workload ~timeout env =
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
            let r = Eio.Buf_read.of_flow flow ~max_size:64 in
            let read () = strict_line r in
            match Eio.Time.with_timeout_exn env#clock 5.0 read with
            | "GET" -> Eio.Flow.copy_string "hello\n" flow
            | _ -> ()));
    let t0 = Eio.Time.now env#clock in
    let got = get ~timeout env in
    if Eio.Time.now env#clock -. t0 > 5. then failwith "too slow";
    match (got, Eio_net_chaos.is_outage env) with
    | Some "hello", false | None, true -> ()
    | Some line, _ -> Fmt.failwith "got %S" line
    | None, false -> failwith "gave up";;
val workload : timeout:bool -> Eio_net_chaos.env -> unit = <fun>
# Eio_net_chaos.check (workload ~timeout:true);;
- : unit = ()
```

Without its timeout, the client waits 133 seconds for a connect whose
handshake is lost, and the workload's bound on latency fails:

```ocaml
# match Eio_net_chaos.check (workload ~timeout:false) with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_net_chaos.check: the workload failed under a fault
  faults:   connect of 1c at 0 times out
  error:    Failure("too slow")
  trace:
    #1 connect 1c @0  <- times out
    #2 connect 2c @1
    #3 write 2c @0+4
    #4 read 2c @0+64
    #5 accept 2s @0
    #6 read 2s @0+64
    #7 write 2s @0+6
  replay:   ~replay:"net/85d85f49/connect1c@0.timeout"
- : unit = ()
```

A report names each connection by its number, in the order the network made
them, and its end: `c` for the end that connected and `s` for the end that
accepted. Here `1c` is the first connect, which the injected fault holds for
133 seconds, and `2c` and `2s` are the two ends of the retry. In `@0+4`, `0`
is what that end had moved before and `4` the bytes the operation asks for or
offers. The `replay` argument runs that scenario alone.

### Writing a workload

- **At most two faults reach a run.** The test of a program that tries three
  times can therefore demand success, as in the example. The test of a broker
  that fans a message out to three subscribers can demand that one of them
  gets everything. A lasting condition, such as one-byte reads, counts as one
  fault but reaches every connection.
- **Every program on the network meets the faults**, the test's own peers
  included. Write test servers with `Eio.Net.run_server ~on_error:ignore`,
  catch `Eio.Io _ | End_of_file` in test clients, and parse strictly in both.
- **Give each read a timeout**, and each write that may fill a socket's
  buffer. A connection may go silent for good, as when a host loses power. No
  close or reset then ends a wait on it, only the kernel's keepalive or
  retransmission timeouts.
- **Catch the errors inside each fiber.** Two fibers of `Eio.Fiber.both` that
  both meet a fault fail together with `Eio.Exn.Multiple`.
- **Assert latency.** A connect that times out takes 133 seconds, as on Linux.
  It then fails with the error eio_posix raises, which Eio does not classify
  as a connection failure. A program with no connect timeout still succeeds,
  so the test bounds its latency to catch it.
- **Listen again with `~reuse_addr:true`.** Ports are held as on Linux: the
  end of a connection that closes first holds its port for 60 seconds. A
  server that listens again on its port within a run needs the option.
- **Check the outage run.** In that run every connect is refused and every
  datagram lost. `Eio_net_chaos.is_outage env` is then `true`, and the test
  checks what its program does when it gives up.

### Runs

Each run has its own event loop. Its clock jumps to the next timer when every
fiber waits, so a timeout of an hour takes no time. A check blocks the event
loop it is called from until it ends.

A check is deterministic. It reads no environment variable and prints nothing.
Some of its scenarios are sampled from a fixed seed. A nightly run can pass
another `~seed` to meet other faults, and the replay of a failure needs no
seed.

For a scenario a check does not try:

- `Eio_net_chaos.wrap ~clock f net` asks `f` about each operation of `net`;
- `Eio_net_chaos.pair` gives a fuzzer two connected ends.

### Limitations

`eio-net.mem` is a host with a loopback interface only, and it follows one
kernel at a time: Linux by default, macOS with `` ~os:`Macos ``. A pass says
the program works under that kernel's rules on such a host, so run a check
under each `?os` you ship on. `Eio_net_chaos.wrap` also follows the `?os` it is
given, not the host's. Over a real network, pass the host's kernel.

Kernel traces recorded on both systems check the model. The READMEs of the
[kernel traces](test/interop/kernel/README.md), the
[CPython vectors](test/interop/cpython-struct/README.md) and the
[socket cases](test/oracle/README.md) say where each oracle comes from and how
to regenerate it.

A check can still miss a bug:

- A silent connection ends a wait only as the kernel would, by the times
  measured over loopback: after the keepalive probes, or once a byte sent goes
  unacknowledged for `TCP_USER_TIMEOUT`, or else for 948.6 s on Linux and 18 s
  on macOS. Over a real network the retransmission timeout grows with the
  round-trip time, so a program whose own deadline is close to these may
  behave otherwise there. With no keepalive and nothing unacknowledged, a read
  waits for ever and the check reports a hang.
- A garbled datagram is random bytes, so a parser that accepts some noise may
  pass. Fuzz the parser on its own.
- A datagram is delayed alone, and for 50 ms. `EMFILE` on accept is never
  tried, as `Eio.Net.run_server` stops on it. Ask for longer or grouped
  delays, or for `Eio_net_chaos.Fault.No_descriptor`, with `Eio_net_chaos.wrap`.
- After an AF_UNIX crash, the crashed end's code still runs and reads what was
  queued to it. Check what the other end sees.

A test can also be too large. A run fails after 10,000,000 turns of its fibers
or 100,000 network operations. A check fails when the scenarios it always runs
need more runs than its budget, 2,000 by default. Shrink the workload, or
raise `~budget`.

## Framing

`Eio_net_framing` writes each payload after its length, an unsigned big-endian
integer. In this example the length takes two bytes and a payload is at most 8
bytes long:

```ocaml
# let framing = Eio_net_framing.Config.v ~length_field:Uint16_be ~max_length:8 ();;
val framing : Eio_net_framing.Config.t = <abstr>
# let buffer = Buffer.create 16;;
val buffer : Buffer.t = <abstr>
# Eio_net_framing.write framing (Eio.Flow.buffer_sink buffer)
    [ [ Cstruct.of_string "abc" ]; [] ];;
- : unit = ()
# Buffer.contents buffer;;
- : string = "\000\003abc\000\000"
# let r = Eio.Buf_read.of_string (Buffer.contents buffer);;
val r : Eio.Buf_read.t = <abstr>
# Eio_net_framing.read framing r;;
- : string = "abc"
# Eio_net_framing.read framing r;;
- : string = ""
# Eio_net_framing.read framing r;;
Exception: End_of_file.
# Eio_net_framing.read framing (Eio.Buf_read.of_string "\000\003a");;
Exception: Eio_net_framing.Error: truncated payload: received 1 of 3 bytes
```

`write` sends the whole batch with one `Eio.Flow.write` and does not copy the
payloads.

A framing configuration accepts payloads up to 1 MiB by default, or 65,535
bytes with a two-byte length. `read` raises
`Eio_net_framing.Error (Invalid_length _)` as soon as a prefix declares a
longer payload, before it reads any payload byte. A reader made with
`~max_size:(Eio_net_framing.Config.buffer_size framing)` therefore never holds
more than the longest frame.

`Eio_net.Frame.of_length_prefixed_flow ~framing flow` turns a byte stream into
a frame channel with the same format, and `Eio_net.Frame.of_datagram_socket`
sends one datagram per frame. A datagram longer than a channel's `max_length`
(64 KiB by default) raises the `Eio.Io` error `Too_long` instead of arriving
truncated.

## Accept loop

`Eio_net_accept.run` serves at most `max_connections` connections, and one
more accepted connection waits unserved. A connection's head is what its peer
sends before it is served. When the loop is full and another peer connects, it
drops the connection whose head is incomplete and has made the least progress. An exception one connection raises
ends that connection alone and goes to `?on_error`.

Some protocols read the head through a layer of their own, such as TLS during
its handshake. Wrap the socket with `Eio_net_accept.reading_flow` so that the
bytes the layer reads count as progress.

The loop reads a failed accept with the classifier given as `?classify`. Over
a Unix network, pass `Eio_net_unix.classify_accept`: when the process runs out
of descriptors, the loop then waits for one to be free. With the default
classifier, that error ends the loop.

## Libraries

- `eio-net`: the `Eio_net.Frame` and `Eio_net.Datagram` channels.
  `Eio_net.Relay` forwards connections and `Eio_net.Progress` ends a transfer
  that moves too slowly.
- `eio-net.framing`: the length-prefix codec of a frame channel over a byte
  stream, with bounded frames, over `Eio.Buf_read`.
- `eio-net.accept`: an accept loop that holds at most a given number of
  connections.
- `eio-net.connect`: a classifier of failed connects, refused or unreachable.
- `eio-net.addr`: conversions between `Ipaddr` and Eio addresses, their
  equality, and host name resolution.
- `eio-net.mem`: an in-memory `Eio.Net.t` with stream and datagram sockets,
  and the socket options each kernel takes. A small `?capacity` shows a peer
  that writes without reading.
- `eio-net.chaos`: `check`, which runs a workload under the network faults
  it may meet, and the wrapper it uses.
- `eio-net.unix`: batched datagram transfers, Unix-domain sockets, the host's
  interface addresses and the Unix classifiers of failed connects and accepts.

`eio-net.accept`, `eio-net.connect` and `eio-net.addr` link nothing of a
platform's network, so a unikernel guest can use them as they are.

The `.mli` files document each library. Those of `eio-net` are
[`frame.mli`](lib/frame.mli), [`datagram.mli`](lib/datagram.mli),
[`relay.mli`](lib/relay.mli) and [`progress.mli`](lib/progress.mli). Each
sub-library has its own directory under [`lib/`](lib/).

## What mem models of each kernel

<!-- Made from lib/mem/scope.ml: dune build @runtest --auto-promote -->

eio-net.mem follows the chosen kernel in most of what a test can see. The tables
below give, for each behaviour and each socket option, how closely it follows
Linux and macOS. A modelled entry matches the kernel. An approximated one
differs as its note says. A socket option that is stored only can be set and
read back, and has no other effect. An unsupported one is refused. The tables
are made from [`lib/mem/scope.ml`](lib/mem/scope.ml), and a test checks the
class of each option against what a connection does.

| Behaviour | Linux | macOS |
| --- | --- | --- |
| Ephemeral ports | approximated (1) | approximated (2) |
| An unbound UDP socket's port | approximated (3) | approximated (3) |
| Host resolution | approximated (4) | approximated (4) |
| Unix paths | approximated (5) | approximated (5) |
| A connection's queue with no buffer set | approximated (6) | approximated (7) |
| Loopback delivery | modelled | approximated (8) |
| Datagram memory | modelled | approximated (9) |
| Accept queue | modelled | modelled |
| Routes off loopback | modelled | modelled |
| TIME_WAIT and FIN_WAIT2, on a clock | modelled | modelled |
| A silent connection, on a clock | modelled | modelled |

| Option | Linux | macOS |
| --- | --- | --- |
| `SO_DEBUG` | stored only (10) | stored only |
| `SO_BROADCAST` | stored only (11) | stored only |
| The `reuse_addr` argument | modelled | modelled |
| `SO_REUSEADDR` | stored only (12) | stored only (12) |
| `SO_KEEPALIVE` | modelled | modelled |
| `SO_DONTROUTE` | stored only | stored only |
| `SO_OOBINLINE` | stored only (13) | stored only (13) |
| The `reuse_port` argument | unsupported (14) | unsupported (14) |
| `SO_REUSEPORT` | stored only | stored only |
| `SO_SNDBUF` and `SO_RCVBUF` | approximated (15) | approximated (16) |
| `SO_RCVLOWAT` | stored only (17) | stored only (17) |
| `SO_SNDLOWAT` | unsupported (18) | stored only (19) |
| `SO_LINGER` of 0 | modelled | modelled |
| `SO_LINGER` above 0 | stored only (20) | stored only (21) |
| `SO_RCVTIMEO` and `SO_SNDTIMEO` | stored only (22) | stored only (22) |
| `TCP_NODELAY` | stored only (23) | stored only (23) |
| `TCP_KEEPIDLE` | modelled | modelled |
| `TCP_KEEPINTVL` | modelled | modelled |
| `TCP_KEEPCNT` | modelled | modelled |
| `TCP_USER_TIMEOUT` | modelled | unsupported (24) |
| `TCP_MAXSEG` | stored only (25) | stored only (25) |
| `TCP_CONGESTION` | stored only (26) | unsupported (24) |
| `TCP_CORK` | stored only (27) | unsupported (24) |
| `TCP_LINGER2` | modelled | unsupported (24) |
| `TCP_DEFER_ACCEPT` | stored only (28) | unsupported (24) |
| `TCP_SYNCNT` | stored only (29) | unsupported (24) |
| `TCP_WINDOW_CLAMP` | stored only (30) | unsupported (24) |
| `TCP_QUICKACK` | stored only (31) | unsupported (24) |
| `TCP_FASTOPEN` | stored only (32) | unsupported (24) |
| `IP_FREEBIND` | stored only (33) | unsupported (24) |
| `IP_BIND_ADDRESS_NO_PORT` | stored only (34) | unsupported (24) |
| `IP_LOCAL_PORT_RANGE` | stored only (35) | unsupported (24) |
| `IP_TTL` | stored only (36) | stored only (36) |
| `IP_MTU` | modelled | unsupported (24) |
| `IP_MTU_DISCOVER` | stored only (37) | unsupported (24) |
| `IPV6_ONLY` | stored only (38) | stored only (38) |
| `SO_ACCEPTCONN` | modelled | unsupported (24) |
| `SO_ERROR` | approximated (39) | approximated (39) |
| `SO_TYPE` | modelled | modelled |

Notes:

1. A socket bound at port 0 takes a port of 49152-65535 in a fixed order, where
   Linux's default range is 32768-60999. Read the port a listener got with
   `Eio.Net.listening_addr`.
2. A socket bound at port 0 takes a port of 49152-65535 in a fixed order, and a
   UDP one the lowest free, where macOS picks its own. Read the port a listener
   got with `Eio.Net.listening_addr`.
3. `` `UdpV4 `` and `` `UdpV6 `` take their port as the socket is made, so a
   full range raises `EADDRINUSE` from `datagram_socket`, where the kernel takes
   it at the first send, and Linux fails that send with `EAGAIN`.
4. Only numeric addresses and `localhost` resolve, as on a host with no name
   server and no services database. Give tests addresses, not host names.
5. Unix paths live in a namespace of their own, not in a file system: a missing
   directory or a denied permission fails no bind. Test those on real sockets.
6. A connection queues 4 MiB over TCP and 256 KiB over AF_UNIX, at least the 2.6
   MiB and 250 KiB that Linux's default buffers hold, so a writer that fills it
   waits later than on Linux.
7. A connection queues 1 MiB over TCP, what macOS's buffers grow to from about
   330 KiB, and 8 KiB over AF_UNIX, as macOS holds.
8. A segment arrives at once, where macOS's loopback delivers it asynchronously.
   Test code that depends on it against real sockets.
9. A UDP datagram is counted by its length and a Unix-domain one as at least 128
   bytes, where macOS counts mbufs.
10. Turning `SO_DEBUG` on raises `EACCES`, as for a process without
    `CAP_NET_ADMIN`.
11. A send to a broadcast address is refused with `EACCES` whatever
    `SO_BROADCAST` says.
12. A bind looks at the `reuse_addr` argument of `listen` and `datagram_socket`
    only, not at `SO_REUSEADDR` among a connect's options.
13. Eio sends no urgent data, so `SO_OOBINLINE` acts on none.
14. `listen` and `datagram_socket` refuse `reuse_port` with `Invalid_argument`,
    where the kernel shares the address between sockets.
15. `SO_SNDBUF` and `SO_RCVBUF` read back as Linux keeps them, and a TCP
    connection's queue is within 15% of what Linux holds in writes of 1 KiB.
16. `SO_SNDBUF` and `SO_RCVBUF` read back as macOS keeps them, but are not
    rounded to whole segments when set before a connect or taken from a
    listener, and a TCP connection's queue fits macOS less closely than it fits
    Linux.
17. A waiting read wakes before `SO_RCVLOWAT` bytes are queued.
18. Linux refuses `SO_SNDLOWAT`, and so does the network, with `Invalid_option`.
19. A write does not wait for `SO_SNDLOWAT` bytes of room.
20. A close returns at once, where the kernel waits up to the `SO_LINGER` time
    for unsent bytes to go. A negative time reads back as set, where Linux's
    answer is not recorded.
21. A close returns at once, where the kernel waits up to the `SO_LINGER` time
    for unsent bytes to go.
22. No read or write times out, as under eio_posix, which waits for its
    non-blocking sockets in poll.
23. A write is delivered at once, as without Nagle's algorithm, whatever
    `TCP_NODELAY` says.
24. Under macOS, an option of Linux's is refused with `Invalid_option`, as
    eio_posix has no such option there.
25. `TCP_MAXSEG` reads back as the kernel works it out, but a write is not cut
    into segments.
26. `TCP_CONGESTION` takes `cubic` and `reno` only, with no effect on timing.
27. A write is delivered at once, whatever `TCP_CORK` says.
28. An accept does not wait for the client's first data under
    `TCP_DEFER_ACCEPT`.
29. No handshake is lost, and eio-net.chaos's lost one times out after 133
    seconds whatever `TCP_SYNCNT` says.
30. A connection's queue does not shrink to `TCP_WINDOW_CLAMP`.
31. No acknowledgement is delayed, with `TCP_QUICKACK` or without.
32. A connect carries no data under `TCP_FASTOPEN`.
33. A connect's `bind_to` still needs a loopback address with `IP_FREEBIND`.
34. A connect's `bind_to` at port 0 takes its port at once, whatever
    `IP_BIND_ADDRESS_NO_PORT` says.
35. Ports still come from 49152-65535 whatever `IP_LOCAL_PORT_RANGE` says.
36. No router lies on the loopback path for `IP_TTL` to count.
37. No datagram is fragmented on loopback, whatever `IP_MTU_DISCOVER` says.
38. Only a connect's options set `IPV6_ONLY` before a bind, and that bind does
    not look at it.
39. `SO_ERROR` reads 0, where the kernel gives an error not yet reported, such
    as a reset, and clears it.

<!-- End of the section made from lib/mem/scope.ml -->

## Related work

[Chaos engineering](https://principlesofchaos.org/) breaks a system on purpose
to find what fails, and `eio-net.chaos` does it inside a test.
[FoundationDB](https://apple.github.io/foundationdb/testing.html) runs its
whole cluster in a deterministic simulation that injects faults and replays a
failure exactly. [turmoil](https://github.com/tokio-rs/turmoil) does the same
for Rust's tokio, with a simulated network. [Jepsen](https://jepsen.io/) breaks
real clusters.

[Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/) (Bishop et al., JACM 2018)
specified TCP and the sockets API and checked the specification against
traces of real kernels. `eio-net.mem` borrows that idea on a smaller scale:
kernel traces recorded on Linux and macOS check its answers, and the
[socket cases](test/oracle/README.md) adapt [gVisor](https://gvisor.dev/)'s
[socket tests](https://github.com/google/gvisor/tree/master/test/syscalls/linux).

Tokio's
[LengthDelimitedCodec](https://docs.rs/tokio-util/latest/tokio_util/codec/length_delimited/struct.LengthDelimitedCodec.html)
does the same job in Rust. It also supports length fields at an offset or with
an adjustment. [Angstrom](https://github.com/inhabitedtype/angstrom) and
[Faraday](https://github.com/inhabitedtype/faraday) parse and serialise any
format in OCaml. `eio-net.framing` reads with Eio's own `Buf_read` and adds
only the bounded frame.

## Licence

ISC. See [LICENSE.md](LICENSE.md).
