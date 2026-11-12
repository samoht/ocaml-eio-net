# eio-net

Network channels and test networks for
[Eio](https://github.com/ocaml-multicore/eio).

Over a real network, a read may return a few bytes of a message or several
messages at once. A peer may stall, reset the connection or end the stream
midway. A connect may be refused or slow, and a datagram lost, duplicated or
reordered. Loopback sockets in a test rarely do any of this, so the bugs that
depend on it stay hidden. `eio-net` gives an Eio program an in-memory network,
and `Eio_net_chaos.check`, which runs a workload on it many times, each time
with a different fault chosen from what the workload does.

It also gives a frame channel, where one receive is one frame, over a datagram
socket, a length-prefixed byte stream or an in-memory pair, and an accept loop
that bounds the connections it holds.

## Chaos tests

Here a client asks a server for a line, with a timeout and three attempts.
The server bounds how long it waits for the request, as a client may stall
for good. The workload checks what the client got, and how long it took:

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
    match (got, Eio_net_chaos.outage env) with
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
# let faults f =
    match f () with
    | () -> "passed"
    | exception Failure m -> List.nth (String.split_on_char '\n' m) 1;;
val faults : (unit -> unit) -> string = <fun>
# faults (fun () -> Eio_net_chaos.check (workload ~timeout:false));;
- : string = "  faults:   connect of 1c at 0 times out"
```

The failure also gives the tier that found it, the operations of the failing
run, and a `~replay:"..."` argument that runs that scenario alone.

The tests follow a few rules:

- At most two faults reach a run. A program that tries three times may be
  asked to succeed, as here. A broker that fans a message out to three
  subscribers may demand that one of them gets everything. A lasting
  condition, such as one-byte reads, counts as one fault but reaches every
  connection.
- Every program on the network meets the faults, the test's own peers
  included. Test servers use `Eio.Net.run_server ~on_error:ignore` and a
  timeout on each read, test clients catch `Eio.Io _ | End_of_file`, and both
  parse strictly. Two fibers of `Eio.Fiber.both` that both meet a fault fail
  together with `Eio.Exn.Multiple`, so catch the errors inside each fiber.
- A connect that times out takes 133 seconds, as on Linux, and then fails
  with the error eio_posix raises, which Eio does not classify as a
  connection failure. A program with no connect timeout still succeeds, so
  the test asserts its latency to catch it.
- Ports are held as on Linux: the end of a connection that closes first
  holds its port for 60 seconds, so a server that listens again on its port
  within a run passes `~reuse_addr:true`. A client that binds no address
  holds its port only toward its destination, and takes a port in TIME_WAIT
  toward it again after a second, so short connections to a few servers do
  not run out of ports.
- In the outage run every connect is refused and every datagram lost.
  `Eio_net_chaos.outage env` is then `true`, and the test checks what its
  program does when it gives up.

Each run has its own event loop, whose clock moves only when every fiber
waits, so a timeout of an hour takes no time. A check blocks the event loop it
is called from until it ends. It is deterministic, reads no environment
variable and prints nothing. For a scenario a check does not try,
`Eio_net_chaos.wrap ~clock f net` asks `f` about each operation, and
`Eio_net_chaos.pair` gives a fuzzer two connected ends.

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
- `eio-net.mem`: an in-memory `Eio.Net.t` with stream and datagram sockets.
  A connection queues what Linux's default buffers hold, and a datagram
  socket what its receive buffer holds; a small `?capacity` shows a peer that
  writes without reading.
  It follows Linux by default, and macOS where the two kernels differ with
  `` ~os:`Macos ``, which `Eio_net_chaos.check` takes too.
- `eio-net.chaos`: `check`, which runs a workload under the network faults
  it may meet, and the wrapper it uses.
- `eio-net.unix`: batched datagram transfers, Unix-domain sockets, the host's
  interface addresses and the Unix classifiers of failed connects and accepts.

## Installation

<!-- $MDX skip -->
```sh
$ opam install eio-net
```

## Framing

`Eio_net_framing` writes each payload after its length, as an unsigned
big-endian integer. Here the length takes two bytes and payloads are at most
8 bytes long:

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
payloads. `Eio_net.Frame.of_length_prefixed_flow ~framing flow` turns a byte
stream into a frame channel with the same format.

## Bounds

A framing configuration accepts payloads up to 1 MiB by default, or 65535
bytes with a two-byte length. `read` raises
`Eio_net_framing.Error (Invalid_length _)` as soon as a prefix declares a
longer payload, before it reads any payload byte. A reader made with
`~max_size:(Eio_net_framing.Config.buffer_size framing)` therefore never holds
more than the longest frame.

A datagram longer than a channel's `max_frame` (64 KiB by default) raises the
`Eio.Io` error `Too_long` instead of arriving truncated.

## Accept loop

`Eio_net_accept.run` holds at most `max_connections` connections. A
connection's head is what its peer sends before it is served. When the loop is
full and another peer connects, it drops the connection whose head is
incomplete and has made the least progress. A protocol whose head is read
through a layer of its own, as a TLS handshake reads its records off the
socket, wraps the socket with `Eio_net_accept.reading_flow` so that the bytes
the layer reads count as progress. An exception one connection raises ends
that connection alone and goes to `?on_error`.

`Eio_net_accept.run` reads a failed accept with the classifier it is given as
`?classify`. Over a Unix network, pass `Eio_net_unix.accept_failure`: when the
process runs out of descriptors, the loop then waits for one to be free. With
the default classifier, that error ends the loop.

## Limitations

`eio-net.mem` models one host:

- It has a loopback interface only, and follows one kernel at a time: Linux
  by default, macOS with `` ~os:`Macos ``. Run a check under each `?os` your
  program targets.
- Socket options other than `TCP_NODELAY` and `TCP_QUICKACK` raise
  `Invalid_argument`, and so does `getsockopt`. Use `?capacity` where a test
  would set `SO_SNDBUF`, and a real socket to test an option.
- Unix paths live in a namespace of their own, not in the file system: a
  missing directory or a denied permission fails no bind. Test those on real
  sockets.
- Host resolution knows numeric addresses and `localhost` only. Give tests
  addresses, not host names.
- A socket bound at port 0 takes a port of 49152-65535 in a fixed order,
  where Linux's default range is 32768-60999 and kernels pick UDP ports at
  random. Read the port a listener got with `Eio.Net.listening_addr`.
- An unbound UDP socket takes its port when it is created, so a full port
  range raises `EADDRINUSE` there, not at its first send. Handle the error
  where the socket is made.
- Under `` `Macos ``, a loopback segment arrives at once, where macOS delivers
  it asynchronously, and what a Unix-domain datagram receiver holds is an
  approximation measured on one Mac. Test code that depends on either on real
  sockets.

`eio-net.chaos` has limits of its own:

- A garbled datagram is random bytes, so a parser that rejects most noise but
  accepts some may pass. Fuzz the parser on its own.
- After an AF_UNIX crash, the crashed end's code still runs and reads what was
  queued to it. Check what the other end sees.
- A peer that stops is a read that hangs until that peer's own timeout ends
  it, so a program blocked on it passes once the peer gives up and closes.
  Assert the program's latency to catch it.
- A datagram is delayed alone, and for 50 ms. Ask for longer or grouped
  delays with `Eio_net_chaos.wrap`.
- `EMFILE` on accept is never tried, as `Eio.Net.run_server` stops on it. Ask
  for `Eio_net_chaos.Fault.Emfile` with `Eio_net_chaos.wrap`.
- A run of more than 10,000,000 turns or 100,000 network operations fails as
  too large, and so does a check whose linear tier needs more runs than its
  budget. Shrink the workload, or raise `~budget` for the linear tier.
- `Eio_net_chaos.wrap` follows the kernel `?os` names, Linux by default, not
  the host's. Over a real network, pass the host's kernel.

## Related work

Tokio's
[LengthDelimitedCodec](https://docs.rs/tokio-util/latest/tokio_util/codec/length_delimited/struct.LengthDelimitedCodec.html)
does the same job in Rust, and also supports length fields at an offset or
with an adjustment. [Angstrom](https://github.com/inhabitedtype/angstrom) and
[Faraday](https://github.com/inhabitedtype/faraday) parse and serialise any
format in OCaml. `eio-net.framing` reads with Eio's own `Buf_read` and adds
only the bounded frame.

## API

The `.mli` files document each library. Those of `eio-net` are
[`frame.mli`](lib/frame.mli), [`datagram.mli`](lib/datagram.mli),
[`relay.mli`](lib/relay.mli) and [`progress.mli`](lib/progress.mli). Each
sub-library has its own directory under [`lib/`](lib/).

## Licence

ISC. See [LICENSE.md](LICENSE.md).
