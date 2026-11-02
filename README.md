# eio-net

Network channels, relays and test backends for
[Eio](https://github.com/ocaml-multicore/eio).

## Overview

A protocol implementation should not care whether its packets travel over a
UDP socket, a unikernel's network interface or a test harness, and its tests
should not need real sockets. `eio-net` provides the small abstractions that
make this possible:

- `Eio_net.Frame`: a bidirectional packet channel where one receive is one
  frame. CCSDS transfer frames, AX.25 and Ethernet codecs read and write over
  it. An outgoing frame is a list of buffers, as `Eio.Flow.write` takes. Over
  a length-prefixed flow or an Eio datagram socket the buffers go to `writev`
  as they are, so a header and a payload held in separate buffers reach the
  kernel without being copied into one, and a burst can go out in one system
  call. The in-memory pair copies each frame, and so does the `eio-net.unix`
  channel, into its batch buffers.
- `Eio_net.Datagram`: an addressed datagram channel that reports each
  datagram's source, for a UDP server that answers whoever wrote to it.
- `Eio_net.Relay`: copies bytes between two flows in both directions, or
  listens and forwards each accepted connection to another address.
- `Eio_net.Progress`: bounds a transfer by its progress, as curl's low-speed
  check does: a window of reads and writes that moves fewer than a stated
  number of bytes a second ends it, and a stalled peer costs at most two
  windows.

Seven sub-libraries sit beside it:

- `eio-net.framing`: bounded length-prefixed frames over Eio's buffered
  reader, which `Eio_net.Frame.of_length_prefixed_flow` uses. See
  [Length-prefixed frames](#length-prefixed-frames).
- `eio-net.mem`: an in-memory `Eio.Net.t` for stream and datagram sockets,
  and connected in-memory pipes, so client and server code can talk through
  the normal `Eio.Net` functions in a hermetic test.
- `eio-net.chaos`: wrappers around a flow, a datagram socket or a network that
  inject faults (short reads and writes, stalls, resets, dropped, duplicated
  and reordered datagrams, refused connections) on a seeded or scripted
  schedule that a test can print and replay.
- `eio-net.unix`: a frame channel over a connected Unix datagram descriptor,
  batching sends and receives with `sendmsg_x` and `recvmsg_x` on Darwin, and a
  vectored write that runs off the OCaml runtime lock. The runtime lock is
  the per-domain lock a thread must hold to run OCaml code; a C call that
  releases it lets the domain's other threads run while it blocks in the
  kernel. `Interface` lists the host's interface addresses with their
  prefixes (getifaddrs) and waits for the kernel to report a change to them.
  `Local` connects to a Unix-domain socket at a path of any length and
  listens at one with the permission bits it is given, and `Tcp.listen`
  reports a failed bind as an `Eio.Io` error. Linking it registers the Unix
  classifiers of a failed connect and a failed accept.
- `eio-net.accept`: the accept loop a listener serves its connections with,
  `Eio_net_accept.run`. It holds at most a declared number of connections
  and, under pressure, sheds the one whose head (an HTTP request head, an SSH
  handshake) is incomplete and has made the least progress; an accept its
  peer aborted, or one with no descriptor left, leaves it serving. With no
  descriptor left it sheds among the connections of every loop of the
  process, since descriptors are the process's. A connection that fails,
  whatever its peer did, is reported and ends alone. It reads
  what a failed accept carries with the classifier its platform's network
  registers, so it works the same over every network.
- `eio-net.connect`: what a failed connect carries from the network, a
  refused or unreachable peer, read by the classifier its platform's network
  registers. A connection library asks it and works the same over every
  network.
- `eio-net.addr`: `Eio_net_addr`, conversions between `Ipaddr` and Eio
  socket addresses, and host name resolution to a stream address.

## Length-prefixed frames

`Eio_net_framing` reads and writes frames made of an unsigned network-order
length followed by that many payload bytes. The length excludes its own
prefix. `Uint16_be` is the LENGTH field of
[RFC 4571 section 2](https://www.rfc-editor.org/rfc/rfc4571.html#section-2),
zero-length frame included, and `Uint32_be` is the default. What a payload
means belongs to the protocol above.

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
```

`read` takes one frame from an `Eio.Buf_read.t`, which reads its flow until
the frame is whole. The end of input at a frame boundary is `End_of_file`;
inside a frame it is the error `Truncated`, with the part (prefix or payload)
and the bytes received of it.

```ocaml
# Eio_net_framing.read framing (Eio.Buf_read.of_string "\000\003a");;
Exception: Eio_net_framing.Error: truncated payload: received 1 of 3 bytes
```

`encode` lays out a batch as one list of buffers: the prefixes of the batch
share one new buffer, and the payload buffers follow them uncopied, so
`write` hands the batch to a single `Eio.Flow.write`, one `writev` on a
socket. It validates the whole batch first: a payload of the wrong length
refuses the batch and nothing is written.

### Bounds

The default maximum payload is 1 MiB, lowered to the capacity of the length
field and to the platform's string limit; an explicit maximum beyond either is
refused. A prefix that declares a length outside the configured range is
refused as `Invalid_length`, with the exact unsigned length, as soon as the
prefix is whole and before any payload byte is requested. A reader made with
`~max_size:(Config.buffer_size framing)` (the maximum payload plus the prefix)
therefore never holds more than that, whatever a peer declares.

Reading a frame reads its prefix in place, without allocating, and copies its
payload once, from the reader's buffer into the payload string.

### Over a stream

`Eio_net.Frame.of_length_prefixed_flow ~framing flow` is a frame channel over
a byte stream. Its receives, and its sends, are serialised, so a batch reaches
the wire whole when fibers send concurrently. A receive that raises
`Eio_net_framing.Error`, or that an exception interrupts inside a frame, makes
every later receive raise the same error (`Aborted` for the exception) rather
than read the middle of a frame as a prefix. A send that raises partway makes
every later send raise `Eio_net_framing.Error Aborted`, since part of a frame
may be on the wire; a batch refused for its lengths writes nothing and leaves
the channel usable.

### Related work

- [Angstrom](https://github.com/inhabitedtype/angstrom) offers general
  incremental parser combinators, and
  [Faraday](https://github.com/inhabitedtype/faraday) serialisers with explicit
  control over copying. Eio's `Buf_read` plays the first role here, and this
  library adds only the bounded frame.
- Tokio's [LengthDelimitedCodec](https://docs.rs/tokio-util/latest/tokio_util/codec/length_delimited/struct.LengthDelimitedCodec.html)
  similarly separates frame bounds from stream IO. Its configurable field
  offsets and adjustments are outside these two network-order representations.
- Committed CPython `struct` vectors in `test/interop/cpython-struct`
  independently check byte order, field boundaries and unsigned declarations;
  the tests replay them without Python.

## Installation

<!-- $MDX skip -->
```sh
$ opam install eio-net
```

## API

The interfaces are documented in the `.mli` files under `lib/`: `frame.mli`,
`datagram.mli` and `relay.mli` for the main library, and one directory per
sub-library.

## Licence

ISC. See [LICENSE.md](LICENSE.md).
