# eio-net

Network channels, relays and test backends for
[Eio](https://github.com/ocaml-multicore/eio).

The companion pure package [net-framing](Framing.md) owns bounded length-prefix
encoding, incremental reads, EOF and write-failure state. The Eio frame adapter
interprets that core; its `~framing` argument accepts `Framing.Config.t`.

## Overview

A protocol implementation should not care whether its packets travel over a
UDP socket, a unikernel's network interface or a test harness, and its tests
should not need real sockets. `eio-net` provides the small abstractions that
make this possible:

- `Eio_net.Frame`: a bidirectional packet channel where one receive is one
  frame. CCSDS transfer frames, AX.25 and Ethernet codecs read and write over
  it. Outgoing frames are ropes (`Rope.t`), a sequence of byte slices read
  as one byte string. `writev` takes an array of (pointer, length) pairs and
  sends them in one system call, so a header and a payload held in separate
  buffers reach the kernel without being copied into one string, and a burst
  can go out in one system call.
- `Eio_net.Datagram`: an addressed datagram channel that reports each
  datagram's source, for a UDP server that answers whoever wrote to it.
- `Eio_net.Relay`: copies bytes between two flows in both directions, or
  listens and forwards each accepted connection to another address.

Six sub-libraries sit beside it:

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
  kernel. It also lists the host's interface addresses with their prefixes
  (getifaddrs) and waits for the kernel to report a change to them. Linking
  it registers the Unix classifiers of a failed connect and a failed accept.
- `eio-net.accept`: the accept loop a listener serves its connections with,
  `Eio_net_accept.run`. It holds at most a declared number of connections
  and, under pressure, sheds the one whose head (an HTTP request head, an SSH
  handshake) is incomplete and has made the least progress; an accept its
  peer aborted, or one with no descriptor left, leaves it serving. With no
  descriptor left it sheds among the connections of every loop of the
  process, since descriptors are the process's. It reads
  what a failed accept carries with the classifier its platform's network
  registers, so it works the same over every network.
- `eio-net.connect`: what a failed connect carries from the network, a
  refused or unreachable peer, read by the classifier its platform's network
  registers. A connection library asks it and works the same over every
  network.
- `eio-net.addr`: conversions between `Ipaddr` and Eio socket addresses, and
  host name resolution to a stream address.

## Installation

<!-- $MDX skip -->
```sh
$ opam install eio-net
```

If opam cannot find the package, it may not yet be released in the public
`opam-repository`. Add the overlay repository, then install it:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install eio-net
```

## API

The interfaces are documented in the `.mli` files under `lib/`: `frame.mli`,
`datagram.mli` and `relay.mli` for the main library, `framing.mli` for the pure
framing core, and one directory per
sub-library.

## Licence

ISC. See [LICENSE.md](LICENSE.md).
