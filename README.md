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
  it. Outgoing frames are ropes (`Rope.t`), a sequence of byte slices read
  as one byte string. `writev` takes an array of (pointer, length) pairs and
  sends them in one system call, so a header and a payload held in separate
  buffers reach the kernel without being copied into one string, and a burst
  can go out in one system call.
- `Eio_net.Datagram`: an addressed datagram channel that reports each
  datagram's source, for a UDP server that answers whoever wrote to it.
- `Eio_net.Relay`: copies bytes between two flows in both directions, or
  listens and forwards each accepted connection to another address.

Four sub-libraries sit beside it:

- `eio-net.mem`: an in-memory `Eio.Net.t` for stream sockets, and connected
  in-memory pipes, so client and server code can talk through the normal
  `Eio.Net` functions in a hermetic test.
- `eio-net.chaos`: wrappers around a flow, a datagram socket or a network that
  inject faults (short reads and writes, stalls, resets, dropped, duplicated
  and reordered datagrams, refused connections) on a seeded or scripted
  schedule that a test can print and replay.
- `eio-net.unix`: a frame channel over a connected Unix datagram descriptor,
  batching sends and receives with `sendmsg_x` and `recvmsg_x` on Darwin, and a
  vectored write that runs off the OCaml runtime lock. The runtime lock is
  the per-domain lock a thread must hold to run OCaml code; a C call that
  releases it lets the domain's other threads run while it blocks in the
  kernel.
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
`datagram.mli` and `relay.mli` for the main library, and one directory per
sub-library.

## Licence

ISC. See [LICENSE.md](LICENSE.md).
