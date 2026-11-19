## 0.1.0 (unreleased)

Initial release.

- `Eio_net_mem.v` is a network in memory, used through `Eio.Net`, with TCP,
  UDP and Unix-domain sockets, name lookups and socket options. It answers
  as Linux, macOS or, for the common traps, Windows does (`?os`), from
  answers recorded on each kernel.
- `Eio_net_chaos.run` runs a program under network faults (resets, short
  reads and writes, stalls, hangs, lost or reordered datagrams) and checks
  that it starts again on the network the fault left.
  `Eio_net_chaos.device` checks that an Eio network answers as the recorded
  kernels do.
- Frame channels (`Eio_net.Frame`, `Eio_net.Datagram`, `Eio_net.Framing`),
  with an accept loop (`Eio_net.Accept`) and a relay (`Eio_net.Relay`).
- `eio-net.unix` binds the POSIX calls Eio lacks and builds on POSIX hosts
  only; the rest of the package builds on Windows too.
