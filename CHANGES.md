## 0.1.0

Initial release.

- `eio-net`: frame channels over datagram sockets, length-prefixed streams and
  in-memory pairs (`Eio_net.Frame`, `Eio_net.Datagram`); `Eio_net.Relay`
  forwards connections, and `Eio_net.Progress` ends a transfer that moves too
  slowly.
- `eio-net.framing`: the length-prefix codec, with a bound on frame length,
  read with `Eio.Buf_read`.
- `eio-net.accept`: an accept loop that holds at most a given number of
  connections and drops the one that has made the least progress.
- `eio-net.connect`: tells a refused connect from an unreachable one.
- `eio-net.addr`: conversions between `Ipaddr` and Eio addresses, and host
  name resolution.
- `eio-net.mem`: an in-memory `Eio.Net.t` with TCP, UDP and Unix-domain
  sockets that follow what Linux does, checked against real sockets on Linux
  and macOS by an oracle adapted from gVisor's socket tests.
- `eio-net.chaos`: replayable fault schedules for flows, datagram sockets and
  networks: short reads and writes, stalls, resets, refused or slow connects,
  lost, duplicated or reordered datagrams.
- `eio-net.unix`: batched datagram transfers, Unix-domain sockets with long
  paths, the host's interface addresses, and the Unix classifiers of failed
  connects and accepts.
