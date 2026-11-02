## 0.1.0

Initial release of `eio-net` and `net-framing`.

- `net-framing`, bounded length-prefixed framing with no IO: a uint16 or
  uint32 network-order length, an incremental decoder that asks for exactly
  the bytes it needs, and an encoder that validates a whole batch before
  emitting any of it.
- `Eio_net.Frame`, a packet channel where one receive is one frame, over a
  datagram socket, a length-prefixed flow or an in-memory pair. An outgoing
  frame is a list of buffers, written with `writev` without being copied into
  one.
- `Eio_net.Datagram`, an addressed datagram channel, and `Eio_net.Relay`,
  which copies bytes between two flows.
- `Eio_net.Progress`, a transfer bounded by its progress, as curl's
  low-speed check bounds one.
- `eio-net.accept`, the accept loop a listener serves its connections with,
  and `eio-net.connect`, the answer a failed connect carries from the
  network.
- `eio-net.mem` and `eio-net.chaos`, an in-memory network and a wrapper that
  injects faults, for tests; `eio-net.unix`, batched sends and receives on
  POSIX sockets; `eio-net.addr`, conversions between `Ipaddr` and Eio
  addresses.
