## 0.1.0

Initial release.

- `eio-net.framing`, bounded length-prefixed frames over Eio's buffered
  reader: a uint16 or uint32 network-order length, a declared length refused
  before any payload byte is requested, and a batch validated whole and
  written with one vectored write, its payloads not copied.
- `Eio_net.Frame`, a packet channel where one receive is one frame, over a
  datagram socket, a length-prefixed flow or an in-memory pair. A
  length-prefixed flow refuses further reads after a failure inside a frame,
  and further writes after a failed write. An outgoing frame is a list of
  buffers, which a length-prefixed flow and an Eio datagram socket write with
  `writev` without copying them into one; the in-memory pair and
  `eio-net.unix` copy each frame.
- `Eio_net.Datagram`, an addressed datagram channel, and `Eio_net.Relay`,
  which copies bytes between two flows, or forwards a listener's
  connections in an accept loop handed the caller's classifier and pool.
- `Eio_net.Progress`, a transfer bounded by its progress, as curl's
  low-speed check bounds one.
- `eio-net.accept`, the accept loop a listener serves its connections with,
  and `eio-net.connect`, the answer a failed connect carries from the
  network. A failed accept or connect is read by the classifier handed to
  the code that accepts or connects: by default the one that reads Eio's
  portable errors, and over a Unix network `Eio_net_unix.accept_failure` or
  `connect_failure`. `accept_failure` reads a network error Linux's accept(2)
  passes on from the new connection as an aborted accept, and an error that
  can persist on the listener, EOPNOTSUPP or EPERM, as none.
- `eio-net.mem` and `eio-net.chaos`, an in-memory network and a wrapper that
  injects faults, for tests; `eio-net.unix`, batched sends and receives on
  POSIX sockets, a vectored write off the runtime lock, the host's interface
  addresses and their changes, Unix-domain sockets at paths of any length,
  and a TCP listen that reports a failed bind as `Eio.Io`; `eio-net.addr`,
  `Eio_net_addr`, conversions between `Ipaddr` and Eio addresses.
