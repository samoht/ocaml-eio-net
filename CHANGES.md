## 0.1.0

Initial release.

### eio-net

- `Eio_net.Frame`: a channel where one receive is one frame, over a datagram
  socket, a length-prefixed byte stream or an in-memory pair.
- `Eio_net.Datagram`: a datagram channel whose receives also give the source
  address.
- `Eio_net.Relay` forwards stream connections between two addresses, and
  `Eio_net.Progress` ends a transfer that moves too few bytes a second.

### eio-net.framing

- The length-prefix codec of a frame channel, read with `Eio.Buf_read` and
  written without copying the payloads.
- A two- or four-byte length; a prefix that declares a payload past the bound
  raises before any payload byte is read.

### eio-net.accept

- `Eio_net_accept.run`: an accept loop that holds at most a given number of
  connections and, when full, drops the one whose head is incomplete and has
  made the least progress.
- Loops that share a pool shed each other's connections when the process runs
  out of descriptors.

### eio-net.connect and eio-net.addr

- `Eio_net_connect.classify` tells a refused connect from an unreachable one.
- `Eio_net_addr` converts between `Ipaddr` and Eio addresses, compares them,
  and resolves host names.

### eio-net.mem

- An in-memory `Eio.Net.t` with TCP, UDP and Unix-domain sockets, for tests
  that need no real socket.
- It follows Linux by default and macOS with `` ~os:`Macos ``: errors, port
  allocation, TIME_WAIT on a given clock, queue sizes and datagram limits.
  Kernel traces recorded on both systems, and tests adapted from gVisor's, check
  it against real sockets.
- `?capacity` sets what each direction of a connection queues, to show a peer
  that writes without reading.
- `setsockopt`, `getsockopt` and the options of a connect take every option of
  `Eio.Net.Sockopt` and `Eio_unix.Net`, and answer as the chosen kernel does,
  defaults and refusals included. `SO_SNDBUF` and `SO_RCVBUF` size the
  queues, and `SO_LINGER` at 0 resets the peer on close.
- On a given clock, keepalive, `TCP_USER_TIMEOUT` and the retransmission
  timeout end a connection gone silent, at the times measured on each kernel.

### eio-net.chaos

- `Eio_net_chaos.check` runs a workload many times on `eio-net.mem`, each run
  with faults chosen from what the workload did on its first run: reads cut
  short or coalesced, a reset, a closed peer, a connection gone silent for
  good, small socket buffers, refused, timed-out or unreachable connects, and lost, duplicated,
  reordered or garbled datagrams.
- Each run has its own event loop and a virtual clock, so a check is
  deterministic and an hour-long timeout takes no time. A run that hangs,
  spins or grows too large fails.
- A failure names its scenario and its operations, and `~replay` runs that
  scenario alone. `?os` picks the kernel the network follows, as in
  `eio-net.mem`.
- `Eio_net_chaos.wrap` and `Eio_net_chaos.pair` inject the faults a function
  chooses, from one domain or several.

### eio-net.unix

- `Eio_net_unix.Frame`: frame channels over Unix datagram descriptors, with
  batched system calls.
- `Eio_net_unix.Local`: Unix-domain stream sockets reached through a directory
  capability, with paths of any length, and the descriptors a message
  carries.
- `Eio_net_unix.Interface`: the host's interface addresses and their changes.
- `Eio_net_unix.Stream` offloads a write to a systhread, and
  `Eio_net_unix.connect_failure` and `Eio_net_unix.accept_failure` classify
  the Unix errors of failed connects and accepts.
