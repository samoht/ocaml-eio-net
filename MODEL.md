# What eio-net.mem models of each kernel

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
| TIME_WAIT and FIN_WAIT2 | modelled | approximated (10) |
| A silent connection | modelled | modelled |

| Option | Linux | macOS |
| --- | --- | --- |
| `SO_DEBUG` | stored only (11) | stored only |
| `SO_BROADCAST` | stored only (12) | stored only |
| The `reuse_addr` argument | modelled | modelled |
| `SO_REUSEADDR` | stored only (13) | stored only (13) |
| `SO_KEEPALIVE` | modelled | modelled |
| `SO_DONTROUTE` | stored only | stored only |
| `SO_OOBINLINE` | stored only (14) | stored only (14) |
| The `reuse_port` argument | unsupported (15) | unsupported (15) |
| `SO_REUSEPORT` | stored only | stored only |
| `SO_SNDBUF` and `SO_RCVBUF` | approximated (16) | approximated (17) |
| `SO_RCVLOWAT` | stored only (18) | stored only (18) |
| `SO_SNDLOWAT` | unsupported (19) | stored only (20) |
| `SO_LINGER` of 0 | modelled | modelled |
| `SO_LINGER` above 0 | stored only (21) | stored only (22) |
| `SO_RCVTIMEO` and `SO_SNDTIMEO` | stored only (23) | stored only (23) |
| `TCP_NODELAY` | stored only (24) | stored only (24) |
| `TCP_KEEPIDLE` | modelled | modelled |
| `TCP_KEEPINTVL` | modelled | modelled |
| `TCP_KEEPCNT` | modelled | modelled |
| `TCP_USER_TIMEOUT` | modelled | unsupported (25) |
| `TCP_MAXSEG` | stored only (26) | stored only (26) |
| `TCP_CONGESTION` | stored only (27) | unsupported (25) |
| `TCP_CORK` | stored only (28) | unsupported (25) |
| `TCP_LINGER2` | modelled | unsupported (25) |
| `TCP_DEFER_ACCEPT` | stored only (29) | unsupported (25) |
| `TCP_SYNCNT` | stored only (30) | unsupported (25) |
| `TCP_WINDOW_CLAMP` | stored only (31) | unsupported (25) |
| `TCP_QUICKACK` | stored only (32) | unsupported (25) |
| `TCP_FASTOPEN` | stored only (33) | unsupported (25) |
| `IP_FREEBIND` | stored only (34) | unsupported (25) |
| `IP_BIND_ADDRESS_NO_PORT` | stored only (35) | unsupported (25) |
| `IP_LOCAL_PORT_RANGE` | stored only (36) | unsupported (25) |
| `IP_TTL` | stored only (37) | stored only (37) |
| `IP_MTU` | modelled | unsupported (25) |
| `IP_MTU_DISCOVER` | stored only (38) | unsupported (25) |
| `IPV6_ONLY` | stored only (39) | stored only (39) |
| `SO_ACCEPTCONN` | modelled | unsupported (25) |
| `SO_ERROR` | approximated (40) | approximated (40) |
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
10. The end that closes second frees its port at once, where macOS holds it
    until the peer's last ACK arrives, a few milliseconds over loopback.
11. Turning `SO_DEBUG` on raises `EACCES`, as for a process without
    `CAP_NET_ADMIN`.
12. A send to a broadcast address is refused with `EACCES` whatever
    `SO_BROADCAST` says.
13. A bind looks at the `reuse_addr` argument of `listen` and `datagram_socket`
    only, not at `SO_REUSEADDR` among a connect's options.
14. Eio sends no urgent data, so `SO_OOBINLINE` acts on none.
15. `listen` and `datagram_socket` refuse `reuse_port` with `Invalid_argument`,
    where the kernel shares the address between sockets.
16. `SO_SNDBUF` and `SO_RCVBUF` read back as Linux keeps them, and a TCP
    connection's queue is within 15% of what Linux holds in writes of 1 KiB.
17. `SO_SNDBUF` and `SO_RCVBUF` read back as macOS keeps them, but are not
    rounded to whole segments when set before a connect or taken from a
    listener, and a TCP connection's queue fits macOS less closely than it fits
    Linux.
18. A waiting read wakes before `SO_RCVLOWAT` bytes are queued.
19. Linux refuses `SO_SNDLOWAT`, and so does the network, with `Invalid_option`.
20. A write does not wait for `SO_SNDLOWAT` bytes of room.
21. A close returns at once, where the kernel waits up to the `SO_LINGER` time
    for unsent bytes to go. A negative time reads back as set, where Linux's
    answer is not recorded.
22. A close returns at once, where the kernel waits up to the `SO_LINGER` time
    for unsent bytes to go.
23. No read or write times out, as under eio_posix, which waits for its
    non-blocking sockets in poll.
24. A write is delivered at once, as without Nagle's algorithm, whatever
    `TCP_NODELAY` says.
25. Under macOS, an option of Linux's is refused with `Invalid_option`, as
    eio_posix has no such option there.
26. `TCP_MAXSEG` reads back as the kernel works it out, but a write is not cut
    into segments.
27. `TCP_CONGESTION` takes `cubic` and `reno` only, with no effect on timing.
28. A write is delivered at once, whatever `TCP_CORK` says.
29. An accept does not wait for the client's first data under
    `TCP_DEFER_ACCEPT`.
30. No handshake is lost, and eio-net.chaos's lost one times out after 133
    seconds whatever `TCP_SYNCNT` says.
31. A connection's queue does not shrink to `TCP_WINDOW_CLAMP`.
32. No acknowledgement is delayed, with `TCP_QUICKACK` or without.
33. A connect carries no data under `TCP_FASTOPEN`.
34. A connect's `bind_to` still needs a loopback address with `IP_FREEBIND`.
35. A connect's `bind_to` at port 0 takes its port at once, whatever
    `IP_BIND_ADDRESS_NO_PORT` says.
36. Ports still come from 49152-65535 whatever `IP_LOCAL_PORT_RANGE` says.
37. No router lies on the loopback path for `IP_TTL` to count.
38. No datagram is fragmented on loopback, whatever `IP_MTU_DISCOVER` says.
39. Only a connect's options set `IPV6_ONLY` before a bind, and that bind does
    not look at it.
40. `SO_ERROR` reads 0, where the kernel gives an error not yet reported, such
    as a reset, and clears it.

<!-- End of the section made from lib/mem/scope.ml -->

## Windows

Under `` `Windows `` eio-net.mem follows Linux, as the tables above give it,
but where the kernel traces record Windows otherwise through eio_windows on
GitHub's `windows-latest`, and where eio_windows breaks Eio's contract it
follows the contract. `Eio_net_mem`'s Windows section lists each change.

Modelled:

- `SO_REUSEADDR` port takeover: two listeners with `reuse_addr` share a port,
  and a connect reaches the first; one with `reuse_addr` beside one without is
  denied with `Eio.Fs.Permission_denied` (`WSAEACCES`); a wildcard address and
  a specific one share a port, IPv4 and IPv6 apart as `IPV6_V6ONLY` is on.
- `SO_REUSEPORT` is refused with `Invalid_option`.
- No TIME_WAIT: a closed socket's address is free at once.
- Resets: each read and write after a reset raises it; a read after a write to
  a closed peer raises one (`WSAECONNABORTED`); bytes reaching a shut receive
  side, or a receive side shut with bytes unread, reset the connection.
- A read after its end's receive shutdown raises `ESHUTDOWN`.
- No Unix-domain datagram sockets (`EAFNOSUPPORT`); a Unix-domain connect to a
  path no socket binds is refused; a Unix-domain end takes one write after its
  peer closed.
- Datagrams: a send with no destination raises `ENOTCONN`; one to a port no
  socket holds makes the sender's next receive raise a reset; of sockets that
  share an address, the first bound receives.
- Resolution: stream addresses, then datagram ones, `::1` first; a host is a
  dotted quad or an IPv6 address, in brackets or not.

Not modelled, and left out of `Eio_net_chaos.conformance`'s Windows replay:

- `SO_EXCLUSIVEADDRUSE`, which neither Eio nor OCaml's `Unix` reaches.
- The socket options' defaults and values, which follow Linux.
- Which addresses a connect or a datagram reaches across address families,
  the edge addresses, the receive memory, and the names `getnameinfo` gives.
- Sends off loopback and port exhaustion: the runner has other interfaces,
  and its ephemeral range cannot be narrowed below 255 ports.
- A connect's timeout, 21 seconds as Windows documents its two SYN
  retransmissions, which no trace records.
