## Unreleased

- `Eio_net_chaos.seeded` and `by_side`: fault functions for the two ends of a connection, drawn from a fuzzer's seed (#5)
- `Eio_net_chaos.Listener`: a listener whose accepts fail on cue, peers that connect and stall, and a count of the connections they make it hold (#6)
- `Eio_net_chaos.datagram_socket` wraps a datagram socket the caller made, and `seeded_receives` draws its receive faults from a seed (#7)
- `Local.connect ~wait:false` raises `EAGAIN` at once when the backlog is full, instead of waiting for room (#8)
- `Progress.pp_too_slow` formats a `Too_slow` in curl's words (#10)
- `Socket.classify` tells why the system refused a new socket, from the raw `Unix.Unix_error` eio_posix raises or a wrapped one (#11)
- `Local.listen` claims its path with a lock file beside it, and with `~replace` replaces only a socket nobody listens on: a live listener no longer loses its name (#12)

## 0.1.0 (21/11/2026)

Initial release.
