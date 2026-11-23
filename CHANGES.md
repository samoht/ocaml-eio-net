## 0.2.0 (23/11/2026)

### Breaking

- `Eio_net_unix.Local.listen ~replace:true` replaces only a socket nobody listens on. A socket a live listener serves is kept and the call fails with `EADDRINUSE`: stop that listener before listening on its path (#12)

### Changed

- `Eio_net_unix.Local.listen` holds a lock on `path.lock`, a file it creates beside the socket and leaves in place. A second listener on the same path fails with `EADDRINUSE` while the first lives (#12)

### Added

- `Eio_net_unix.Local.connect` takes `?wait`: with `~wait:false`, a connect to a full backlog on Linux raises `EAGAIN` at once instead of waiting for room (#8)
- `Eio_net.Socket.classify` tells why the system refused a new socket (no descriptor, no memory, unsupported or denied), from the raw `Unix.Unix_error` eio_posix raises or from an Eio error (#11)
- `Eio_net.Progress.pp_too_slow` formats the `min_rate` and `window` of a `Too_slow` as curl words its low-speed error (#10)
- `Eio_net_chaos.seeded` draws the read and write faults of one end of a connection from a fuzzer's seed, and `by_side` gives the client and the server end a fault function each (#5)
- `Eio_net_chaos.datagram_socket` injects receive faults into a datagram socket the caller made, and `seeded_receives` draws them from a seed (#7)
- `Eio_net_chaos.Listener` tests a listener against its peers. `failing` fails its accepts on cue, `stall` is a peer that connects and then stalls, and `accepted` counts the connections silent peers make it hold (#6)
- `Eio_net_chaos.Fault.Cancel n` cancels a connection's read or write `n` turns of the event loop in. The test fails if a byte reaches the buffer after the cancel, or if a write the cancel stopped sends any (#16)

## 0.1.0 (21/11/2026)

Initial release.
