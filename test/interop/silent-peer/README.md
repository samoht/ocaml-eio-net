# Silent peers

`traces/silent.csv` holds how long a TCP connection over loopback takes to
fail once its peer has gone silent and a byte it sent goes unacknowledged,
on Linux and macOS, and how its socket answers then: one row per connection,
timed on the monotonic clock from the client's first write after the
silence. The test checks that `eio-net.mem`, on Eio's mock clock, fails a
silent connection within the times measured on its kernel, with the same
outcomes for the call that blocked and for a read, a write and a read after
it.

[`scripts/generate.sh`](scripts/generate.sh) measures them on a Mac, running
[`scripts/measure.ml`](scripts/measure.ml) for five connections per case,
started 0.7 s apart. Each connection exchanges a byte each way, then its
accepted end drops every segment it sends, as
[`scripts/silence_stubs.c`](scripts/silence_stubs.c) arranges, so that its
client hears nothing more. The client then writes a byte and reads
(`write-read`), or writes until a write blocks (`fill`).

- macOS natively: the accepted end sends through `stf0`, an interface that
  is down, so nothing it sends leaves it. No root and no firewall rule.
- Linux in Docker, in the `ocaml/opam` image the [kernel traces](../kernel)
  pin, with the kernel's default settings and no network: the accepted end
  sends through a loose source route by 192.0.2.1, so that the client, which
  accepts no source route, drops what it sends. Setting the route needs
  `CAP_NET_RAW`, which Docker grants to the container's root. A socket
  filter on the accepted end would do without it, but this kernel refuses
  one on a TCP socket.

To regenerate, on macOS with Docker, outside any sandbox that keeps it from
the Docker socket:

```sh
REGEN=1 dune build @test/interop/silent-peer/regen-traces
```

Linux gives up after about 16 minutes, so the run takes that long.

[`traces/provenance`](traces/provenance) says where the trace comes from:
the revision, date, user and host, the macOS version, the Linux kernel of
Docker's VM and its TCP settings (from the kernel traces'
[`probe.sh`](../kernel/scripts/probe.sh)), and the MD5 of the trace and of
`generate.sh`, `measure.ml` and `silence_stubs.c`.
[`test/provenance`](../../provenance) fails when one of them differs from
what the file records.
