# Kernel traces

`traces/*.csv` hold what Linux and macOS answer to every operation sequence
and scenario of [`Eio_net_kernel`](../../../lib/kernel/eio_net_kernel.mli), recorded through
`eio_posix`: one row per sequence, a column per kernel. The test replays each
row on `eio-net.mem` and compares.

[`scripts/generate.sh`](scripts/generate.sh) records them on a Mac: macOS
natively, and Linux in Docker, in the `ocaml/opam` image it pins by digest,
with IPv6 enabled on loopback and, for the scenarios that run out of ports,
`net.ipv4.ip_local_port_range` narrowed to `40000 40019`. To regenerate, on
macOS with Docker, outside any sandbox that keeps it from the network or the
Docker socket:

```sh
REGEN=1 dune build @test/interop/kernel/regen-traces
```

## Provenance

[`traces/provenance`](traces/provenance) says where the traces come from: the
revision, date, user and host, the macOS version, the Linux kernel of Docker's
VM, the settings the traces depend on (from
[`scripts/probe.sh`](scripts/probe.sh)), and the MD5 of each trace and of
`generate.sh` and `record.ml`. Regenerating rewrites it.

[`test/provenance`](../../provenance) fails when a trace or a pinned input
differs from what the file records. `Eio_net_kernel` is not pinned: the replay
also runs it, and it maps `eio-net.mem`'s exceptions, so most of its edits
change no recording. The replay test checks instead that the traces hold exactly
its sequences and scenarios, so an added, removed or renamed one asks for a
re-recording; an edit to how one runs does not.

## Numbers the traces do not hold

`eio-net.mem` and `eio-net.chaos` also use kernel timings that no trace here
records:

- A connect that times out fails after 133 s on Linux and after 75 s on
  macOS ([`lib/chaos/wrapper.ml`](../../../lib/chaos/wrapper.ml)). These
  traces back only the six SYN retries: `TCP_SYNCNT` reads 6 on a new socket
  of the recorder's Linux. The [connect-timeout traces](../connect-timeout)
  record the timeout itself, on both kernels: Linux's `ip-sysctl.rst`, in
  6.12 and 7.2, gives 131 s for six retries after four linear timeouts, and
  its connects fail after 133.0 to 136.5 s.
- A connection whose bytes go unacknowledged fails after 948.6 s on Linux
  (`tcp_retries2` = 15) and 18 s on macOS. The
  [silent-peer traces](../silent-peer) record it, on both kernels: Linux's
  connections fail after 948.3 to 951.7 s, and macOS's after 17.95 to
  18.06 s.
