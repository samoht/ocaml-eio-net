# Kernel traces

`traces/*.csv` hold what Linux, macOS and Windows answer to every operation
sequence and scenario of
[`Eio_net_kernel`](../../../lib/kernel/eio_net_kernel.mli), recorded through
`eio_posix` on Linux and macOS and through `eio_windows` on Windows: one row
per sequence, a column per kernel.
`eio-net.chaos` compiles them in, with
[`lib/chaos/gen`](../../../lib/chaos/gen/gen.ml), and `Eio_net_chaos.device`
replays them on any network. The test runs it on `eio-net.mem`, and
[`host`](host) on the host's own network:

```sh
dune build @test/interop/kernel/host/host-device
```

## Regenerate

Two hosts record the traces, each its own columns, and keep the others from
the traces before: `record merge` writes a row per sequence or scenario, and
an answer no host recorded reads `not recorded`. Each column is found by its
name: `linux`, `macos` and `windows eio_windows`.

On macOS, with Docker, outside any sandbox that keeps it from the network or
the Docker socket:

```sh
REGEN=1 dune build @test/interop/kernel/regen-traces
```

[`scripts/generate.sh`](scripts/generate.sh) records macOS natively, and
Linux in Docker, in the `ocaml/opam` image it pins by digest, with Eio 1.7
from the opam-repository commit it pins, IPv6 enabled on loopback and, for
the scenarios that run out of ports, `net.ipv4.ip_local_port_range` narrowed
to `40000 40019`.

On Windows the same command runs
[`scripts/generate-windows.sh`](scripts/generate-windows.sh), which records
through eio_windows. The workflow
[`kernel-windows.yml`](../../../.github/workflows/kernel-windows.yml) runs it
on GitHub's `windows-latest` with Eio 1.7 and uploads `traces/` as the
artifact `kernel-windows`, to copy into the tree before the Mac's turn. The
runner has other interfaces and its ephemeral range cannot be narrowed below
255 ports, so the scenarios that send off loopback or run out of ports read
`not recorded` there. eio_windows never completes a refused TCP connect, so
a TCP operation answers `timeout` after 5 s and ends its sequence.

A change to the sequences, the scenarios or the recorder means recording on
both hosts: Windows first, then macOS.

## Provenance

[`traces/provenance`](traces/provenance) says where the traces come from: the
revision, date, user and host of the last recording, the macOS version, the
Linux kernel of Docker's VM and the settings they depend on (from
[`scripts/probe.sh`](scripts/probe.sh)), the Windows version, runner image,
TCP settings and eio_windows's version (from
[`scripts/probe-windows.sh`](scripts/probe-windows.sh)), and the MD5 of each
trace and of the scripts that recorded it. A recording keeps the other
host's lines, its revision, date and pins as `posix.*` or `windows.*`
([`scripts/carry.sh`](scripts/carry.sh)).

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
