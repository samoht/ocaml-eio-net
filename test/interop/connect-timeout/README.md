# Connect timeouts

`traces/connect.csv` holds how long a TCP connect whose SYNs are all dropped
takes to fail on Linux and macOS, and with what error, as eio_posix,
eio_linux and a plain `Unix.connect` see it: one row per connect, timed on
the monotonic clock.

[`scripts/generate.sh`](scripts/generate.sh) measures them on a Mac, running
[`scripts/measure.ml`](scripts/measure.ml) for ten connects per client,
started 0.7 s apart: a kernel fires a long timer at the end of a granule of
its timer wheel, so how late a connect fails depends on when it started.

- macOS natively, connecting to 127.0.0.2: `lo0` holds 127.0.0.1 only, so the
  SYNs loop back to no address and are dropped. No root and no firewall rule.
- Linux in Docker, in the `ocaml/opam` image the [kernel traces](../kernel)
  pin, with the kernel's default settings, connecting to a listener on
  127.0.0.1 whose accept queue is full, so that the kernel drops each SYN to
  it. No capability and no package beyond the image. The container runs
  without Docker's seccomp profile, which refuses io_uring to eio_linux.

To regenerate, on macOS with Docker, outside any sandbox that keeps it from
the Docker socket or from connecting to 127.0.0.2:

```sh
REGEN=1 dune build @test/interop/connect-timeout/regen-traces
```

[`traces/provenance`](traces/provenance) says where the trace comes from:
the revision, date, user and host, the macOS version, the Linux kernel of
Docker's VM and its TCP settings (from the kernel traces'
[`probe.sh`](../kernel/scripts/probe.sh)), and the MD5 of the trace and of
`generate.sh` and `measure.ml`. [`test/provenance`](../../provenance) fails
when one of them differs from what the file records.
