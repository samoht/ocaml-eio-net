# Socket cases

Each case runs once on `eio-net.mem` and once on the host's real sockets, and
compares both with the outcome it expects on Linux and on macOS. Nothing is
recorded: the expected outcomes are in the code, and every run on Linux or
macOS checks them against that kernel. To check them again, run on each:

```sh
dune build @test/oracle/runtest
```

## gVisor cases

[`test_gvisor.ml`](test_gvisor.ml) adapts tests from
[gVisor](https://github.com/google/gvisor)'s
[`test/syscalls/linux`](https://github.com/google/gvisor/tree/6782e66bb04ba41de7da417da53406c2525ac9f1/test/syscalls/linux)
(Apache License 2.0). Each case keeps the file and test name it comes from, as
`~file` and `~test`. The cases were written in ba4756f (2026-11-09) without
noting the gVisor revision. The thirteen files they name are the same at every
gVisor commit from 9533bcc (2026-11-04) to 89cacad (2026-11-15); at 6782e66,
the last before ba4756f, each of the 52 file and test pairs names a test of
that file.

What a case changes from its gVisor test:

- It runs through Eio, so its outcome is what an Eio program sees: Eio's
  error, or none where Eio hides the kernel's (a shutdown's `ENOTCONN`).
- Its pair is two sockets over loopback or a Unix path, one of the kinds of
  pair gVisor runs a test on; steps Eio cannot express are left out.
- It adds macOS's outcome, and `eio-net.mem`'s where that differs; the case's
  comment says why.

## Review cases

[`test_review.ml`](test_review.ml) holds cases a review of `eio-net.mem`
raised, measured on Linux and macOS when they were written. No kernel version
is recorded; one comment names Linux 7.2 (76af812, 2026-11-10).
