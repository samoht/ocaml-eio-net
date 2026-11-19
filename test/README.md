# Oracles

What the tests compare against, and where each came from.

| oracle | source | regenerate |
|--------|--------|------------|
| `interop/kernel/traces/*.csv`, read by `Eio_net_chaos.conformance` and `interop/kernel/` | Linux and macOS kernels, recorded through eio_posix, and Windows, through eio_windows on GitHub's `windows-latest` | `REGEN=1 dune build @test/interop/kernel/regen-traces`, on Windows (the `kernel-windows` workflow) then on macOS; see `interop/kernel/README.md` |
| `interop/connect-timeout/traces/connect.csv` | how long a lost handshake takes to fail on Linux and macOS, measured through eio_posix, eio_linux and `Unix` | `REGEN=1 dune build @test/interop/connect-timeout/regen-traces`; see `interop/connect-timeout/README.md` |
| `interop/silent-peer/traces/silent.csv` | how long a connection to a silent peer takes to fail on Linux and macOS, measured over loopback | `REGEN=1 dune build @test/interop/silent-peer/regen-traces`; see `interop/silent-peer/README.md` |
| `interop/cpython-struct/traces/vectors.csv` | CPython 3.15.0's `struct` module | `REGEN=1 dune build @test/interop/cpython-struct/regen-traces`; see `interop/cpython-struct/README.md` |
| `oracle/` | gVisor's socket tests, adapted, with the outcomes each kernel gives written in the code | nothing recorded; see `oracle/README.md` |
| `MODEL.md`, checked by `scope/` | the tables of `lib/mem/scope.ml` | `dune build @runtest --auto-promote` |
| `unix/`, `interop/kernel/host/` and the host side of `oracle/` | the kernel of the machine running the tests, live | nothing committed |
