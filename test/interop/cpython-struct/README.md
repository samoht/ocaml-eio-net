# CPython framing vectors

Committed vectors compare `eio-net.framing` with CPython 3.15.0's public
[`struct`](https://docs.python.org/3.15/library/struct.html) API: `!H` and `!I`
encode unsigned 16-bit and 32-bit network-order lengths. The generator calls
that API; it does not contain a second length encoder.

Normal tests replay `traces/vectors.csv` without Python. To regenerate with
CPython 3.15.0 on `PATH` as `python3.15`, run from the project checkout:

```sh
REGEN=1 dune build @test/interop/cpython-struct/regen-traces
```

The generator creates a build-local venv, uses only the standard library, and
refuses another interpreter/version. `traces/oracle.json` records the oracle.
Vectors cover empty frames, field boundaries, uint16 overflow and uint32 values
whose sign bit is set. Oversized declarations must be refused before payload
bytes are requested. This checks the length representation, independently of
the payload protocol (including the uint16 LENGTH used by
[RFC 4571 section 2](https://www.rfc-editor.org/rfc/rfc4571.html#section-2)).
