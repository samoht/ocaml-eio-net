# net-framing

Bounded length-prefixed frames without an IO runtime.

`Framing` owns network-order length encoding, incremental prefix/payload state,
EOF classification and the lifecycle of one outstanding write. `eio-net`
interprets its read/write instructions. Both use the same limits and errors.

## Installation

<!-- $MDX skip -->
```sh
$ opam install net-framing
```

## Usage

The length excludes its own prefix. `Uint16_be` uses the unsigned LENGTH
representation described by [RFC 4571 section 2](https://www.rfc-editor.org/rfc/rfc4571.html#section-2),
including its zero-length null frame. `Uint32_be` is the default. Payload
semantics belong to the protocol above framing.

```ocaml
# let framing = Framing.Config.v ~length_field:Uint16_be ~max_length:8 ();;
val framing : Framing.Config.t = <abstr>
# Framing.encode framing (Rope.of_string "abc") |> Result.map Rope.to_string;;
- : (string, Framing.error) result = Ok "\000\003abc"
# let decoder = Framing.Decoder.v framing;;
val decoder : Framing.Decoder.t = <abstr>
# let decoder = Framing.Decoder.handle decoder (Data "\000\003");;
val decoder : Framing.Decoder.t = <abstr>
# let decoder, event = Framing.Decoder.next_event decoder;;
val decoder : Framing.Decoder.t = <abstr>
val event : Framing.Decoder.event = Framing.Decoder.Read 3
# let decoder = Framing.Decoder.handle decoder (Data "abc");;
val decoder : Framing.Decoder.t = <abstr>
# let decoder, event = Framing.Decoder.next_event decoder;;
val decoder : Framing.Decoder.t = <abstr>
val event : Framing.Decoder.event = Framing.Decoder.Frame "abc"
# let decoder = Framing.Decoder.handle decoder (Eof "");;
val decoder : Framing.Decoder.t = <abstr>
# snd (Framing.Decoder.next_event decoder);;
- : Framing.Decoder.event = Framing.Decoder.End_of_stream
```

An interpreter supplies at most the requested bytes. EOF includes any final
partial read. A clean frame-boundary EOF differs from a truncated prefix or
payload; failed input cannot resume a terminal decoder. Every read request is
positive, so empty input cannot create a zero-byte progress loop.

`encode_batch` validates every payload before returning a batch. A rejected
batch leaves `Encoder` usable. Once physical output starts, report `Sent` or
`Failed` exactly once; failure permanently closes that encoder because a
partial frame may already be on the wire.

## Bounds

The default maximum payload is 1 MiB, reduced to the selected field's capacity
and the platform string limit. Explicit unrepresentable limits are refused.
Each decoder state retains at most `Config.buffer_size` bytes (maximum payload
plus prefix). An oversized declaration is refused after its two or four prefix
bytes, before requesting or retaining any payload.

Incremental input uses the existing balanced rope: retained space is O(n) and
arbitrarily fragmented input costs O(n log n) work for n payload bytes. The Eio
interpreter obtains each requested prefix/payload through its bounded buffered
reader, whose buffer is independently bounded by `Config.buffer_size`. Thus
the complete Eio input path retains at most twice that byte bound plus fixed
state metadata; kernel socket buffers are owned by the transport. Outgoing
batches reference payload leaves, adding length prefixes and
rope metadata; the caller owns the supplied batch size and delivered frames.

## API

The documented interface is `Framing`: `Config`, `encode`, `encode_batch`,
`Decoder` and `Encoder`. `Eio_net.Frame.of_length_prefixed_flow ~framing` uses
that configuration; clean EOF raises `End_of_file`, while framing refusals
raise `Eio_net.Frame.Error` with the pure error value.

## Related work

- The existing Wire unsigned codecs encode/decode the length fields here.
- [Angstrom](https://github.com/inhabitedtype/angstrom) offers general incremental
  parser combinators, while [Faraday](https://github.com/inhabitedtype/faraday)
  offers serializers with explicit copying/allocation control. This package
  provides the bounded frame lifecycle using the project's existing codecs.
- Tokio's [LengthDelimitedCodec](https://docs.rs/tokio-util/latest/tokio_util/codec/length_delimited/struct.LengthDelimitedCodec.html)
  similarly separates frame bounds from stream IO. Its configurable field
  offsets/adjustments are outside these two network-order representations.
- Committed CPython `struct` vectors independently check byte order, field
  boundaries and unsigned declarations; normal tests replay them without Python.

## Licence

ISC. See [LICENSE.md](LICENSE.md).
