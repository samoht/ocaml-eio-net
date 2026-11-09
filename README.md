# eio-net

Network channels and test networks for
[Eio](https://github.com/ocaml-multicore/eio).

`eio-net` gives an Eio application a frame channel, where one receive is one
frame. The same channel runs over a datagram socket, a length-prefixed byte
stream or an in-memory pair, so a protocol can be tested without real sockets.
For those tests it also has an in-memory network and a wrapper that injects
faults on a schedule a test can replay.

## Libraries

- `eio-net`: the `Eio_net.Frame` and `Eio_net.Datagram` channels.
  `Eio_net.Relay` forwards connections and `Eio_net.Progress` ends a transfer
  that moves too slowly.
- `eio-net.framing`: bounded length-prefixed frames over `Eio.Buf_read`.
- `eio-net.accept`: an accept loop that holds at most a given number of
  connections.
- `eio-net.connect`: the answer a failed connect carries, refused or
  unreachable.
- `eio-net.addr`: conversions between `Ipaddr` and Eio addresses, and host
  name resolution.
- `eio-net.mem`: an in-memory `Eio.Net.t` with stream and datagram sockets.
  Writes never wait unless `?capacity` bounds the bytes a connection queues;
  a small capacity shows a peer that writes without reading.
- `eio-net.chaos`: fault injection for flows, datagram sockets and networks.
- `eio-net.unix`: batched datagram transfers, Unix-domain sockets, the host's
  interface addresses and the Unix classifiers of failed connects and accepts.

`Eio_net_accept.run` reads a failed accept with the classifier it is given as
`?classify`. Over a Unix network, pass `Eio_net_unix.accept_failure`: when the
process runs out of descriptors, the loop then waits for one to be free. With
the default classifier, that error ends the loop.

## Installation

<!-- $MDX skip -->
```sh
$ opam install eio-net
```

## Usage

`Eio_net_framing` writes each payload after its length, as an unsigned
big-endian integer. Here the length takes two bytes and payloads are at most
8 bytes long:

```ocaml
# let framing = Eio_net_framing.Config.v ~length_field:Uint16_be ~max_length:8 ();;
val framing : Eio_net_framing.Config.t = <abstr>
# let buffer = Buffer.create 16;;
val buffer : Buffer.t = <abstr>
# Eio_net_framing.write framing (Eio.Flow.buffer_sink buffer)
    [ [ Cstruct.of_string "abc" ]; [] ];;
- : unit = ()
# Buffer.contents buffer;;
- : string = "\000\003abc\000\000"
# let r = Eio.Buf_read.of_string (Buffer.contents buffer);;
val r : Eio.Buf_read.t = <abstr>
# Eio_net_framing.read framing r;;
- : string = "abc"
# Eio_net_framing.read framing r;;
- : string = ""
# Eio_net_framing.read framing r;;
Exception: End_of_file.
# Eio_net_framing.read framing (Eio.Buf_read.of_string "\000\003a");;
Exception: Eio_net_framing.Error: truncated payload: received 1 of 3 bytes
```

`write` sends the whole batch with one `Eio.Flow.write` and does not copy the
payloads. `Eio_net.Frame.of_length_prefixed_flow ~framing flow` turns a byte
stream into a frame channel with the same format.

## Bounds

A framing configuration accepts payloads up to 1 MiB by default, or 65535
bytes with a two-byte length. `read` raises
`Eio_net_framing.Error (Invalid_length _)` as soon as a prefix declares a
longer payload, before it reads any payload byte. A reader made with
`~max_size:(Eio_net_framing.Config.buffer_size framing)` therefore never holds
more than the longest frame.

A datagram longer than a channel's `max_frame` (64 KiB by default) raises the
`Eio.Io` error `Too_long` instead of arriving truncated. `Eio_net_accept.run`
holds at most `max_connections` connections. When it is full and another peer
connects, it drops the connection whose head (an HTTP request head, say) is
incomplete and has made the least progress.

## Related work

Tokio's
[LengthDelimitedCodec](https://docs.rs/tokio-util/latest/tokio_util/codec/length_delimited/struct.LengthDelimitedCodec.html)
does the same job in Rust, and also supports length fields at an offset or
with an adjustment. [Angstrom](https://github.com/inhabitedtype/angstrom) and
[Faraday](https://github.com/inhabitedtype/faraday) parse and serialise any
format in OCaml. `eio-net.framing` reads with Eio's own `Buf_read` and adds
only the bounded frame.

## API

The `.mli` files document each library. Those of `eio-net` are
[`frame.mli`](lib/frame.mli), [`datagram.mli`](lib/datagram.mli),
[`relay.mli`](lib/relay.mli) and [`progress.mli`](lib/progress.mli). Each
sub-library has its own directory under [`lib/`](lib/).

## Licence

ISC. See [LICENSE.md](LICENSE.md).
