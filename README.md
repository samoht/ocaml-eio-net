# eio-net

Small Eio network backends and test helpers.

Lightweight network backends for Eio-based code, including an in-memory backend
used to drive network protocols in tests without a real socket.

## Install

<!-- $MDX skip -->
```sh
opam install eio-net
```

If opam cannot find the package, it may not yet be released in the public
`opam-repository`. Add the overlay repository, then install it:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install eio-net
```

## API

See the `.mli` files under `lib/` for the documented interface.

## License

ISC. See [LICENSE.md](LICENSE.md).
