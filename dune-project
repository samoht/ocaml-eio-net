(lang dune 3.21)
(using mdx 0.4)

(name eio-net)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github samoht/ocaml-eio-net))

(package
 (name net-framing)
 (synopsis "Pure bounded length-prefixed framing")
 (description "IO-free network-order framing with bounded incremental decoding and atomic batch validation.")
 (tags (network framing))
 (depends
  (ocaml (>= 5.3))
  fmt
  (wire (>= 1.3.0))
  (cstruct (>= 6.0.0))
  (alcotest :with-test)
  (alcobar :with-test)
  (ohex :with-test)
  (mdx (and :with-test (>= 2.3.0)))))

(package
 (name eio-net)
 (synopsis "Eio network backends and test helpers")
 (description
  "Network channels, relays and test backends for Eio: a frame channel where one
   receive is one frame, over a datagram socket, a length-prefixed flow or an
   in-memory pair; an addressed datagram channel; a bidirectional relay; an
   in-memory network and a fault-injecting wrapper for tests; and batched
   datagram transfers on POSIX sockets.")
 (tags (eio network test))
 (depends
  (hermetic (>= 0.1.0))
  (net-framing (= :version))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
  (cstruct (>= 6.0.0))
  fmt
  logs
  bytesrw
  mtime
  (ipaddr (>= 5.3.0))
  fpath
  (alcotest :with-test)
  (eio_main :with-test)))
