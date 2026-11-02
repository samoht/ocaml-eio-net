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
  (ocaml (>= 4.14))
  fmt
  (wire (>= 1.3.0))
  (cstruct (>= 6.0.0))
  (alcotest :with-test)
  (alcobar :with-test)
  (ohex :with-test)
  (mdx :with-test)))

(package
 (name eio-net)
 (synopsis "Eio network backends and test helpers")
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
  ipaddr
  fpath
  (alcotest :with-test)
  (eio_main :with-test)))
