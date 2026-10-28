(lang dune 3.21)
(using mdx 0.4)

(name eio-net)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (tangled gazagnaire.org/ocaml-eio-net))

(package
 (name net-framing)
 (synopsis "Pure bounded length-prefixed framing")
 (description "IO-free network-order framing with bounded incremental decoding and atomic batch validation.")
 (tags (org:blacksun network-programming area:10003039))
 (depends
  (ocaml (>= 4.14))
  fmt
  wire
  nox-rope
  (alcotest :with-test)
  (alcobar :with-test)
  (nox-csv :with-test)
  (nox-hex :with-test)
  (mdx :with-test)))

(package
 (name eio-net)
 (synopsis "Eio network backends and test helpers")
 (tags (org:blacksun network-programming eio test hermetic:eio-boundary area:10003039))
 (depends
  hermetic
  net-framing
  (ocaml (>= 5.1))
  (eio (>= 1.5))
  cstruct
  fmt
  logs
  bytesrw
  mtime
  ipaddr
  fpath
  nox-rope
  (alcotest :with-test)
  (eio_main :with-test)))
