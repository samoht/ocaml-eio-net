(lang dune 3.21)

(name eio-net)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (tangled gazagnaire.org/ocaml-eio-net))

(package
 (name eio-net)
 (synopsis "Eio network backends and test helpers")
 (tags (org:blacksun network-programming eio test hermetic:eio-boundary area:10003039))
 (depends
  hermetic
  (ocaml (>= 5.1))
  (eio (>= 1.5))
  cstruct
  fmt
  logs
  bytesrw
  ipaddr
  nox-rope
  (alcotest :with-test)
  (eio_main :with-test)))
