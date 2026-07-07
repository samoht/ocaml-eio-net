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
 (tags (org:blacksun network eio test))
 (depends
  (ocaml (>= 5.1))
  eio
  cstruct
  (alcotest :with-test)
  (eio_main :with-test)))
