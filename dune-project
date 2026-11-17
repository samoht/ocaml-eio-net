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
 (name eio-net)
 (synopsis "Network tests and channels for Eio")
 (description
  "\| Answers two questions, with one call each: does an Eio program cope
  "\| with a network that fails, and does an Eio network answer as Linux or
  "\| macOS does? Both run on an in-memory network that follows either
  "\| kernel.
  )
 (tags (eio network framing test))
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.4))
  (eio (>= 1.7))
  (cstruct (>= 6.0.0))
  fmt
  logs
  (bytesrw (>= 0.2.0))
  (mtime (>= 2.0.0))
  (psq (>= 0.2.0))
  (domain-local-await (>= 0.1.0))
  (ipaddr (>= 5.3.0))
  fpath
  (conf-linux-libc-dev (= :os linux))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)
  (ohex :with-test)
  (eio_main (and :with-test (>= 1.7)))
  (eio_posix (and :with-test (>= 1.7)))
  (mdx (and :with-test (>= 2.3.0)))))
