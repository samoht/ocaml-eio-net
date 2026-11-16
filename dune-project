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
 (synopsis "Network channels and chaos tests for Eio")
 (description
  "\| Tests protocols without real sockets: an in-memory network following
  "\| Linux or macOS, and a check that runs a workload on it under network
  "\| faults.
  )
 (tags (eio network framing test))
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
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
  (eio_main :with-test)
  (eio_posix :with-test)
  (mdx (and :with-test (>= 2.3.0)))))
