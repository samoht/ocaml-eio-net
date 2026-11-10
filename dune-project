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
 (synopsis "Eio network backends and test helpers")
 (description
  "\| Frame channels for Eio over datagram sockets, length-prefixed streams
  "\| and in-memory pairs. An in-memory network and a wrapper that injects
  "\| replayable faults test protocols without real sockets. POSIX adapters
  "\| add batched datagrams, Unix-domain sockets and interface addresses.
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
  (alcotest :with-test)
  (alcobar :with-test)
  (ohex :with-test)
  (eio_main :with-test)
  (eio_posix :with-test)
  (mdx (and :with-test (>= 2.3.0)))))
