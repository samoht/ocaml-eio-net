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
  "eio-net tests Eio code against a network, and gives programs frame
channels to build on. Eio_net_chaos.run runs a program again and again on an
in-memory network, each time with a different fault, and restarts it on what
the fault left. Eio_net_chaos.device checks that an Eio network answers as
Linux or macOS does; the in-memory network follows either kernel and passes
it. The frame channels run over datagram sockets and byte streams, in which
one receive is one frame, beside an accept loop and a relay.")
 (tags (eio test chaos network framing))
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.4))
  (eio (>= 1.7))
  (cstruct (>= 6.2.0))
  fmt
  logs
  (bytesrw (>= 0.2.0))
  (mtime (>= 2.0.0))
  (eio-chaos (and (>= 0.1.0) (< 0.2.0)))
  (ipaddr (>= 5.3.0))
  fpath
  (conf-linux-libc-dev (= :os linux))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)
  (ohex :with-test)
  (eio_main (and :with-test (>= 1.7)))
  (eio_posix (and :with-test (>= 1.7)))
  (mdx (and :with-test (>= 2.3.0)))))
