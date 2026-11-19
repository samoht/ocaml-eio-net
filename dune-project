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
 (synopsis "In-memory network for Eio, to test code under network faults, and frame channels")
 (tags (eio test chaos network in-memory framing))
 (description
  "eio-net is an in-memory network for Eio, to test code under network
faults, and frame channels to build programs on. Each test gets a fresh
network of its own, used through the ordinary Eio.Net API, so the code under
test does not change. Eio_net_chaos.run runs a program again and again, each
time with a different network fault, and restarts it on the network the fault
left. Eio_net_chaos.device checks that an Eio network answers as Linux,
macOS or Windows does. The frame channels, in which one receive is one
frame, come with an accept loop and a relay.")
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
  (conf-linux-libc-dev (= :os linux))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)
  (ohex :with-test)
  (eio_main (and :with-test (>= 1.7)))
  (eio_posix (and :with-test (>= 1.7) (<> :os win32)))
  (mdx (and :with-test (>= 2.3.0)))))
