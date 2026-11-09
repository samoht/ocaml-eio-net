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
  "Network channels, relays and test backends for Eio: a frame channel where one
   receive is one frame, over a datagram socket, a length-prefixed flow or an
   in-memory pair; bounded length-prefixed framing over Eio's buffered reader;
   an addressed datagram channel; a bidirectional relay; a transfer bounded by
   its progress, as curl's low-speed check bounds one; an accept loop that
   bounds the connections it holds and sheds the least progressed under
   pressure; the answers a failed connect or accept carries; conversions
   between Ipaddr and Eio addresses; an in-memory network and a
   fault-injecting wrapper for tests; and, on POSIX systems, batched datagram
   transfers, a vectored write off the runtime lock, the host's interface
   addresses and their changes, Unix-domain sockets at paths of any length,
   and a TCP listen that reports a failed bind as an Eio error.")
 (tags (eio network framing test))
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
  (cstruct (>= 6.0.0))
  fmt
  logs
  bytesrw
  mtime
  (ipaddr (>= 5.3.0))
  fpath
  (conf-linux-libc-dev (= :os linux))
  (alcotest :with-test)
  (alcobar :with-test)
  (ohex :with-test)
  (eio_main :with-test)
  (eio_posix :with-test)
  (mdx (and :with-test (>= 2.3.0)))))
