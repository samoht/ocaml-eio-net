(** Cases adapted from gVisor's socket syscall tests. Each test's name starts
    with the gVisor file and test it adapts. *)

val suite : string * unit Alcotest.test_case list
(** Socket pairs of every type: socket_generic_test_cases.cc, socket_stream*.cc
    and socket_non_stream*.cc. *)

val unix_suite : string * unit Alcotest.test_case list
(** Unix-domain sockets: socket_unix_*.cc. *)

val udp_suite : string * unit Alcotest.test_case list
(** UDP sockets: udp_socket.cc. *)

val tcp_suite : string * unit Alcotest.test_case list
(** TCP sockets: tcp_socket.cc and socket_inet_loopback.cc. *)
