(** Whether the host's loopback addresses take a socket.

    A host or a container may run without IPv6, and a sandbox may refuse IP
    sockets altogether: opam's macOS build sandbox fails every IPv4 [bind(2)]
    with [EPERM]. Tests of the host's own sockets skip where these are [false].
*)

val ipv4 : unit -> bool
(** [ipv4 ()] is [true] if a socket binds 127.0.0.1. *)

val ipv6 : unit -> bool
(** [ipv6 ()] is [true] if a socket binds ::1. *)
