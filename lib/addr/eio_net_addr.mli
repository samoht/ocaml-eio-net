(** Eio socket addresses from {!Ipaddr} addresses, both ways.

    Parsing is {!Ipaddr}'s own job: [Ipaddr.of_string], or
    [Ipaddr.with_port_of_string ~default] for a [HOST:PORT]. Resolving a name is
    Eio's: {!Eio.Net.getaddrinfo_stream}, which takes a literal address too. *)

val ip : Ipaddr.t -> Eio.Net.Ipaddr.v4v6
(** [ip a] is [a] as an Eio address. *)

val ipaddr : Eio.Net.Ipaddr.v4v6 -> Ipaddr.t
(** [ipaddr a] is [a] as an {!Ipaddr.t}: [ip (ipaddr a)] is [a]. *)

val tcp : Ipaddr.t -> int -> Eio.Net.Sockaddr.stream
(** [tcp a port] is the stream address of [port] on [a]. *)

val udp : Ipaddr.t -> int -> Eio.Net.Sockaddr.datagram
(** [udp a port] is the datagram address of [port] on [a]. *)

val equal_sockaddr : ([< Eio.Net.Sockaddr.t ] as 'a) -> 'a -> bool
(** [equal_sockaddr a b] is [true] iff [a] and [b] are the same socket address:
    of the same kind, with equal IP address and port, or equal Unix socket path.
    It takes stream and datagram addresses alike. A receiver that locks onto the
    source of a datagram compares later sources with it. *)

val of_sockaddr : [< Eio.Net.Sockaddr.t ] -> (Ipaddr.t * int) option
(** [of_sockaddr addr] is the address and port [addr] names, or [None] for a
    Unix socket, which names neither. It takes stream and datagram addresses
    alike. *)
