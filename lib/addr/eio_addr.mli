(** Eio socket addresses from {!Ipaddr} addresses.

    Eio names an IP address by its raw octets and a socket by a polymorphic
    variant; {!Ipaddr} names one by a parsed, comparable type. Every program
    that reads an address from a config file, a boot flag or a protocol message
    and then connects to it crosses that boundary. This is the crossing, in one
    place, both ways.

    Parsing the address itself is {!Ipaddr}'s own job: [Ipaddr.of_string], or
    [Ipaddr.with_port_of_string ~default] for the [HOST:PORT] a boot flag
    carries. *)

val ip : Ipaddr.t -> Eio.Net.Ipaddr.v4v6
(** [ip a] is [a] as an Eio address. *)

val ipaddr : Eio.Net.Ipaddr.v4v6 -> Ipaddr.t
(** [ipaddr a] is the Eio address [a] parsed back. Raises [Invalid_argument] if
    [a] is neither 4 nor 16 octets, which no address Eio hands out is. *)

val tcp : Ipaddr.t -> int -> Eio.Net.Sockaddr.stream
(** [tcp a port] is the stream address of [port] on [a]. *)

val udp : Ipaddr.t -> int -> Eio.Net.Sockaddr.datagram
(** [udp a port] is the datagram address of [port] on [a]. *)

val of_stream : Eio.Net.Sockaddr.stream -> (Ipaddr.t * int) option
(** [of_stream addr] is the address and port [addr] names, or [None] for a Unix
    socket, which names neither. *)

val of_datagram : Eio.Net.Sockaddr.datagram -> (Ipaddr.t * int) option
(** [of_datagram addr] is the address and port [addr] names, or [None] for a
    Unix socket. *)
