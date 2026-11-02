(** Eio socket addresses from {!Ipaddr} addresses, both ways.

    Parsing is {!Ipaddr}'s own job: [Ipaddr.of_string], or
    [Ipaddr.with_port_of_string ~default] for a [HOST:PORT]. *)

val ip : Ipaddr.t -> Eio.Net.Ipaddr.v4v6
(** [ip a] is [a] as an Eio address. *)

val ipaddr : Eio.Net.Ipaddr.v4v6 -> Ipaddr.t
(** [ipaddr a] is the Eio address [a] parsed back. Raises [Invalid_argument] if
    [a] is neither 4 nor 16 octets, which no address Eio hands out is. *)

val tcp : Ipaddr.t -> int -> Eio.Net.Sockaddr.stream
(** [tcp a port] is the stream address of [port] on [a]. *)

val udp : Ipaddr.t -> int -> Eio.Net.Sockaddr.datagram
(** [udp a port] is the datagram address of [port] on [a]. *)

val stream :
  _ Eio.Net.t ->
  host:string ->
  port:int ->
  (Eio.Net.Sockaddr.stream, [ `Msg of string ]) result
(** [stream net ~host ~port] is the stream address of [port] on [host], where
    [host] is either a literal IP address or a name [net] resolves. A name with
    several addresses gives the first the resolver returns. This is
    [Error (`Msg m)] if [host] is neither a literal address nor a name that
    resolves, where [m] names [host] and carries the resolver's {!Eio.Io} error,
    if any. A cancellation during the lookup is raised, not returned. *)

val of_stream : Eio.Net.Sockaddr.stream -> (Ipaddr.t * int) option
(** [of_stream addr] is the address and port [addr] names, or [None] for a Unix
    socket, which names neither. *)

val of_datagram : Eio.Net.Sockaddr.datagram -> (Ipaddr.t * int) option
(** [of_datagram addr] is the address and port [addr] names, or [None] for a
    Unix socket. *)
