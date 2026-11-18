open! Hermetic

let ip a = Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets a)

(* An Eio address is 4 or 16 octets, so [of_octets_exn] does not raise. *)
let ipaddr (a : Eio.Net.Ipaddr.v4v6) = Ipaddr.of_octets_exn (a :> string)
let tcp a port = `Tcp (ip a, port)
let udp a port = `Udp (ip a, port)

let equal_port (host, port) (host', port') =
  Int.equal port port'
  && String.equal
       (host : Eio.Net.Ipaddr.v4v6 :> string)
       (host' : Eio.Net.Ipaddr.v4v6 :> string)

let equal_stream (a : Eio.Net.Sockaddr.stream) (b : Eio.Net.Sockaddr.stream) =
  match (a, b) with
  | `Tcp a, `Tcp b -> equal_port a b
  | `Unix a, `Unix b -> String.equal a b
  | `Tcp _, `Unix _ | `Unix _, `Tcp _ -> false

let equal_datagram (a : Eio.Net.Sockaddr.datagram)
    (b : Eio.Net.Sockaddr.datagram) =
  match (a, b) with
  | `Udp a, `Udp b -> equal_port a b
  | `Unix a, `Unix b -> String.equal a b
  | `Udp _, `Unix _ | `Unix _, `Udp _ -> false

let of_stream = function
  | `Tcp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None

let of_datagram = function
  | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
