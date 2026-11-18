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

let equal_sockaddr a b =
  match (a, b) with
  | `Tcp a, `Tcp b | `Udp a, `Udp b -> equal_port a b
  | `Unix a, `Unix b -> String.equal a b
  | (`Tcp _ | `Udp _ | `Unix _), (`Tcp _ | `Udp _ | `Unix _) -> false

let of_sockaddr = function
  | `Tcp (a, port) | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
