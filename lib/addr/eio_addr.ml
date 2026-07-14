(* Eio names an IP address by its raw octets; Ipaddr names it by a parsed type.
   Both directions, once. *)

let ip a = Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets a)

let ipaddr (a : Eio.Net.Ipaddr.v4v6) =
  match Ipaddr.of_octets (a :> string) with
  | Ok a -> a
  | Error (`Msg msg) -> invalid_arg ("Eio_addr.ipaddr: " ^ msg)

let tcp a port = `Tcp (ip a, port)
let udp a port = `Udp (ip a, port)

let of_stream = function
  | `Tcp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None

let of_datagram = function
  | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
