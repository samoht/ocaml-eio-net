open! Hermetic

let ip a = Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets a)

let ipaddr (a : Eio.Net.Ipaddr.v4v6) =
  match Ipaddr.of_octets (a :> string) with
  | Ok a -> a
  | Error (`Msg msg) -> invalid_arg ("Eio_net_addr.ipaddr: " ^ msg)

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

let stream net ~host ~port =
  match Ipaddr.of_string host with
  | Ok ip -> Ok [ tcp ip port ]
  | Error _ -> (
      match
        Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
      with
      | [] -> Fmt.error_msg "cannot resolve %S: no address" host
      | addrs -> Ok addrs
      | exception (Eio.Io _ as e) ->
          Fmt.error_msg "cannot resolve %S: %a" host Eio.Exn.pp e)

let of_stream = function
  | `Tcp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None

let of_datagram = function
  | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
