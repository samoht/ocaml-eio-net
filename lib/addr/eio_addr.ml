let ip a = Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets a)

let ipaddr (a : Eio.Net.Ipaddr.v4v6) =
  match Ipaddr.of_octets (a :> string) with
  | Ok a -> a
  | Error (`Msg msg) -> invalid_arg ("Eio_addr.ipaddr: " ^ msg)

let tcp a port = `Tcp (ip a, port)
let udp a port = `Udp (ip a, port)

let stream net ~host ~port =
  match Ipaddr.of_string host with
  | Ok ip -> tcp ip port
  | Error _ -> (
      match
        Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
      with
      | addr :: _ -> addr
      | [] -> Fmt.invalid_arg "%s: cannot resolve %S" __FUNCTION__ host
      | exception (Eio.Io _ as e) ->
          Fmt.invalid_arg "%s: cannot resolve %S: %a" __FUNCTION__ host Fmt.exn
            e)

let of_stream = function
  | `Tcp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None

let of_datagram = function
  | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
