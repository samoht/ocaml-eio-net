(* Eio names an IP address by its raw octets; Ipaddr names it by a parsed type.
   Both directions, once. *)

let ip a = Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets a)

let ipaddr (a : Eio.Net.Ipaddr.v4v6) =
  match Ipaddr.of_octets (a :> string) with
  | Ok a -> a
  | Error (`Msg msg) -> invalid_arg ("Eio_addr.ipaddr: " ^ msg)

let tcp a port = `Tcp (ip a, port)
let udp a port = `Udp (ip a, port)

(* A host is a name or a literal address; Eio names an address by its raw
   octets, so neither reaches [`Tcp] without being parsed or resolved first. *)
let stream net ~host ~port =
  match Ipaddr.of_string host with
  | Ok ip -> tcp ip port
  | Error _ -> (
      match
        Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
      with
      | addr :: _ -> addr
      | [] -> Fmt.invalid_arg "%s: cannot resolve %S" __FUNCTION__ host
      (* Only a lookup failure is a bad host name. A catch-all here would also
         take [Eio.Cancel.Cancelled] and report a cancelled connect as an
         unresolvable name, leaving the fiber running past its cancel
         context. *)
      | exception (Eio.Io _ as e) ->
          Fmt.invalid_arg "%s: cannot resolve %S: %a" __FUNCTION__ host Fmt.exn
            e)

let of_stream = function
  | `Tcp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None

let of_datagram = function
  | `Udp (a, port) -> Some (ipaddr a, port)
  | `Unix _ -> None
