let ip = Alcotest.testable Ipaddr.pp ( = )
and eio_ip = Alcotest.testable Eio.Net.Ipaddr.pp ( = )

let test_roundtrip () =
  let check s =
    let a = Ipaddr.of_string_exn s in
    Alcotest.(check ip) s a (Eio_addr.ipaddr (Eio_addr.ip a))
  in
  check "10.0.0.1";
  check "127.0.0.1";
  check "0.0.0.0";
  check "::1";
  check "2001:db8::dead:beef"

let test_octets () =
  Alcotest.(check int)
    "IPv4 is four octets" 4
    (String.length (Eio_addr.ip (Ipaddr.of_string_exn "10.0.0.1") :> string));
  Alcotest.(check int)
    "IPv6 is sixteen" 16
    (String.length (Eio_addr.ip (Ipaddr.of_string_exn "::1") :> string));
  Alcotest.(check eio_ip)
    "the loopback Eio names itself" Eio.Net.Ipaddr.V4.loopback
    (Eio_addr.ip (Ipaddr.of_string_exn "127.0.0.1"))

let test_sockaddr () =
  let a = Ipaddr.of_string_exn "192.0.2.7" in
  Alcotest.(check (option (pair ip int)))
    "a stream address"
    (Some (a, 9997))
    (Eio_addr.of_stream (Eio_addr.tcp a 9997));
  Alcotest.(check (option (pair ip int)))
    "a datagram address"
    (Some (a, 53))
    (Eio_addr.of_datagram (Eio_addr.udp a 53));
  Alcotest.(check (option (pair ip int)))
    "a unix socket names no address" None
    (Eio_addr.of_stream (`Unix "/tmp/agent.sock"))

let test_not_an_address () =
  Alcotest.check_raises "a hostname is not an address"
    (Invalid_argument
       "An IP address must be either 4 or 16 bytes long (\"flatsat-1\" is 9 \
        bytes)") (fun () ->
      ignore (Eio_addr.ip (Ipaddr.of_string_exn "10.0.0.1"));
      ignore (Eio_addr.ipaddr (Eio.Net.Ipaddr.of_raw "flatsat-1")))

let test_stream_literal () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let a = Ipaddr.of_string_exn "192.0.2.7" in
  Alcotest.(check (option (pair ip int)))
    "a literal IPv4 host"
    (Some (a, 9997))
    (Eio_addr.of_stream (Eio_addr.stream net ~host:"192.0.2.7" ~port:9997));
  let a6 = Ipaddr.of_string_exn "2001:db8::1" in
  Alcotest.(check (option (pair ip int)))
    "a literal IPv6 host"
    (Some (a6, 9998))
    (Eio_addr.of_stream (Eio_addr.stream net ~host:"2001:db8::1" ~port:9998))

(* RFC 6761 sec 6.4: the ".invalid" TLD is reserved and does not resolve. *)
let test_stream_unresolvable () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  match Eio_addr.stream net ~host:"flatsat-1.invalid" ~port:9997 with
  | addr ->
      Alcotest.failf "an unresolvable host produced an address: %a"
        Eio.Net.Sockaddr.pp addr
  | exception Invalid_argument _ -> ()

let suite =
  ( "eio_addr",
    [
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "octets" `Quick test_octets;
      Alcotest.test_case "sockaddr" `Quick test_sockaddr;
      Alcotest.test_case "not an address" `Quick test_not_an_address;
      Alcotest.test_case "stream: a literal host" `Quick test_stream_literal;
      Alcotest.test_case "stream: an unresolvable host" `Quick
        test_stream_unresolvable;
    ] )
