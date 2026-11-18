let ip = Alcotest.testable Ipaddr.pp ( = )
and eio_ip = Alcotest.testable Eio.Net.Ipaddr.pp ( = )

let test_roundtrip () =
  let check s =
    let a = Ipaddr.of_string_exn s in
    Alcotest.(check ip) s a (Eio_net_addr.ipaddr (Eio_net_addr.ip a))
  in
  check "10.0.0.1";
  check "127.0.0.1";
  check "0.0.0.0";
  check "::1";
  check "2001:db8::dead:beef"

let test_octets () =
  Alcotest.(check int)
    "IPv4 is four octets" 4
    (String.length
       (Eio_net_addr.ip (Ipaddr.of_string_exn "10.0.0.1") :> string));
  Alcotest.(check int)
    "IPv6 is sixteen" 16
    (String.length (Eio_net_addr.ip (Ipaddr.of_string_exn "::1") :> string));
  Alcotest.(check eio_ip)
    "the loopback Eio names itself" Eio.Net.Ipaddr.V4.loopback
    (Eio_net_addr.ip (Ipaddr.of_string_exn "127.0.0.1"))

let test_sockaddr () =
  let a = Ipaddr.of_string_exn "192.0.2.7" in
  Alcotest.(check (option (pair ip int)))
    "a stream address"
    (Some (a, 9997))
    (Eio_net_addr.of_sockaddr (Eio_net_addr.tcp a 9997));
  Alcotest.(check (option (pair ip int)))
    "a datagram address"
    (Some (a, 53))
    (Eio_net_addr.of_sockaddr (Eio_net_addr.udp a 53));
  Alcotest.(check (option (pair ip int)))
    "a unix socket names no address" None
    (Eio_net_addr.of_sockaddr (`Unix "/tmp/agent.sock"))

(* A comparison of the ports alone or of the hosts alone answers one of these
   wrongly. *)
let test_equal_stream_sockaddr () =
  let a = Ipaddr.of_string_exn "192.0.2.7"
  and b = Ipaddr.of_string_exn "192.0.2.8" in
  let check name want x y =
    Alcotest.(check bool) name want (Eio_net_addr.equal_sockaddr x y)
  in
  check "the same address" true (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp a 1);
  check "another host" false (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp b 1);
  check "another port" false (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp a 2);
  check "a path and a port" false (`Unix "/a") (Eio_net_addr.tcp a 1);
  check "the same path" true (`Unix "/a") (`Unix "/a");
  check "another path" false (`Unix "/a") (`Unix "/b")

let test_equal_datagram_sockaddr () =
  let a = Ipaddr.of_string_exn "127.0.0.1"
  and b = Ipaddr.of_string_exn "127.0.0.2" in
  let check name want x y =
    Alcotest.(check bool) name want (Eio_net_addr.equal_sockaddr x y)
  in
  check "the same address" true (Eio_net_addr.udp a 9000)
    (`Udp (Eio.Net.Ipaddr.of_raw "\127\000\000\001", 9000));
  check "another port" false (Eio_net_addr.udp a 9000) (Eio_net_addr.udp a 9001);
  check "another host" false (Eio_net_addr.udp a 9000) (Eio_net_addr.udp b 9000);
  check "the same path" true (`Unix "/tmp/a") (`Unix "/tmp/a");
  check "a path and a port" false (Eio_net_addr.udp a 9000) (`Unix "/tmp/a");
  check "a stream and a datagram address" false
    (Eio_net_addr.tcp a 9000 :> Eio.Net.Sockaddr.t)
    (Eio_net_addr.udp a 9000 :> Eio.Net.Sockaddr.t)

let suite =
  ( "eio_net_addr",
    [
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "octets" `Quick test_octets;
      Alcotest.test_case "sockaddr" `Quick test_sockaddr;
      Alcotest.test_case "stream equality" `Quick test_equal_stream_sockaddr;
      Alcotest.test_case "datagram equality" `Quick test_equal_datagram_sockaddr;
    ] )
