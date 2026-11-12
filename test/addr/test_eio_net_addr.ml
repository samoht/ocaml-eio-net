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
    (Eio_net_addr.of_stream (Eio_net_addr.tcp a 9997));
  Alcotest.(check (option (pair ip int)))
    "a datagram address"
    (Some (a, 53))
    (Eio_net_addr.of_datagram (Eio_net_addr.udp a 53));
  Alcotest.(check (option (pair ip int)))
    "a unix socket names no address" None
    (Eio_net_addr.of_stream (`Unix "/tmp/agent.sock"))

(* A comparison of the ports alone or of the hosts alone answers one of these
   wrongly. *)
let test_equal_stream () =
  let a = Ipaddr.of_string_exn "192.0.2.7"
  and b = Ipaddr.of_string_exn "192.0.2.8" in
  let check name want x y =
    Alcotest.(check bool) name want (Eio_net_addr.equal_stream x y)
  in
  check "the same address" true (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp a 1);
  check "another host" false (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp b 1);
  check "another port" false (Eio_net_addr.tcp a 1) (Eio_net_addr.tcp a 2);
  check "a path and a port" false (`Unix "/a") (Eio_net_addr.tcp a 1);
  check "the same path" true (`Unix "/a") (`Unix "/a");
  check "another path" false (`Unix "/a") (`Unix "/b")

let test_equal_datagram () =
  let a = Ipaddr.of_string_exn "127.0.0.1"
  and b = Ipaddr.of_string_exn "127.0.0.2" in
  let check name want x y =
    Alcotest.(check bool) name want (Eio_net_addr.equal_datagram x y)
  in
  check "the same address" true (Eio_net_addr.udp a 9000)
    (`Udp (Eio.Net.Ipaddr.of_raw "\127\000\000\001", 9000));
  check "another port" false (Eio_net_addr.udp a 9000) (Eio_net_addr.udp a 9001);
  check "another host" false (Eio_net_addr.udp a 9000) (Eio_net_addr.udp b 9000);
  check "the same path" true (`Unix "/tmp/a") (`Unix "/tmp/a");
  check "a path and a port" false (Eio_net_addr.udp a 9000) (`Unix "/tmp/a")

let test_not_an_address () =
  Alcotest.check_raises "a hostname is not an address"
    (Invalid_argument
       "An IP address must be either 4 or 16 bytes long (\"flatsat-1\" is 9 \
        bytes)") (fun () ->
      ignore (Eio_net_addr.ip (Ipaddr.of_string_exn "10.0.0.1"));
      ignore (Eio_net_addr.ipaddr (Eio.Net.Ipaddr.of_raw "flatsat-1")))

let stream net ~host ~port =
  match Eio_net_addr.stream net ~host ~port with
  | Ok addrs -> List.filter_map Eio_net_addr.of_stream addrs
  | Error (`Msg msg) -> Alcotest.fail msg

let test_stream_literal () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let a = Ipaddr.of_string_exn "192.0.2.7" in
  Alcotest.(check (list (pair ip int)))
    "a literal IPv4 host"
    [ (a, 9997) ]
    (stream net ~host:"192.0.2.7" ~port:9997);
  let a6 = Ipaddr.of_string_exn "2001:db8::1" in
  Alcotest.(check (list (pair ip int)))
    "a literal IPv6 host"
    [ (a6, 9998) ]
    (stream net ~host:"2001:db8::1" ~port:9998)

(* A name gives every address it resolves to, in the resolver's order, for a
   client to try in turn. *)
let test_stream_every_address () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  Alcotest.(check (list (pair ip int)))
    "localhost"
    [ (Ipaddr.of_string_exn "::1", 80); (Ipaddr.of_string_exn "127.0.0.1", 80) ]
    (stream net ~host:"localhost" ~port:80)

(* RFC 6761 sec 6.4: the ".invalid" TLD is reserved and does not resolve. *)
let test_stream_unresolvable () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  match Eio_net_addr.stream net ~host:"flatsat-1.invalid" ~port:9997 with
  | Ok addrs ->
      Alcotest.failf "an unresolvable host produced addresses: %a"
        Fmt.(Dump.list Eio.Net.Sockaddr.pp)
        addrs
  | Error (`Msg msg) ->
      Alcotest.(check bool)
        "the error names the host" true
        (String.starts_with ~prefix:"cannot resolve \"flatsat-1.invalid\": " msg)

let suite =
  ( "eio_net_addr",
    [
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "octets" `Quick test_octets;
      Alcotest.test_case "sockaddr" `Quick test_sockaddr;
      Alcotest.test_case "stream equality" `Quick test_equal_stream;
      Alcotest.test_case "datagram equality" `Quick test_equal_datagram;
      Alcotest.test_case "not an address" `Quick test_not_an_address;
      Alcotest.test_case "stream: a literal host" `Quick test_stream_literal;
      Alcotest.test_case "stream: every address of a name" `Quick
        test_stream_every_address;
      Alcotest.test_case "stream: an unresolvable host" `Quick
        test_stream_unresolvable;
    ] )
