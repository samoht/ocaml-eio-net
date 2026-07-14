(* The Ipaddr <-> Eio crossing, both ways and both families. Eio names an
   address by its raw octets, so a wrong conversion is not a type error -- it is
   4 bytes where 16 belong, or a hostname where octets belong. *)

let ip = Alcotest.testable Ipaddr.pp ( = )
and eio_ip = Alcotest.testable Eio.Net.Ipaddr.pp ( = )

(* Round-tripping an address through Eio's representation is the identity. *)
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

(* An IPv4 address is four octets to Eio, an IPv6 one sixteen. *)
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

(* Socket addresses carry the address and the port, and read back. *)
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

(* Octets are not a name: Eio addresses are 4 or 16 raw bytes, so a hostname
   handed to [Eio.Net.Ipaddr.of_raw] raises rather than becoming an address --
   which is why a caller with a name in hand parses it ([Ipaddr.of_string]) or
   resolves it ([Eio.Net.getaddrinfo]) before crossing. *)
let test_not_an_address () =
  Alcotest.check_raises "a hostname is not an address"
    (Invalid_argument
       "An IP address must be either 4 or 16 bytes long (\"flatsat-1\" is 9 \
        bytes)") (fun () ->
      ignore (Eio_addr.ip (Ipaddr.of_string_exn "10.0.0.1"));
      ignore (Eio_addr.ipaddr (Eio.Net.Ipaddr.of_raw "flatsat-1")))

let suite =
  ( "addr",
    [
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "octets" `Quick test_octets;
      Alcotest.test_case "sockaddr" `Quick test_sockaddr;
      Alcotest.test_case "not an address" `Quick test_not_an_address;
    ] )
