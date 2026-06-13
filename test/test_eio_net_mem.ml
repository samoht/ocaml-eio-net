let sockaddr = Alcotest.testable Eio.Net.Sockaddr.pp ( = )

let test_tcp_roundtrip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Eio.Fiber.both
    (fun () ->
      let flow, _peer = Eio.Net.accept ~sw listener in
      let got = Eio.Buf_read.(parse_exn take_all) flow ~max_size:16 in
      Alcotest.(check string) "server read" "ping" got;
      Eio.Flow.copy_string "pong" flow;
      Eio.Flow.shutdown flow `Send)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      Eio.Flow.copy_string "ping" flow;
      Eio.Flow.shutdown flow `Send;
      let got = Eio.Buf_read.(parse_exn take_all) flow ~max_size:16 in
      Alcotest.(check string) "client read" "pong" got)

let test_resolve_localhost () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  Alcotest.(check (list sockaddr))
    "localhost resolves"
    [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) ]
    (Eio.Net.getaddrinfo_stream net "localhost" ~service:"80")

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
    ] )
