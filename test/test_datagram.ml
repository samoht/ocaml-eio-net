(* Bound (not socketpair) sockets, so each received datagram carries the real
   source address the tests assert on. *)
let with_bound_pair f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = Filename.temp_dir "eio-net-dgram" "" in
  Fun.protect ~finally:(fun () ->
      Array.iter
        (fun name -> Unix.unlink (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
  @@ fun () ->
  let addr_a = `Unix (Filename.concat dir "a.sock") in
  let addr_b = `Unix (Filename.concat dir "b.sock") in
  let sock_a = Eio.Net.datagram_socket ~sw env#net addr_a in
  let sock_b = Eio.Net.datagram_socket ~sw env#net addr_b in
  f ~addr_a ~addr_b ~sock_a ~sock_b

let send channel dst payload =
  channel.Eio_net.Datagram.send dst [ Cstruct.of_string payload ]

let test_source_round_trip () =
  with_bound_pair @@ fun ~addr_a ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Datagram.of_socket sock_b in
  send a (Some addr_b) "ping";
  let source, payload = b.Eio_net.Datagram.recv () in
  Alcotest.(check string) "request arrives whole" "ping" payload;
  Alcotest.(check bool)
    "source is the sender's bound address" true
    (Eio_net.Datagram.equal_addr source addr_a);
  send b (Some source) "pong";
  let reply_source, reply = a.Eio_net.Datagram.recv () in
  Alcotest.(check string) "reply reaches the source" "pong" reply;
  Alcotest.(check bool)
    "reply comes from the responder" true
    (Eio_net.Datagram.equal_addr reply_source addr_b)

let test_default_destination () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket ~dst:addr_b sock_a in
  let b = Eio_net.Datagram.of_socket sock_b in
  send a None "to-default";
  let _, payload = b.Eio_net.Datagram.recv () in
  Alcotest.(check string) "datagram reaches configured dst" "to-default" payload

let test_oversized_refused () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Datagram.of_socket ~max_frame:8 sock_b in
  send a (Some addr_b) "exactly-9";
  send a (Some addr_b) "8-bytes!";
  let refused =
    match b.Eio_net.Datagram.recv () with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool) "9-byte datagram refused at max_frame 8" true refused;
  let _, payload = b.Eio_net.Datagram.recv () in
  Alcotest.(check string) "channel survives the refusal" "8-bytes!" payload

let test_frame_carrier_refuses_oversize () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Frame.of_datagram_socket ~max_frame:4 sock_b in
  send a (Some addr_b) "five!";
  let refused =
    match b.Eio_net.Frame.recv () with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool) "frame channel refuses oversized datagram" true refused

let test_equal_addr () =
  let ip raw = Eio.Net.Ipaddr.of_raw raw in
  let localhost = ip "\127\000\000\001" in
  Alcotest.(check bool)
    "same host and port" true
    (Eio_net.Datagram.equal_addr
       (`Udp (localhost, 9000))
       (`Udp (ip "\127\000\000\001", 9000)));
  Alcotest.(check bool)
    "different port" false
    (Eio_net.Datagram.equal_addr
       (`Udp (localhost, 9000))
       (`Udp (localhost, 9001)));
  Alcotest.(check bool)
    "different host" false
    (Eio_net.Datagram.equal_addr
       (`Udp (localhost, 9000))
       (`Udp (ip "\127\000\000\002", 9000)));
  Alcotest.(check bool)
    "unix paths compare by path" true
    (Eio_net.Datagram.equal_addr (`Unix "/tmp/a") (`Unix "/tmp/a"));
  Alcotest.(check bool)
    "udp never equals unix" false
    (Eio_net.Datagram.equal_addr (`Udp (localhost, 9000)) (`Unix "/tmp/a"))

let suite =
  ( "datagram",
    [
      Alcotest.test_case "source round-trip" `Quick test_source_round_trip;
      Alcotest.test_case "default destination" `Quick test_default_destination;
      Alcotest.test_case "oversized datagram refused" `Quick
        test_oversized_refused;
      Alcotest.test_case "frame carrier refuses oversize" `Quick
        test_frame_carrier_refuses_oversize;
      Alcotest.test_case "address equality" `Quick test_equal_addr;
    ] )
