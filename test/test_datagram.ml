(* Bound (not socketpair) sockets, so each received datagram carries the real
   source address the tests assert on. UDP on the loopback, not Unix-domain
   sockets: eio_linux 1.6 refuses a send to a Unix-domain datagram address
   with EINVAL (ocaml-multicore/eio#963). *)
let with_bound_pair f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bind () =
    Eio.Net.datagram_socket ~sw env#net (`Udp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let sock_a = bind () and sock_b = bind () in
  let addr sock =
    match Eio_unix.Resource.fd_opt sock with
    | None -> Alcotest.fail "a datagram socket with no descriptor"
    | Some fd -> (
        match Eio_unix.Fd.use_exn "getsockname" fd Unix.getsockname with
        | Unix.ADDR_INET (ip, port) ->
            `Udp (Eio_unix.Net.Ipaddr.of_unix ip, port)
        | Unix.ADDR_UNIX _ -> Alcotest.fail "a UDP socket bound to a path")
  in
  f ~addr_a:(addr sock_a) ~addr_b:(addr sock_b) ~sock_a ~sock_b

let send channel ?dst payload =
  Eio_net.Datagram.send channel ?dst [ Cstruct.of_string payload ]

let test_source_round_trip () =
  with_bound_pair @@ fun ~addr_a ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Datagram.of_socket sock_b in
  send a ~dst:addr_b "ping";
  let source, payload = Eio_net.Datagram.recv b in
  Alcotest.(check string) "request arrives whole" "ping" payload;
  Alcotest.(check bool)
    "source is the sender's bound address" true
    (Eio_net.Datagram.equal_addr source addr_a);
  send b ~dst:source "pong";
  let reply_source, reply = Eio_net.Datagram.recv a in
  Alcotest.(check string) "reply reaches the source" "pong" reply;
  Alcotest.(check bool)
    "reply comes from the responder" true
    (Eio_net.Datagram.equal_addr reply_source addr_b)

let test_default_destination () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket ~dst:addr_b sock_a in
  let b = Eio_net.Datagram.of_socket sock_b in
  send a "to-default";
  let _, payload = Eio_net.Datagram.recv b in
  Alcotest.(check string) "datagram reaches configured dst" "to-default" payload

(* A datagram longer than [max_frame] is the peer's doing, not the caller's:
   it is refused with an [Eio.Io] error, as a failed receive is. *)
let too_long ?(max = 8) f =
  match f () with
  | _ -> false
  | exception Eio.Io (Eio.Exn.X (Eio_net.Datagram.Too_long e), _) ->
      Alcotest.(check int) "the bound it names" max e.max;
      true

let test_oversized_refused () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Datagram.of_socket ~max_frame:8 sock_b in
  send a ~dst:addr_b "exactly-9";
  send a ~dst:addr_b "8-bytes!";
  Alcotest.(check bool)
    "9-byte datagram refused at max_frame 8" true
    (too_long (fun () -> Eio_net.Datagram.recv b));
  let _, payload = Eio_net.Datagram.recv b in
  Alcotest.(check string) "channel survives the refusal" "8-bytes!" payload

let test_frame_carrier_refuses_oversize () =
  with_bound_pair @@ fun ~addr_a:_ ~addr_b ~sock_a ~sock_b ->
  let a = Eio_net.Datagram.of_socket sock_a in
  let b = Eio_net.Frame.of_datagram_socket ~max_frame:4 sock_b in
  send a ~dst:addr_b "five!";
  Alcotest.(check bool)
    "recv refuses an oversized datagram" true
    (too_long ~max:4 (fun () -> Eio_net.Frame.recv b));
  send a ~dst:addr_b "five!";
  Alcotest.(check bool)
    "recv_batch refuses it" true
    (too_long ~max:4 (fun () -> Eio_net.Frame.recv_batch b));
  send a ~dst:addr_b "five!";
  Alcotest.(check bool)
    "recv_slice refuses it" true
    (too_long ~max:4 (fun () -> Eio_net.Frame.recv_slice b))

let test_too_long_prints () =
  Alcotest.(check string)
    "the printed error"
    "Eio.Io Too_long (a datagram longer than 8 bytes, eio-net)"
    (Printexc.to_string
       (Eio.Exn.create (Eio.Exn.X (Eio_net.Datagram.Too_long { max = 8 }))))

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
      Alcotest.test_case "Too_long prints its bound" `Quick test_too_long_prints;
      Alcotest.test_case "address equality" `Quick test_equal_addr;
    ] )
