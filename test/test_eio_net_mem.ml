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

let test_connect_to_wildcard_listener () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let listener =
    Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.any, 8081))
  in
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Flow.copy_string "ok" flow)
    (fun () ->
      let flow =
        Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8081))
      in
      let buf = Cstruct.create 2 in
      Eio.Flow.read_exact flow buf;
      Alcotest.(check string)
        "wildcard accepted loopback" "ok" (Cstruct.to_string buf))

(* A write to a mem flow whose peer has closed raises the same [Eio.Io]
   connection-reset a real socket raises on write-after-reset, not the bare
   [Failure] that [Eio.Buf_write] raises internally on a closed writer. That
   fidelity is what lets an adapter's best-effort [Eio.Io] catch behave the
   same against the fake as against a real socket. *)
let test_write_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  (* The peer goes away, closing the client's send channel. *)
  Eio.Flow.close server;
  match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | exception Failure msg ->
      Alcotest.failf "write to closed mem flow leaked Failure %S" msg
  | exception e ->
      Alcotest.failf "write to closed mem flow raised %s" (Printexc.to_string e)
  | (_ : int) -> Alcotest.fail "write to closed mem flow should raise"

(* Closing this end's own send side also makes a subsequent write raise the
   faithful reset (a real socket's EPIPE), not [Failure]. *)
let test_write_after_own_shutdown () =
  Eio_mock.Backend.run @@ fun () ->
  let client, _server = Eio_net_mem.pipe () in
  Eio.Flow.shutdown client `Send;
  match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | exception Failure msg ->
      Alcotest.failf "write after own shutdown leaked Failure %S" msg
  | exception e ->
      Alcotest.failf "write after own shutdown raised %s" (Printexc.to_string e)
  | (_ : int) -> Alcotest.fail "write after own shutdown should raise"

(* A read after the peer closed returns end-of-file, matching a real socket:
   any buffered bytes drain first, then [single_read] raises [End_of_file]. *)
let test_read_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.copy_string "hi" client;
  Eio.Flow.close client;
  let buf = Cstruct.create 16 in
  let n = Eio.Flow.single_read server buf in
  Alcotest.(check string)
    "buffered bytes drain first" "hi"
    (Cstruct.to_string (Cstruct.sub buf 0 n));
  match Eio.Flow.single_read server buf with
  | exception End_of_file -> ()
  | exception e ->
      Alcotest.failf "read after peer close raised %s" (Printexc.to_string e)
  | (_ : int) -> Alcotest.fail "read after peer close should reach EOF"

(* A latency hint this transport already satisfies connects rather than
   raising. conpool and happy-eyeballs pass TCP_NODELAY on every connect, so a
   refusal here is a failed connect for every caller reaching a mem network
   through an HTTP client. *)
let test_connect_ignores_latency_hint () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8082) in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Flow.copy_string "ok" flow)
    (fun () ->
      let flow =
        Eio.Net.connect ~sw
          ~options:Eio.Net.Sockopt.[ (TCP_NODELAY, true); (TCP_QUICKACK, true) ]
          net addr
      in
      let buf = Cstruct.create 2 in
      Eio.Flow.read_exact flow buf;
      Alcotest.(check string)
        "connected with hints set" "ok" (Cstruct.to_string buf))

(* An option that would change what a caller observes is still refused, and
   the refusal names it. Accepting this one silently would trade a loud wrong
   answer for a quiet one: the connection would have no keepalive and nothing
   would say so. *)
let test_connect_refuses_semantic_option () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8083) in
  let _listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  match
    Eio.Net.connect ~sw
      ~options:Eio.Net.Sockopt.[ (TCP_NODELAY, true); (SO_KEEPALIVE, true) ]
      net addr
  with
  | exception Invalid_argument msg ->
      Alcotest.(check string)
        "refusal names the option"
        "eio-net.mem does not support socket option SO_KEEPALIVE" msg
  | exception e ->
      Alcotest.failf "keepalive on a mem connect raised %s"
        (Printexc.to_string e)
  | _ -> Alcotest.fail "keepalive on a mem connect should be refused"

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
      Alcotest.test_case "connect to wildcard listener" `Quick
        test_connect_to_wildcard_listener;
      Alcotest.test_case "write after peer close" `Quick
        test_write_after_peer_close;
      Alcotest.test_case "write after own shutdown" `Quick
        test_write_after_own_shutdown;
      Alcotest.test_case "read after peer close" `Quick
        test_read_after_peer_close;
      Alcotest.test_case "connect ignores a latency hint" `Quick
        test_connect_ignores_latency_hint;
      Alcotest.test_case "connect refuses a semantic option" `Quick
        test_connect_refuses_semantic_option;
    ] )
