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

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
      Alcotest.test_case "write after peer close" `Quick
        test_write_after_peer_close;
      Alcotest.test_case "write after own shutdown" `Quick
        test_write_after_own_shutdown;
      Alcotest.test_case "read after peer close" `Quick
        test_read_after_peer_close;
    ] )
