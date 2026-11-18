(* How long an end that closed before its peer's FIN holds its address, in
   FIN_WAIT2: Linux's tcp_fin_time takes the socket's TCP_LINGER2 when one is
   set, and tcp_fin_timeout, 60 seconds, otherwise. *)

let lo = Eio.Net.Ipaddr.V4.loopback

let in_use net addr =
  Eio.Switch.run @@ fun sw ->
  match Eio.Net.listen ~sw ~backlog:1 net addr with
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _)
    ->
      true
  | _ -> false

let at clock secs =
  Eio_mock.Clock.Mono.set_time clock
    (Mtime.of_uint64_ns (Int64.mul (Int64.of_int secs) 1_000_000_000L))

let test_linger2 () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let addr = `Tcp (lo, 8190) in
  let listener = Eio.Net.listen ~sw ~backlog:1 net addr in
  let _client = Eio.Net.connect ~sw net addr in
  let server, _ = Eio.Net.accept ~sw listener in
  Eio.Resource.close listener;
  Eio.Net.setsockopt server Eio.Net.Sockopt.TCP_LINGER2 (Some 10);
  Eio.Resource.close server;
  at clock 9;
  Alcotest.(check bool) "an orphan at 9 s" true (in_use net addr);
  at clock 10;
  Alcotest.(check bool) "an orphan at 10 s" false (in_use net addr)

let suite =
  ( "fin_wait",
    [ Alcotest.test_case "TCP_LINGER2 bounds FIN_WAIT2" `Quick test_linger2 ] )
