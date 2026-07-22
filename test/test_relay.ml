(* The relay's promises: bytes cross both ways, a finished direction reaches
   the peer as a half-close, and one bad connection never takes the listener
   down. Exercised over real loopback sockets because listening and accepting
   is the part a mock flow cannot vouch for. *)

let loopback port = `Tcp (Eio.Net.Ipaddr.V4.loopback, port)

(* An echo server on an ephemeral port, reading to end-of-stream and writing
   everything back -- the shape that only terminates if the relay propagates
   the client's half-close. *)
let echo_server ~sw ~net =
  let socket =
    Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true net (loopback 0)
  in
  let rec serve () : [ `Stop_daemon ] =
    Eio.Net.accept_fork ~sw socket
      ~on_error:(fun _ -> ())
      (fun flow _addr ->
        let all =
          Eio.Buf_read.of_flow ~max_size:1_000_000 flow |> Eio.Buf_read.take_all
        in
        Eio.Flow.copy_string all flow;
        Eio.Flow.shutdown flow `Send);
    serve ()
  in
  Eio.Fiber.fork_daemon ~sw serve;
  Eio.Net.listening_addr socket

let port_of = function
  | `Tcp (_, port) -> port
  | `Unix _ -> Alcotest.fail "expected a TCP address"

let test_forward_echo () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let upstream = echo_server ~sw ~net in
  let bound =
    Eio_net.Relay.forward ~sw ~net ~listen:(loopback 0)
      ~connect:(loopback (port_of upstream))
      ()
  in
  let flow = Eio.Net.connect ~sw net (loopback (port_of bound)) in
  Eio.Flow.copy_string "through the relay" flow;
  Eio.Flow.shutdown flow `Send;
  let back =
    Eio.Buf_read.of_flow ~max_size:1_000_000 flow |> Eio.Buf_read.take_all
  in
  Alcotest.(check string) "echoed through" "through the relay" back

let test_forward_reports_port () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let bound =
    Eio_net.Relay.forward ~sw ~net ~listen:(loopback 0) ~connect:(loopback 1) ()
  in
  Alcotest.(check bool) "a real port, not 0" true (port_of bound > 0)

let test_dial_failure_keeps_listening () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let upstream = echo_server ~sw ~net in
  let failures = ref 0 in
  (* First relay points at a dead port: its connections fail and are reported.
     The listener must survive to serve the second, healthy exchange. *)
  let dead =
    Eio_net.Relay.forward ~sw ~net
      ~on_error:(fun _ -> incr failures)
      ~listen:(loopback 0) ~connect:(loopback 1) ()
  in
  (let flow = Eio.Net.connect ~sw net (loopback (port_of dead)) in
   Eio.Flow.copy_string "lost" flow;
   Eio.Flow.shutdown flow `Send;
   (* Clean end-of-file or a reset, depending on whether the relay closed
      before or after the client's bytes arrived; either way nothing comes
      back. *)
   let got =
     try Eio.Buf_read.of_flow ~max_size:1_000 flow |> Eio.Buf_read.take_all
     with Eio.Io (Eio.Net.E (Connection_reset _), _) -> ""
   in
   Alcotest.(check string) "nothing comes back from a dead upstream" "" got);
  Alcotest.(check bool) "the failure was reported" true (!failures >= 1);
  (* Same listener, second connection: still accepting after the failure. *)
  let dead2 = Eio.Net.connect ~sw net (loopback (port_of dead)) in
  Eio.Flow.shutdown dead2 `Send;
  let healthy =
    Eio_net.Relay.forward ~sw ~net ~listen:(loopback 0)
      ~connect:(loopback (port_of upstream))
      ()
  in
  let flow = Eio.Net.connect ~sw net (loopback (port_of healthy)) in
  Eio.Flow.copy_string "still up" flow;
  Eio.Flow.shutdown flow `Send;
  let back =
    Eio.Buf_read.of_flow ~max_size:1_000 flow |> Eio.Buf_read.take_all
  in
  Alcotest.(check string) "relaying still works" "still up" back

let suite =
  ( "relay",
    [
      Alcotest.test_case "forward echoes through" `Quick test_forward_echo;
      Alcotest.test_case "port 0 reports the real port" `Quick
        test_forward_reports_port;
      Alcotest.test_case "dial failure keeps listening" `Quick
        test_dial_failure_keeps_listening;
    ] )
