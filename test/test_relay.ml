let loopback port = `Tcp (Eio.Net.Ipaddr.V4.loopback, port)

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
    Eio_net.Relay.forward ~sw ~net ~max_connections:4 ~listen:(loopback 0)
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
    Eio_net.Relay.forward ~sw ~net ~max_connections:4 ~listen:(loopback 0)
      ~connect:(loopback 1) ()
  in
  Alcotest.(check bool) "a real port, not 0" true (port_of bound > 0)

let test_dial_failure_keeps_listening () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let upstream = echo_server ~sw ~net in
  let failures = ref 0 in
  let dead =
    Eio_net.Relay.forward ~sw ~net ~max_connections:4
      ~on_error:(fun _ -> incr failures)
      ~listen:(loopback 0) ~connect:(loopback 1) ()
  in
  (let flow = Eio.Net.connect ~sw net (loopback (port_of dead)) in
   Eio.Flow.copy_string "lost" flow;
   Eio.Flow.shutdown flow `Send;
   (* Clean end-of-file or a reset; either way nothing comes back. *)
   let got =
     try Eio.Buf_read.of_flow ~max_size:1_000 flow |> Eio.Buf_read.take_all
     with Eio.Io (Eio.Net.E (Connection_reset _), _) -> ""
   in
   Alcotest.(check string) "nothing comes back from a dead upstream" "" got);
  Alcotest.(check bool) "the failure was reported" true (!failures >= 1);
  let dead2 = Eio.Net.connect ~sw net (loopback (port_of dead)) in
  Eio.Flow.shutdown dead2 `Send;
  let healthy =
    Eio_net.Relay.forward ~sw ~net ~max_connections:4 ~listen:(loopback 0)
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

(* A relay declaring [max_connections] relays at most that many at once: a
   client past it is accepted and waits, holding no upstream connection, until
   a relayed one ends. The mock backend runs every fiber until it blocks, so
   the upstream's count after the clients settle is all the relay dialled. *)
let test_forward_holds_its_bound () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let dialled = ref 0 in
  let upstream = Eio.Net.listen ~sw ~backlog:16 net (loopback 0) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec serve () =
        Eio.Net.accept_fork ~sw upstream ~on_error:raise (fun flow _ ->
            incr dialled;
            Eio.Flow.copy flow flow);
        serve ()
      in
      serve ());
  let bound =
    Eio_net.Relay.forward ~sw ~net ~max_connections:1 ~listen:(loopback 0)
      ~connect:(Eio.Net.listening_addr upstream)
      ()
  in
  let settle () =
    for _ = 1 to 100 do
      Eio.Fiber.yield ()
    done
  in
  Eio.Switch.run (fun second ->
      Eio.Switch.run (fun first ->
          ignore (Eio.Net.connect ~sw:first net bound);
          ignore (Eio.Net.connect ~sw:second net bound);
          settle ();
          Alcotest.(check int) "one client relayed of two" 1 !dialled);
      settle ();
      Alcotest.(check int) "the second once the first ended" 2 !dialled)

let suite =
  ( "relay",
    [
      Alcotest.test_case "forward echoes through" `Quick test_forward_echo;
      Alcotest.test_case "port 0 reports the real port" `Quick
        test_forward_reports_port;
      Alcotest.test_case "dial failure keeps listening" `Quick
        test_dial_failure_keeps_listening;
      Alcotest.test_case "forward holds its bound" `Quick
        test_forward_holds_its_bound;
    ] )
