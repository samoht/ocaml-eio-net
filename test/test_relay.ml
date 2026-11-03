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

(* A listener over [inner] whose accepts fail with each of [failures] in
   turn before they accept from [inner]. *)
module Failing_listener = struct
  type t = {
    inner : [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
    mutable failures : exn list;
  }

  type tag = [ `Generic ]

  let accept t ~sw =
    match t.failures with
    | exn :: rest ->
        t.failures <- rest;
        raise exn
    | [] -> Eio.Net.accept ~sw t.inner

  let close t = Eio.Resource.close t.inner
  let listening_addr t = Eio.Net.listening_addr t.inner
  let setsockopt t opt v = Eio.Net.setsockopt t.inner opt v
  let getsockopt t opt = Eio.Net.getsockopt t.inner opt
end

(* A network over [inner] whose listeners are [Failing_listener]s. *)
module Failing_net = struct
  type t = {
    inner : [ `Generic ] Eio.Net.ty Eio.Resource.t;
    failures : exn list;
  }

  type tag = [ `Generic ]

  let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
    let inner =
      Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.inner addr
    in
    Eio.Resource.T
      ( { Failing_listener.inner; failures = t.failures },
        Eio.Net.Pi.listening_socket (module Failing_listener) )

  let connect t ~bind_to ~options ~sw addr =
    Eio.Net.connect ?bind_to ~options ~sw t.inner addr

  let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
    Eio.Net.datagram_socket ~reuse_addr ~reuse_port ~sw t.inner addr

  let getaddrinfo t ~service host = Eio.Net.getaddrinfo ~service t.inner host
  let getnameinfo t addr = Eio.Net.getnameinfo t.inner addr
end

let failing_net inner failures : [ `Generic ] Eio.Net.ty Eio.Resource.t =
  Eio.Resource.T
    ({ Failing_net.inner; failures }, Eio.Net.Pi.network (module Failing_net))

let pending_error () = Eio_unix.Err.v Unix.ENETDOWN "accept" ""

(* An upstream on [net] that echoes each connection, and its address. *)
let mem_echo ~sw net =
  let upstream = Eio.Net.listen ~sw ~backlog:16 net (loopback 0) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec serve () =
        Eio.Net.accept_fork ~sw upstream ~on_error:raise (fun flow _ ->
            Eio.Flow.copy flow flow;
            Eio.Flow.shutdown flow `Send);
        serve ()
      in
      serve ());
  Eio.Net.listening_addr upstream

let relayed ~sw net bound data =
  let flow = Eio.Net.connect ~sw net bound in
  Eio.Flow.copy_string data flow;
  Eio.Flow.shutdown flow `Send;
  Eio.Buf_read.(parse_exn take_all) ~max_size:1_000 flow

(* A relay reads its accepts' failures with the classifier it is handed: one
   that reads a pending network error (ENETDOWN) as an aborted accept relays the
   next client. *)
let test_forward_reads_failures_with_its_classifier () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let mem = Eio_net_mem.v ~sw () in
  let upstream = mem_echo ~sw mem in
  let net = failing_net mem [ pending_error (); pending_error () ] in
  let classify = function
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENETDOWN, _, _)), _) ->
        Some `Aborted
    | _ -> None
  in
  let bound =
    Eio_net.Relay.forward ~sw ~net ~classify ~max_connections:4
      ~listen:(loopback 0) ~connect:upstream ()
  in
  Alcotest.(check string)
    "relayed past the failed accepts" "after"
    (relayed ~sw mem bound "after")

(* A relay handed a pool frees a descriptor another loop of it holds: when its
   accept finds none left (EMFILE), it sheds the stalled head that loop holds,
   and relays its client once that connection has ended. *)
let test_forward_sheds_within_its_pool () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let mem = Eio_net_mem.v ~sw () in
  let upstream = mem_echo ~sw mem in
  let pool = Eio_net_accept.pool () in
  let server = Eio.Net.listen ~sw ~backlog:16 mem (loopback 0) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio_net_accept.run ~pool ~max_connections:4 server (fun _head flow _ ->
          ignore (Eio.Buf_read.(parse_exn take_all) ~max_size:64 flow)));
  let shed, shed_r = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun csw ->
      let flow = Eio.Net.connect ~sw:csw mem (Eio.Net.listening_addr server) in
      Eio.Flow.copy_string "stalled" flow;
      match Eio.Flow.single_read flow (Cstruct.create 1) with
      | _ -> Alcotest.fail "a stalled head was answered"
      | exception End_of_file -> Eio.Promise.resolve shed_r ());
  (* The mock backend runs every fiber until it blocks: the server holds the
     stalled peer once these yields have run. *)
  for _ = 1 to 100 do
    Eio.Fiber.yield ()
  done;
  let net = failing_net mem [ Eio_unix.Err.v Unix.EMFILE "accept" "" ] in
  let classify = function
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, _, _)), _) ->
        Some `Descriptors
    | _ -> None
  in
  let bound =
    Eio_net.Relay.forward ~sw ~net ~pool ~classify ~max_connections:4
      ~listen:(loopback 0) ~connect:upstream ()
  in
  Alcotest.(check string)
    "relayed once a descriptor is freed" "freed"
    (relayed ~sw mem bound "freed");
  Alcotest.(check bool)
    "the other loop's stalled head was shed" true
    (Eio.Promise.is_resolved shed)

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
      Alcotest.test_case "forward reads failures with its classifier" `Quick
        test_forward_reads_failures_with_its_classifier;
      Alcotest.test_case "forward sheds within its pool" `Quick
        test_forward_sheds_within_its_pool;
    ] )
