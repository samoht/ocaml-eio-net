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

(* Port 0 asks the system to choose the port, as bind(2) does (ip(7),
   "ip_local_port_range"): each listener on port 0 holds a port of the dynamic
   range (RFC 6335, section 6) that no other listener holds, [listening_addr]
   reports it, and a client reaches that listener there. *)
let test_listen_on_port_zero () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let listen () =
    Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let a = listen () in
  let b = listen () in
  let port listener =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "a TCP listener has a TCP address"
  in
  let pa = port a and pb = port b in
  List.iter
    (fun p ->
      Alcotest.(check bool)
        (Fmt.str "port %d is in 49152-65535" p)
        true
        (p >= 49152 && p <= 65535))
    [ pa; pb ];
  Alcotest.(check bool) "two listeners hold two ports" true (pa <> pb);
  let reaches listener p msg =
    Eio.Fiber.both
      (fun () ->
        let flow, _ = Eio.Net.accept ~sw listener in
        Eio.Flow.copy_string msg flow)
      (fun () ->
        let flow =
          Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, p))
        in
        let buf = Cstruct.create 1 in
        Eio.Flow.read_exact flow buf;
        Alcotest.(check string)
          (Fmt.str "port %d reaches its listener" p)
          msg (Cstruct.to_string buf))
  in
  reaches a pa "a";
  reaches b pb "b"

(* A write to a mem flow whose peer has closed raises the [Eio.Io] reset a
   real socket raises, not the internal [Failure]. *)
let test_write_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.close server;
  match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | exception Failure msg ->
      Alcotest.failf "write to closed mem flow leaked Failure %S" msg
  | (_ : int) -> Alcotest.fail "write to closed mem flow should raise"

let test_write_after_own_shutdown () =
  Eio_mock.Backend.run @@ fun () ->
  let client, _server = Eio_net_mem.pipe () in
  Eio.Flow.shutdown client `Send;
  match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | exception Failure msg ->
      Alcotest.failf "write after own shutdown leaked Failure %S" msg
  | (_ : int) -> Alcotest.fail "write after own shutdown should raise"

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
  | (_ : int) -> Alcotest.fail "read after peer close should reach EOF"

(* A latency hint this transport already satisfies connects rather than
   raising. *)
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
  | _ -> Alcotest.fail "keepalive on a mem connect should be refused"

(* A connection made on a switch is closed when that switch finishes, as
   [Eio.Net.connect] says of every socket: the server's end then reads EOF and a
   write to the client that went away is a reset, rather than a read waiting on
   a peer nobody holds any more. *)
let test_connect_closes_at_switch_end () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8084) in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Eio.Switch.run (fun csw -> ignore (Eio.Net.connect ~sw:csw net addr));
  let flow, _ = Eio.Net.accept ~sw listener in
  match Eio.Flow.single_write flow [ Cstruct.of_string "late" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | (_ : int) ->
      Alcotest.fail
        "a write to a client whose switch finished was accepted: the client's \
         end is still open"

(* The two ends of one connection, used from two domains: a byte written in one
   wakes the read suspended on it in the other. The reader runs in this domain
   and the writer in a domain of its own. Each round the reader asks for one
   byte and, beside the read, waits for the writer's word that the byte is
   written. The writer wakes the read before it gives that word, and both wakes
   join this domain's run queue in that order, so a read still suspended once
   the word has arrived and the domain has run what was queued before it is a
   wake-up that was lost. Each round the writer starts a different number of
   spins after the reader asks, so the write falls at every point of the
   reader's path from finding nothing to suspending. *)
let test_a_write_in_another_domain_wakes_the_read () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let near, far = Eio_net_mem.pipe () in
  let rounds = 4096 in
  let turn = Atomic.make (-1) in
  let over = Atomic.make false in
  let written = Array.init rounds (fun _ -> Eio.Promise.create ()) in
  let byte i = Char.chr (i land 0xff) in
  let writer () =
    let rec round i =
      if i < rounds && not (Atomic.get over) then
        if Atomic.get turn < i then begin
          Domain.cpu_relax ();
          round i
        end
        else begin
          for _ = 1 to i land 63 do
            Domain.cpu_relax ()
          done;
          Eio.Flow.write far [ Cstruct.of_string (String.make 1 (byte i)) ];
          Eio.Promise.resolve (snd written.(i)) ();
          round (i + 1)
        end
    in
    round 0
  in
  let reader () =
    Fun.protect ~finally:(fun () -> Atomic.set over true) @@ fun () ->
    let buf = Cstruct.create 1 in
    for i = 0 to rounds - 1 do
      Atomic.set turn i;
      match
        Eio.Fiber.first
          (fun () ->
            ignore (Eio.Flow.single_read near buf : int);
            `Read)
          (fun () ->
            Eio.Promise.await (fst written.(i));
            Eio.Fiber.yield ();
            `Written)
      with
      | `Read ->
          Alcotest.(check char)
            (Fmt.str "round %d reads its own byte" i)
            (byte i) (Cstruct.get_char buf 0)
      | `Written ->
          Alcotest.failf
            "round %d: the byte written in the other domain did not wake the \
             read waiting for it"
            i
    done
  in
  Eio.Fiber.both reader (fun () -> Eio.Domain_manager.run domain_mgr writer)

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
      Alcotest.test_case "connect to wildcard listener" `Quick
        test_connect_to_wildcard_listener;
      Alcotest.test_case "listen on port 0 binds a free port" `Quick
        test_listen_on_port_zero;
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
      Alcotest.test_case "connect closes at switch end" `Quick
        test_connect_closes_at_switch_end;
      Alcotest.test_case "a write in another domain wakes the read" `Quick
        test_a_write_in_another_domain_wakes_the_read;
    ] )
