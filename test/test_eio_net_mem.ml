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

(* Each loopback name resolves to a stream and a datagram address, and
   "localhost" to IPv6 first, as glibc and macOS order it. *)
let test_resolve_localhost () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let v4 = Eio.Net.Ipaddr.V4.loopback and v6 = Eio.Net.Ipaddr.V6.loopback in
  let check what expected got =
    let str = List.map (Fmt.str "%a" Eio.Net.Sockaddr.pp) in
    Alcotest.(check (list string)) what (str expected) (str got)
  in
  check "localhost, streams"
    [ `Tcp (v6, 80); `Tcp (v4, 80) ]
    (Eio.Net.getaddrinfo_stream net "localhost" ~service:"80"
      :> Eio.Net.Sockaddr.t list);
  check "localhost, datagrams"
    [ `Udp (v6, 53); `Udp (v4, 53) ]
    (Eio.Net.getaddrinfo_datagram net "localhost" ~service:"53"
      :> Eio.Net.Sockaddr.t list);
  check "127.0.0.1, datagrams"
    [ `Udp (v4, 53) ]
    (Eio.Net.getaddrinfo_datagram net "127.0.0.1" ~service:"53"
      :> Eio.Net.Sockaddr.t list);
  check "::1, streams"
    [ `Tcp (v6, 80) ]
    (Eio.Net.getaddrinfo_stream net "::1" ~service:"80"
      :> Eio.Net.Sockaddr.t list);
  match Eio.Net.getaddrinfo net "example.org" ~service:"80" with
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed _), _) -> ()
  | _ -> Alcotest.fail "another host resolved"

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

(* A listen at an address a listener holds is refused with the Eio.Io error
   Address_in_use, unclassified as the EADDRINUSE that Eio_net_unix.Tcp.listen
   raises over a real network is; the listener there keeps the address. A
   wildcard address and another address of the same port overlap, as they do
   for bind(2). *)
let test_listen_on_an_address_in_use () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8085) in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  let in_use addr =
    match Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr with
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> ()
    | _ -> Alcotest.failf "listened twice at %a" Eio.Net.Sockaddr.pp addr
  in
  in_use addr;
  in_use (`Tcp (Eio.Net.Ipaddr.V4.any, 8085));
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Flow.copy_string "first" flow)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      let buf = Cstruct.create 5 in
      Eio.Flow.read_exact flow buf;
      Alcotest.(check string)
        "the first listener keeps the address" "first" (Cstruct.to_string buf))

(* A listener frees its address when it is closed, and when the switch passed
   to [listen] finishes: a connect there is then refused, and a new listen
   binds it. *)
let test_listener_frees_its_address () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8086) in
  let refused () =
    match Eio.Net.connect ~sw net addr with
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> ()
    | _ -> Alcotest.fail "a connect reached a listener that is gone"
  in
  Eio.Resource.close (Eio.Net.listen ~sw ~backlog:1 net addr);
  refused ();
  Eio.Switch.run (fun inner ->
      ignore (Eio.Net.listen ~sw:inner ~backlog:1 net addr));
  refused ();
  ignore (Eio.Net.listen ~sw ~backlog:1 net addr)

(* A Unix-domain path a listener holds is refused, unless the new listen asks
   for [reuse_addr]: Eio's POSIX backend then removes the socket file and binds
   a new one there, so connects reach the new listener. *)
let test_listen_on_a_unix_path_in_use () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Unix "/tmp/in-use.sock" in
  ignore (Eio.Net.listen ~sw ~backlog:1 net addr);
  (match Eio.Net.listen ~sw ~backlog:1 net addr with
  | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> ()
  | _ -> Alcotest.fail "listened twice at a Unix path");
  let second = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw second in
      Eio.Flow.copy_string "second" flow)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      let buf = Cstruct.create 6 in
      Eio.Flow.read_exact flow buf;
      Alcotest.(check string)
        "the path reaches the new listener" "second" (Cstruct.to_string buf))

(* Several listeners at one address would need a kernel to spread connections
   across them, so [reuse_port] is refused, as for datagram sockets. *)
let test_listen_refuses_reuse_port () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  match
    Eio.Net.listen ~sw ~backlog:1 ~reuse_port:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8087))
  with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "a listen with reuse_port was accepted"

let is_reset = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> true
  | _ -> false

let read_all flow =
  let buf = Cstruct.create 64 and got = Buffer.create 16 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | n ->
        Buffer.add_string got (Cstruct.to_string ~len:n buf);
        loop ()
    | exception e -> (Buffer.contents got, e)
  in
  loop ()

(* A write to a flow whose peer has closed is lost and answered with a reset,
   as a TCP peer answers it: the first write returns and the next write raises
   the [Eio.Io] reset. That write reports it, so reads return end of file, as
   on Linux. *)
let test_write_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.close server;
  Alcotest.(check int)
    "the first write returns" 4
    (Eio.Flow.single_write client [ Cstruct.of_string "ping" ]);
  (match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception e when is_reset e -> ()
  | (_ : int) -> Alcotest.fail "the second write went through");
  match read_all client with
  | "", End_of_file -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e

(* A write on a Unix-domain connection whose peer has closed raises the reset
   at once, as EPIPE, where a TCP connection loses the first one; reads return
   end of file. *)
let test_unix_write_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let listener = Eio.Net.listen ~sw ~backlog:1 net (`Unix "/tmp/epipe.sock") in
  let client = Eio.Net.connect ~sw net (`Unix "/tmp/epipe.sock") in
  let server, _ = Eio.Net.accept ~sw listener in
  Eio.Flow.close server;
  (match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception e when is_reset e -> ()
  | (_ : int) -> Alcotest.fail "the first write went through");
  match read_all client with
  | "", End_of_file -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e

(* An accepted connection's peer is the client's address: the loopback
   address of the family the listener takes it in, at a port of the dynamic
   range that no other connection holds; an IPv4 client of an IPv6 listener
   shows as IPv4-mapped. A Unix-domain client binds no path, as Eio's POSIX
   backend reports it. *)
let test_accepted_peer_address () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let peer listen_addr connect_ip =
    let l = Eio.Net.listen ~sw ~backlog:4 net listen_addr in
    let port =
      match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
    in
    let dst =
      match connect_ip with
      | Some ip -> `Tcp (ip, port)
      | None -> Eio.Net.listening_addr l
    in
    ignore (Eio.Net.connect ~sw net dst);
    snd (Eio.Net.accept ~sw l)
  in
  let show = function
    | `Tcp (ip, port) ->
        Fmt.str "%a %b" Eio.Net.Ipaddr.pp ip (port >= 49152 && port <= 65535)
    | `Unix path -> Fmt.str "unix %S" path
  in
  let v4 = Eio.Net.Ipaddr.V4.loopback in
  let a = peer (`Tcp (v4, 0)) None in
  let b = peer (`Tcp (v4, 0)) None in
  let c = peer (`Tcp (Eio.Net.Ipaddr.V6.any, 0)) (Some v4) in
  let d = peer (`Unix "/tmp/peer.sock") None in
  Alcotest.(check (list string))
    "peers"
    [ "127.0.0.1 true"; "127.0.0.1 true"; "::ffff:127.0.0.1 true"; "unix \"\"" ]
    (List.map show [ a; b; c; d ]);
  match (a, b) with
  | `Tcp (_, p), `Tcp (_, q) when p <> q -> ()
  | _ -> Alcotest.fail "two connections share a port"

(* A client's port is one no socket holds, so a listener at the top of the
   dynamic range never accepts a peer with its own address. *)
let test_client_port_is_free () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 65535) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  ignore (Eio.Net.connect ~sw net addr);
  match Eio.Net.accept ~sw l with
  | _, `Tcp (_, 65535) -> Alcotest.fail "the peer holds the listener's port"
  | _ -> ()

(* A Unix-domain listener closed by hand leaves its path until the switch it
   listened under finishes, as Eio's POSIX backend removes the file then. *)
let test_listener_path_freed_at_switch_end () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Unix "/tmp/hand.sock" in
  Eio.Switch.run (fun lsw ->
      Eio.Net.close (Eio.Net.listen ~sw:lsw ~backlog:1 net addr);
      match Eio.Net.listen ~sw:lsw ~backlog:1 net addr with
      | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> ()
      | _ -> Alcotest.fail "listened at a path whose file is still there");
  ignore (Eio.Net.listen ~sw ~backlog:1 net addr)

(* A connect bound to a local address holds it: the listener sees that
   address as the peer's, with a port of the dynamic range for port 0. *)
let test_connect_bound () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let v4 = Eio.Net.Ipaddr.V4.loopback in
  let l = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (v4, 9300)) in
  let peer bind_to =
    ignore (Eio.Net.connect ~sw ~bind_to net (`Tcp (v4, 9300)));
    match snd (Eio.Net.accept ~sw l) with
    | `Tcp (ip, port) -> Fmt.str "%a %b" Eio.Net.Ipaddr.pp ip (port = 9301)
    | `Unix path -> path
  in
  let given = peer (`Tcp (v4, 9301)) in
  let chosen = peer (`Tcp (Eio.Net.Ipaddr.V4.any, 0)) in
  Alcotest.(check (list string))
    "peers"
    [ "127.0.0.1 true"; "127.0.0.1 false" ]
    [ given; chosen ]

(* An empty write after the peer closed writes nothing and draws no reset:
   the next read returns end of file. *)
let test_empty_write_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.close server;
  Alcotest.(check int)
    "an empty write" 0
    (Eio.Flow.single_write client [ Cstruct.empty ]);
  match read_all client with
  | "", End_of_file -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e

(* A connection reset before it is read reports the reset to one read, and end
   of file to the reads after it, as a socket clears its pending error once it
   reports it. *)
let test_reset_read_once () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.copy_string "unread" client;
  Eio.Flow.close server;
  let read () =
    match Eio.Flow.single_read client (Cstruct.create 8) with
    | exception e when is_reset e -> "reset"
    | exception End_of_file -> "EOF"
    | n -> Fmt.str "%d bytes" n
  in
  let first = read () in
  let second = read () in
  let third = read () in
  Alcotest.(check (list string))
    "reads" [ "reset"; "EOF"; "EOF" ] [ first; second; third ]

(* A peer that shuts its receive side takes writes and drops them, as on
   Linux, and reads end-of-file itself. *)
let test_write_after_peer_shutdown_receive () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.shutdown server `Receive;
  let write () = Eio.Flow.single_write client [ Cstruct.of_string "ping" ] in
  Alcotest.(check (list int))
    "both writes return" [ 4; 4 ]
    [ write (); write () ];
  (match read_all server with
  | "", End_of_file -> ()
  | got, e -> Alcotest.failf "the server read %S then %a" got Fmt.exn e);
  Eio.Flow.copy_string "pong" server;
  Eio.Flow.shutdown server `Send;
  match read_all client with
  | "pong", End_of_file -> ()
  | got, e -> Alcotest.failf "the client read %S then %a" got Fmt.exn e

(* A peer that closes with bytes unread resets the connection: what it wrote
   before arrives, then the reset, and writes raise it at once. *)
let test_close_with_unread_bytes () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.copy_string "unread request" client;
  Eio.Flow.copy_string "bye" server;
  Eio.Flow.close server;
  (match read_all client with
  | "bye", e when is_reset e -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e);
  match Eio.Flow.single_write client [ Cstruct.of_string "more" ] with
  | exception e when is_reset e -> ()
  | (_ : int) -> Alcotest.fail "a write after the reset went through"

(* A flow used after its own close raises [Invalid_argument], as Eio's own
   sockets do, rather than read what was queued. *)
let test_use_after_own_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
  Eio.Flow.copy_string "queued" server;
  Eio.Flow.close client;
  (match Eio.Flow.single_read client (Cstruct.create 8) with
  | exception Invalid_argument _ -> ()
  | (_ : int) -> Alcotest.fail "read a closed flow");
  match Eio.Flow.single_write client [ Cstruct.of_string "x" ] with
  | exception Invalid_argument _ -> ()
  | (_ : int) -> Alcotest.fail "wrote to a closed flow"

(* A Unix path no socket binds has no file: a connect or a datagram sent there
   raises [Eio.Fs.Not_found], as over a real network. *)
let test_unix_path_not_found () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  (match Eio.Net.connect ~sw net (`Unix "/nowhere/stream.sock") with
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  | _ -> Alcotest.fail "connected to a missing path");
  let sock = Eio.Net.datagram_socket ~sw net (`Unix "/tmp/me.sock") in
  match
    Eio.Net.send sock ~dst:(`Unix "/nowhere/dgram.sock")
      [ Cstruct.of_string "x" ]
  with
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  | () -> Alcotest.fail "sent to a missing path"

(* With a capacity, a write takes what fits and waits while the queue is
   full, so a writer that ignores flow control shows. *)
(* A closed Unix-domain datagram socket leaves its file, as Eio's POSIX backend
   removes none: a send there is refused, and a bind there finds the address
   in use. *)
let test_closed_datagram_path () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let path = `Unix "/tmp/gone.sock" in
  Eio.Switch.run (fun inner ->
      ignore (Eio.Net.datagram_socket ~sw:inner net path));
  let sock = Eio.Net.datagram_socket ~sw net (`Unix "/tmp/sender.sock") in
  (match Eio.Net.send sock ~dst:path [ Cstruct.of_string "x" ] with
  | exception
      Eio.Io
        (Eio.Net.E (Connection_failure (Refused Eio_net_mem.No_listener)), _) ->
      ()
  | () -> Alcotest.fail "sent to a closed socket's path");
  match Eio.Net.datagram_socket ~sw net path with
  | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> ()
  | _ -> Alcotest.fail "bound a path whose file is still there"

let test_capacity () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe ~capacity:4 () in
  Alcotest.(check int)
    "a write takes what fits" 4
    (Eio.Flow.single_write client [ Cstruct.of_string "abcdef" ]);
  let blocked =
    Eio.Fiber.first
      (fun () ->
        ignore (Eio.Flow.single_write client [ Cstruct.of_string "ef" ] : int);
        false)
      (fun () ->
        Eio.Fiber.yield ();
        true)
  in
  Alcotest.(check bool) "a write to a full queue waits" true blocked;
  let got = ref "" in
  Eio.Fiber.both
    (fun () -> Eio.Flow.write client [ Cstruct.of_string "efgh" ])
    (fun () ->
      let buf = Cstruct.create 8 in
      Eio.Flow.read_exact server buf;
      got := Cstruct.to_string buf);
  Alcotest.(check string) "every byte arrives once" "abcdefgh" !got;
  match Eio_net_mem.pipe ~capacity:0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "a capacity of 0 was accepted"

(* A read or a write wakes one waiter, not every one: 8000 writers blocked on
   a queue of one byte, and 8000 readers on an empty one, finish with work, here
   counted as the words allocated, that grows with the number of waiters, not
   with its square. Waking all of them each time allocates well past the
   bound. *)
let test_capacity_wakes_one () =
  let n = 8000 in
  let words f =
    let before = Gc.minor_words () in
    Eio_mock.Backend.run f;
    Gc.minor_words () -. before
  in
  let writers () =
    let client, server = Eio_net_mem.pipe ~capacity:1 () in
    let one = Cstruct.of_string "x" in
    Eio.Fiber.both
      (fun () ->
        Eio.Fiber.all (List.init n (fun _ () -> Eio.Flow.write client [ one ]));
        Eio.Flow.shutdown client `Send)
      (fun () ->
        let buf = Cstruct.create 1 in
        try
          while true do
            ignore (Eio.Flow.single_read server buf : int)
          done
        with End_of_file -> ())
  in
  let readers () =
    let client, server = Eio_net_mem.pipe ~capacity:1 () in
    let one = Cstruct.of_string "x" in
    Eio.Fiber.both
      (fun () ->
        Eio.Fiber.all
          (List.init n (fun _ () ->
               ignore (Eio.Flow.single_read server (Cstruct.create 1) : int))))
      (fun () ->
        Eio.Fiber.yield ();
        for _ = 1 to n do
          Eio.Flow.write client [ one ]
        done)
  in
  let bound = 4000. *. float n in
  let check what f =
    let w = words f in
    if w > bound then
      Alcotest.failf "%s: %.0f words for %d waiters, past %.0f" what w n bound
  in
  check "writers" writers;
  check "readers" readers

(* Binding at port 0 costs the same however many sockets hold a port: 2000
   listeners and 2000 datagram sockets bind with work, counted as the words
   allocated, that grows with their number, not with its square. *)
let test_port_zero_scales () =
  let n = 2000 in
  let words f =
    let before = Gc.minor_words () in
    Eio_mock.Backend.run f;
    Gc.minor_words () -. before
  in
  let bound = 2000. *. float n in
  let check what f =
    let w = words f in
    if w > bound then
      Alcotest.failf "%s: %.0f words for %d binds, past %.0f" what w n bound
  in
  let v4 = Eio.Net.Ipaddr.V4.loopback in
  check "listeners" (fun () ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio_net_mem.v ~sw () in
      for _ = 1 to n do
        ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (v4, 0)))
      done);
  check "datagram sockets" (fun () ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio_net_mem.v ~sw () in
      for _ = 1 to n do
        ignore (Eio.Net.datagram_socket ~sw net (`Udp (v4, 0)))
      done)

(* A read or a write cancelled while it waits leaves nothing behind: 100000
   cancelled reads and 100000 cancelled writes keep the live heap within a
   bound that a waiter left queued by each would pass. *)
let test_cancelled_waits_free () =
  Eio_mock.Backend.run @@ fun () ->
  let live () =
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  let reader, _ = Eio_net_mem.pipe () in
  let writer, _ = Eio_net_mem.pipe ~capacity:1 () in
  Eio.Flow.write writer [ Cstruct.of_string "x" ];
  let buf = Cstruct.create 1 in
  let cancelled f = Eio.Fiber.first (fun () -> ignore (f ())) Eio.Fiber.yield in
  let waits n =
    for _ = 1 to n do
      cancelled (fun () -> Eio.Flow.single_read reader buf);
      cancelled (fun () -> Eio.Flow.single_write writer [ buf ])
    done
  in
  waits 1000;
  let before = live () in
  waits 100_000;
  let grown = live () - before in
  (* The pipes stay live while the heap is measured. *)
  ignore (Sys.opaque_identity (reader, writer));
  if grown > 50_000 then
    Alcotest.failf "200000 cancelled waits kept %d live words" grown

(* A writer that waits for room goes before a writer that comes after it,
   though the later one finds room first: the room a read frees is the first
   waiter's. *)
let test_capacity_first_come () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe ~capacity:1 () in
  Eio.Flow.write client [ Cstruct.of_string "x" ];
  let got = Buffer.create 3 in
  let read () =
    let buf = Cstruct.create 1 in
    let n = Eio.Flow.single_read server buf in
    Buffer.add_string got (Cstruct.to_string ~len:n buf)
  in
  Eio.Fiber.all
    [
      (fun () -> Eio.Flow.write client [ Cstruct.of_string "1" ]);
      (fun () ->
        Eio.Fiber.yield ();
        read ();
        Eio.Flow.write client [ Cstruct.of_string "2" ]);
      (fun () ->
        Eio.Fiber.yield ();
        Eio.Fiber.yield ();
        read ();
        read ());
    ];
  Alcotest.(check string)
    "bytes in the order the writers came" "x12" (Buffer.contents got)

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

(* [setsockopt] follows [connect]'s rule: the options whose effect the queues
   already have are accepted on a connection and on a listener, and every
   other one is refused by name, rather than ignored. A datagram socket has no
   such option, and refuses them all. *)
let test_setsockopt_follows_the_connect_rule () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let refused what set opt =
    match set () with
    | exception Invalid_argument msg ->
        Alcotest.(check string)
          (what ^ ": the refusal names the option")
          (Fmt.str "eio-net.mem does not support socket option %s" opt)
          msg
    | () -> Alcotest.failf "%s: %s was accepted and ignored" what opt
  in
  let flow, _ = Eio_net_mem.pipe () in
  Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_NODELAY true;
  Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_QUICKACK true;
  refused "a connection"
    (fun () -> Eio.Net.setsockopt flow Eio.Net.Sockopt.SO_KEEPALIVE true)
    "SO_KEEPALIVE";
  let listener =
    Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8088))
  in
  Eio.Net.setsockopt listener Eio.Net.Sockopt.TCP_NODELAY true;
  refused "a listener"
    (fun () -> Eio.Net.setsockopt listener Eio.Net.Sockopt.SO_RCVBUF 4096)
    "SO_RCVBUF";
  let datagram = Eio.Net.datagram_socket ~sw net `UdpV4 in
  refused "a datagram socket"
    (fun () -> Eio.Net.setsockopt datagram Eio.Net.Sockopt.TCP_NODELAY true)
    "TCP_NODELAY"

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
  (* The first write is lost and draws the reset the second raises. *)
  ignore (Eio.Flow.single_write flow [ Cstruct.of_string "late" ] : int);
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

(* A connection waiting to be accepted when its listener closes is reset with
   it, as a kernel resets it, rather than wait for good. *)
let test_a_closed_listener_resets_its_queue () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8089) in
  let client =
    Eio.Switch.run (fun lsw ->
        ignore (Eio.Net.listen ~sw:lsw ~backlog:4 net addr);
        Eio.Net.connect ~sw net addr)
  in
  match Eio.Flow.single_read client (Cstruct.create 1) with
  | exception e when is_reset e -> ()
  | exception e -> Alcotest.failf "a queued connection raised %a" Fmt.exn e
  | (_ : int) -> Alcotest.fail "a queued connection read a byte"

(* A connect waiting for room in a full queue when the listener closes is
   refused, as a connect to a closed port is, and so is a later one. *)
let test_a_closed_listener_refuses_connects () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8090) in
  let refused () =
    match Eio.Net.connect ~sw net addr with
    | exception
        Eio.Io
          (Eio.Net.E (Connection_failure (Refused Eio_net_mem.No_listener)), _)
      ->
        true
    | (_ : _ Eio.Net.stream_socket) -> false
  in
  let waiting = ref None in
  Eio.Switch.run (fun lsw ->
      let listener = Eio.Net.listen ~sw:lsw ~backlog:4 net addr in
      for _ = 1 to 128 do
        ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket)
      done;
      Eio.Fiber.fork ~sw (fun () -> waiting := Some (refused ()));
      Eio.Resource.close listener);
  Eio.Fiber.yield ();
  Alcotest.(check (option bool)) "the waiting connect" (Some true) !waiting;
  Alcotest.(check bool) "a later connect" true (refused ())

(* A connection releases its switch once it is closed: nothing of it stays
   reachable from the switch, however many a long-lived switch opens. *)
let test_a_closed_connection_leaves_its_switch () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8091) in
  let listener = Eio.Net.listen ~sw ~backlog:4 net addr in
  let held = Weak.create 2 in
  let () =
    let client = Eio.Net.connect ~sw net addr in
    let server, _ = Eio.Net.accept ~sw listener in
    Weak.set held 0 (Some client);
    Weak.set held 1 (Some server);
    Eio.Flow.close client;
    Eio.Flow.close server
  in
  Gc.full_major ();
  Alcotest.(check (list bool))
    "neither end is held" [ false; false ]
    [ Weak.check held 0; Weak.check held 1 ]

(* {1 Datagram sockets} *)

let udp port : Eio.Net.Sockaddr.datagram =
  `Udp (Eio.Net.Ipaddr.V4.loopback, port)

let datagram_addr_equal (a : Eio.Net.Sockaddr.datagram)
    (b : Eio.Net.Sockaddr.datagram) =
  match (a, b) with
  | `Udp (a_ip, a_port), `Udp (b_ip, b_port) ->
      String.equal (a_ip :> string) (b_ip :> string) && Int.equal a_port b_port
  | `Unix a, `Unix b -> String.equal a b
  | `Udp _, `Unix _ | `Unix _, `Udp _ -> false

let datagram_addr = Alcotest.testable Eio.Net.Sockaddr.pp datagram_addr_equal

let recv_string sock =
  let buf = Cstruct.create 2048 in
  let src, n = Eio.Net.recv sock buf in
  (src, Cstruct.to_string (Cstruct.sub buf 0 n))

(* A datagram longer than UDP can carry, 65507 bytes of payload over IPv4 and
   65527 over IPv6, or than Linux's default send buffer lets a Unix-domain
   socket send, is refused with the error that [Message_too_long] marks, as a
   socket refuses it with EMSGSIZE. *)
let test_datagram_too_long () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let v6 port : Eio.Net.Sockaddr.datagram =
    `Udp (Eio.Net.Ipaddr.V6.loopback, port)
  in
  let send sock dst n =
    match Eio.Net.send sock ~dst [ Cstruct.create n ] with
    | () -> "sent"
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Message_too_long, _) -> "EMSGSIZE"
  in
  let a4 = Eio.Net.datagram_socket ~sw net (udp 9100) in
  let a6 = Eio.Net.datagram_socket ~sw net (v6 9100) in
  let au = Eio.Net.datagram_socket ~sw net (`Unix "/tmp/long-a.sock") in
  ignore (Eio.Net.datagram_socket ~sw net (`Unix "/tmp/long-b.sock"));
  let b = `Unix "/tmp/long-b.sock" in
  let r4a = send a4 (udp 9101) 65507 in
  let r4b = send a4 (udp 9101) 65508 in
  let r6a = send a6 (v6 9101) 65527 in
  let r6b = send a6 (v6 9101) 65528 in
  let rua = send au b 212960 in
  let rub = send au b 212961 in
  Alcotest.(check (list string))
    "at and past each limit"
    [ "sent"; "EMSGSIZE"; "sent"; "EMSGSIZE"; "sent"; "EMSGSIZE" ]
    [ r4a; r4b; r6a; r6b; rua; rub ]

(* A datagram sent from an IPv6 socket to an IPv4-mapped IPv6 address reaches
   the socket bound at the IPv4 address it maps, one sent to a wildcard address reaches the
   socket bound at loopback, and one sent to an IPv4 address reaches an IPv6
   wildcard socket, as dual-stack kernels deliver them. *)
let test_datagram_reaches_like_a_kernel () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let mapped =
    Eio.Net.Ipaddr.of_raw
      "\000\000\000\000\000\000\000\000\000\000\xff\xff\127\000\000\001"
  in
  let v4 = Eio.Net.datagram_socket ~sw net (udp 9200) in
  let dual =
    Eio.Net.datagram_socket ~sw net (`Udp (Eio.Net.Ipaddr.V6.any, 9201))
  in
  let client = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let client6 =
    Eio.Net.datagram_socket ~sw net (`Udp (Eio.Net.Ipaddr.V6.any, 9202))
  in
  let send ?(from = client) dst s =
    Eio.Net.send from ~dst [ Cstruct.of_string s ]
  in
  send ~from:client6 (`Udp (mapped, 9200)) "mapped";
  send (`Udp (Eio.Net.Ipaddr.V4.any, 9200)) "wildcard";
  send (udp 9201) "dual";
  let first = snd (recv_string v4) in
  let second = snd (recv_string v4) in
  let third = snd (recv_string dual) in
  Alcotest.(check (list string))
    "delivered"
    [ "mapped"; "wildcard"; "dual" ]
    [ first; second; third ]

(* A datagram arrives whole with the address it was sent from, so a server
   answers whoever wrote to it, as over UDP. The client binds no address of its
   own and is given one. *)
let test_datagram_roundtrip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let server = Eio.Net.datagram_socket ~sw net (udp 9000) in
  let client = Eio.Net.datagram_socket ~sw net `UdpV4 in
  Eio.Net.send client ~dst:(udp 9000) [ Cstruct.of_string "ping" ];
  let src, got = recv_string server in
  Alcotest.(check string) "server read" "ping" got;
  Eio.Net.send server ~dst:src [ Cstruct.of_string "pong" ];
  let src, got = recv_string client in
  Alcotest.(check string) "client read" "pong" got;
  Alcotest.check datagram_addr "the answer comes from the server" (udp 9000) src

(* Datagrams from one socket to another arrive in the order they were sent,
   and none is dropped however many are sent before the first is read: the
   network is a queue, not a buffer of a fixed size. *)
let test_datagrams_keep_order () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let server = Eio.Net.datagram_socket ~sw net (udp 9001) in
  let client = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let n = 1000 in
  for i = 1 to n do
    Eio.Net.send client ~dst:(udp 9001) [ Cstruct.of_string (string_of_int i) ]
  done;
  let got = List.init n (fun _ -> snd (recv_string server)) in
  Alcotest.(check (list string))
    "every datagram, in order"
    (List.init n (fun i -> string_of_int (i + 1)))
    got

(* A datagram sent to an address no socket binds is lost, as UDP loses it,
   and the sender is not told. *)
let test_datagram_to_nobody_is_lost () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let client = Eio.Net.datagram_socket ~sw net `UdpV4 in
  Eio.Net.send client ~dst:(udp 9002) [ Cstruct.of_string "lost" ];
  let server = Eio.Net.datagram_socket ~sw net (udp 9002) in
  Eio.Net.send client ~dst:(udp 9002) [ Cstruct.of_string "kept" ];
  Alcotest.(check string)
    "only the later datagram" "kept"
    (snd (recv_string server))

(* A socket's address is free again once the switch it was made under ends. *)
let test_datagram_address_freed_at_switch_end () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  Eio.Switch.run (fun inner ->
      ignore (Eio.Net.datagram_socket ~sw:inner net (udp 9003)));
  let again = Eio.Net.datagram_socket ~sw net (udp 9003) in
  let client = Eio.Net.datagram_socket ~sw net `UdpV4 in
  Eio.Net.send client ~dst:(udp 9003) [ Cstruct.of_string "again" ];
  Alcotest.(check string)
    "the address binds again" "again"
    (snd (recv_string again))

(* A datagram socket at an address another holds is refused as a listen is,
   with the [Eio.Io] error {!Eio_net_mem.Address_in_use}; the wildcard address
   of a port overlaps every address of it. *)
let test_datagram_address_in_use () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let in_use addr =
    match Eio.Net.datagram_socket ~sw net addr with
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> true
    | (_ : _ Eio.Net.datagram_socket) -> false
  in
  ignore (Eio.Net.datagram_socket ~sw net (udp 9004));
  Alcotest.(check bool) "the same address" true (in_use (udp 9004));
  Alcotest.(check bool)
    "the wildcard address of its port" true
    (in_use (`Udp (Eio.Net.Ipaddr.V4.any, 9004)));
  ignore (Eio.Net.datagram_socket ~sw net (`Udp (Eio.Net.Ipaddr.V4.any, 9005)));
  Alcotest.(check bool)
    "an address of a wildcard's port" true
    (in_use (udp 9005));
  Alcotest.(check bool)
    "the same port of the other family" false
    (in_use (`Udp (Eio.Net.Ipaddr.V6.loopback, 9005)))

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
      Alcotest.test_case "connect to wildcard listener" `Quick
        test_connect_to_wildcard_listener;
      Alcotest.test_case "listen on port 0 binds a free port" `Quick
        test_listen_on_port_zero;
      Alcotest.test_case "listen on an address in use" `Quick
        test_listen_on_an_address_in_use;
      Alcotest.test_case "a listener frees its address" `Quick
        test_listener_frees_its_address;
      Alcotest.test_case "listen on a Unix path in use" `Quick
        test_listen_on_a_unix_path_in_use;
      Alcotest.test_case "listen refuses reuse_port" `Quick
        test_listen_refuses_reuse_port;
      Alcotest.test_case "write after peer close" `Quick
        test_write_after_peer_close;
      Alcotest.test_case "write after peer shuts its receive side" `Quick
        test_write_after_peer_shutdown_receive;
      Alcotest.test_case "close with unread bytes" `Quick
        test_close_with_unread_bytes;
      Alcotest.test_case "use after own close" `Quick test_use_after_own_close;
      Alcotest.test_case "a Unix path with no socket" `Quick
        test_unix_path_not_found;
      Alcotest.test_case "a capacity bounds the queue" `Quick test_capacity;
      Alcotest.test_case "write after own shutdown" `Quick
        test_write_after_own_shutdown;
      Alcotest.test_case "read after peer close" `Quick
        test_read_after_peer_close;
      Alcotest.test_case "connect ignores a latency hint" `Quick
        test_connect_ignores_latency_hint;
      Alcotest.test_case "connect refuses a semantic option" `Quick
        test_connect_refuses_semantic_option;
      Alcotest.test_case "setsockopt follows the connect rule" `Quick
        test_setsockopt_follows_the_connect_rule;
      Alcotest.test_case "connect closes at switch end" `Quick
        test_connect_closes_at_switch_end;
      Alcotest.test_case "a datagram roundtrip" `Quick test_datagram_roundtrip;
      Alcotest.test_case "datagrams keep their order" `Quick
        test_datagrams_keep_order;
      Alcotest.test_case "a datagram to nobody is lost" `Quick
        test_datagram_to_nobody_is_lost;
      Alcotest.test_case "a datagram address is freed at switch end" `Quick
        test_datagram_address_freed_at_switch_end;
      Alcotest.test_case "a datagram address in use" `Quick
        test_datagram_address_in_use;
      Alcotest.test_case "a closed listener resets its queue" `Quick
        test_a_closed_listener_resets_its_queue;
      Alcotest.test_case "a reset is read once" `Quick test_reset_read_once;
      Alcotest.test_case "a listener's path freed at switch end" `Quick
        test_listener_path_freed_at_switch_end;
      Alcotest.test_case "a client's port is free" `Quick
        test_client_port_is_free;
      Alcotest.test_case "a capacity serves writers first come first served"
        `Quick test_capacity_first_come;
      Alcotest.test_case "a cancelled wait leaves nothing behind" `Quick
        test_cancelled_waits_free;
      Alcotest.test_case "binding at port 0 scales" `Quick test_port_zero_scales;
      Alcotest.test_case "a capacity wakes one waiter" `Quick
        test_capacity_wakes_one;
      Alcotest.test_case "an empty write after peer close" `Quick
        test_empty_write_after_peer_close;
      Alcotest.test_case "a connect bound to an address" `Quick
        test_connect_bound;
      Alcotest.test_case "an accepted connection's peer" `Quick
        test_accepted_peer_address;
      Alcotest.test_case "a datagram too long" `Quick test_datagram_too_long;
      Alcotest.test_case "a datagram reaches as a kernel delivers it" `Quick
        test_datagram_reaches_like_a_kernel;
      Alcotest.test_case "a closed datagram socket's path" `Quick
        test_closed_datagram_path;
      Alcotest.test_case "a Unix-domain write after peer close" `Quick
        test_unix_write_after_peer_close;
      Alcotest.test_case "a closed listener refuses connects" `Quick
        test_a_closed_listener_refuses_connects;
      Alcotest.test_case "a closed connection leaves its switch" `Quick
        test_a_closed_connection_leaves_its_switch;
      Alcotest.test_case "a write in another domain wakes the read" `Quick
        test_a_write_in_another_domain_wakes_the_read;
    ] )
