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

(* Host and service resolution follows glibc's for numbers: a service is
   read as strtoul(3) reads it in base 10, kept to its low 32 bits, refused
   when that is negative and otherwise taken modulo 65536; a host is an IPv4
   address as inet_aton(3) reads it, an IPv6 one with a scope glibc 2.41
   takes, "localhost" in any case, or the empty name for the loopback
   addresses. The scopes are those glibc took on a host whose interfaces
   include lo, the only one here; a host with no IPv6 address but loopback
   hides them from the kernel's traces. *)
let test_resolve_numbers () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let resolve name service =
    match Eio.Net.getaddrinfo_stream net name ~service with
    | addrs ->
        String.concat " "
          (List.map
             (Fmt.str "%a" Eio.Net.Sockaddr.pp)
             (addrs :> Eio.Net.Sockaddr.t list))
    | exception Eio.Io (Eio.Net.E (Address_lookup_failed SERVICE), _) ->
        "SERVICE"
    | exception Eio.Io (Eio.Net.E (Address_lookup_failed NONAME), _) -> "NONAME"
  in
  List.iter
    (fun (service, expected) ->
      Alcotest.(check string)
        (Fmt.str "service %S" service)
        expected
        (resolve "127.0.0.1" service))
    [
      ("80", "tcp:127.0.0.1:80");
      ("", "tcp:127.0.0.1:0");
      ("65535", "tcp:127.0.0.1:65535");
      ("65536", "tcp:127.0.0.1:0");
      ("65617", "tcp:127.0.0.1:81");
      ("+80", "tcp:127.0.0.1:80");
      ("080", "tcp:127.0.0.1:80");
      (" 80", "tcp:127.0.0.1:80");
      ("\t7", "tcp:127.0.0.1:7");
      ("-0", "tcp:127.0.0.1:0");
      ("4294967296", "tcp:127.0.0.1:0");
      ("4294967295", "SERVICE");
      ("2147483648", "SERVICE");
      ("99999999999999999999", "SERVICE");
      ("-1", "SERVICE");
      ("0x50", "SERVICE");
      ("1_000", "SERVICE");
      ("0o17", "SERVICE");
      ("8 ", "SERVICE");
      ("+", "SERVICE");
      ("http", "SERVICE");
    ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        (Fmt.str "host %S" name) expected (resolve name "80"))
    [
      ("127.1", "tcp:127.0.0.1:80");
      ("0x7f.1", "tcp:127.0.0.1:80");
      ("2130706433", "tcp:127.0.0.1:80");
      ("10.1.2.3", "tcp:10.1.2.3:80");
      ("0.0.0.0", "tcp:0.0.0.0:80");
      ("::", "tcp:[::]:80");
      ("::ffff:127.0.0.1", "tcp:[::ffff:127.0.0.1]:80");
      ("LOCALHOST", "tcp:[::1]:80 tcp:127.0.0.1:80");
      ("localhost.", "NONAME");
      ("LocalHost.", "NONAME");
      ("", "tcp:[::1]:80 tcp:127.0.0.1:80");
      ("256.0.0.1", "NONAME");
      ("127.0.0.1 ", "NONAME");
      ("example.org", "NONAME");
      ("[::1]", "NONAME");
      ("0x", "NONAME");
      ("0x.1", "NONAME");
      ("1.0x", "NONAME");
      ("0x1.0x2", "tcp:1.0.0.2:80");
      ("fe80::1%lo", "tcp:[fe80::1]:80");
      ("fe80::1%1", "tcp:[fe80::1]:80");
      ("fe80::1%0", "tcp:[fe80::1]:80");
      ("fe80::1%01", "tcp:[fe80::1]:80");
      ("fe80::1%4294967295", "tcp:[fe80::1]:80");
      ("febf::1%lo", "tcp:[febf::1]:80");
      ("ff02::1%lo", "tcp:[ff02::1]:80");
      ("ff12::1%lo", "tcp:[ff12::1]:80");
      ("::1%1", "tcp:[::1]:80");
      ("::%1", "tcp:[::]:80");
      ("::ffff:127.0.0.1%1", "tcp:[::ffff:127.0.0.1]:80");
      ("fe80::1%4294967296", "NONAME");
      ("fe80::1%-1", "NONAME");
      ("fe80::1%+1", "NONAME");
      ("fe80::1% 1", "NONAME");
      ("fe80::1%1 ", "NONAME");
      ("fe80::1%", "NONAME");
      ("fe80::1%nosuch", "NONAME");
      ("fe80::1%LO", "NONAME");
      ("fe80::1%lo%lo", "NONAME");
      ("fec0::1%lo", "NONAME");
      ("ff05::1%lo", "NONAME");
      ("::1%lo", "NONAME");
      ("2001:db8::1%lo", "NONAME");
      ("::ffff:127.0.0.1%lo", "NONAME");
      ("127.0.0.1%1", "NONAME");
    ];
  Alcotest.(check string) "no host, no service" "NONAME" (resolve "" "");
  Alcotest.(check string)
    "a bad service first" "SERVICE" (resolve "nowhere" "-1")

(* getnameinfo names the loopback addresses "localhost", as /etc/hosts does,
   127.0.0.1 IPv4-mapped too, and gives any other address as a number; a
   service stays a number, as there is no services database, and a Unix
   address's host is "localhost", as there is no host name. *)
let test_name_info () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = Eio.Net.Ipaddr.of_raw in
  List.iter
    (fun (addr, expected) ->
      let host, service = Eio.Net.getnameinfo net addr in
      Alcotest.(check string)
        (Fmt.str "%a" Eio.Net.Sockaddr.pp addr)
        expected
        (host ^ "/" ^ service))
    [
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 80), "localhost/80");
      (`Udp (Eio.Net.Ipaddr.V6.loopback, 53), "localhost/53");
      (`Tcp (ip "\010\001\002\003", 80), "10.1.2.3/80");
      (`Tcp (ip "\127\000\000\002", 8080), "127.0.0.2/8080");
      ( `Tcp
          ( ip "\000\000\000\000\000\000\000\000\000\000\255\255\127\000\000\001",
            80 ),
        "localhost/80" );
      ( `Tcp
          ( ip "\000\000\000\000\000\000\000\000\000\000\255\255\010\001\002\003",
            80 ),
        "::ffff:10.1.2.3/80" );
      (`Unix "x.sock", "localhost/x.sock");
    ]

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

(* A listen at an address a listener holds is refused with the raw
   EADDRINUSE eio_posix raises; the listener there keeps the address. A
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
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
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
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
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
  let client, server = Eio_net_mem.pair () in
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
   listened under finishes, as eio_linux removes the file then; eio_posix
   keeps it. *)
let test_listener_path_freed_at_switch_end () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Unix "/tmp/hand.sock" in
  Eio.Switch.run (fun lsw ->
      Eio.Net.close (Eio.Net.listen ~sw:lsw ~backlog:1 net addr);
      match Eio.Net.listen ~sw:lsw ~backlog:1 net addr with
      | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
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
  let client, server = Eio_net_mem.pair () in
  Eio.Flow.close server;
  Alcotest.(check int)
    "an empty write" 0
    (Eio.Flow.single_write client [ Cstruct.empty ]);
  match read_all client with
  | "", End_of_file -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e

(* A TCP write after this end's own send shutdown reports a pending reset, as
   Linux reports its pending error before EPIPE: a read after it returns end
   of file. *)
let test_write_after_shutdown_reports_reset () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8090) in
  let client =
    Eio.Switch.run (fun lsw ->
        ignore (Eio.Net.listen ~sw:lsw ~backlog:4 net addr);
        Eio.Net.connect ~sw net addr)
  in
  Eio.Flow.shutdown client `All;
  (match Eio.Flow.single_write client [ Cstruct.of_string "ab" ] with
  | exception e when is_reset e -> ()
  | (_ : int) -> Alcotest.fail "the write went through");
  match Eio.Flow.single_read client (Cstruct.create 1) with
  | exception End_of_file -> ()
  | (_ : int) -> Alcotest.fail "the read returned bytes"

(* A connection reset before it is read reports the reset to one read, and end
   of file to the reads after it, as a socket clears its pending error once it
   reports it. *)
let test_reset_read_once () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
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

(* A peer that shuts its receive side still reads what is written to it, as on
   Linux, then end-of-file once nothing is queued. *)
let test_write_after_peer_shutdown_receive () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
  Eio.Flow.shutdown server `Receive;
  let write () = Eio.Flow.single_write client [ Cstruct.of_string "ping" ] in
  Alcotest.(check (list int))
    "both writes return" [ 4; 4 ]
    [ write (); write () ];
  (match read_all server with
  | "pingping", End_of_file -> ()
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
  let client, server = Eio_net_mem.pair () in
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
  let client, server = Eio_net_mem.pair () in
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
let test_capacity () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair ~capacity:4 () in
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
  Alcotest.check_raises "a capacity of 0 is refused by pair"
    (Invalid_argument "Eio_net_mem.pair: capacity is 0, it must be positive")
    (fun () -> ignore (Eio_net_mem.pair ~capacity:0 ()))

(* By default a connection queues at most 4 MiB each way over TCP and
   256 KiB over a Unix path, as Linux's default buffers hold no more: a
   writer whose reader does not read then waits. *)
let test_default_capacity () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let queued addr bound =
    let l = Eio.Net.listen ~sw ~backlog:1 net addr in
    let client = Eio.Net.connect ~sw net addr in
    let _ = Eio.Net.accept ~sw l in
    let chunk = Cstruct.create 65536 and total = ref 0 in
    let blocked =
      Eio.Fiber.first
        (fun () ->
          while !total <= 4 * bound do
            total := !total + Eio.Flow.single_write client [ chunk ]
          done;
          false)
        (fun () ->
          Eio.Fiber.yield ();
          true)
    in
    Alcotest.(check bool) "the writer waits" true blocked;
    Alcotest.(check int) "the bytes queued" bound !total
  in
  queued (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)) (4 * 1024 * 1024);
  queued (`Unix "/tmp/queued.sock") (256 * 1024)

(* A capacity above the kernel's default is what a connection queues, over
   TCP and over a Unix path, under either kernel. *)
let test_large_capacity () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let capacity = 8 * 1024 * 1024 in
  let queued os addr =
    let net = Eio_net_mem.v ~os ~capacity ~sw () in
    let l = Eio.Net.listen ~sw ~backlog:1 net addr in
    let client = Eio.Net.connect ~sw net (Eio.Net.listening_addr l) in
    let _ = Eio.Net.accept ~sw l in
    let n = Eio.Flow.single_write client [ Cstruct.create (capacity + 1) ] in
    Alcotest.(check int) "the bytes queued" capacity n
  in
  List.iter
    (fun os ->
      queued os (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0));
      queued os (`Unix "/tmp/large.sock"))
    [ `Linux; `Macos ];
  let client, _server = Eio_net_mem.pair ~os:`Macos ~capacity () in
  let n = Eio.Flow.single_write client [ Cstruct.create (capacity + 1) ] in
  Alcotest.(check int) "a pipe queues its capacity" capacity n

(* Under [`Macos] a connection queues what macOS's buffers hold: at least
   the 1 MiB its TCP autotuning grows to, and AF_UNIX's 8 KiB. *)
let test_macos_capacity () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~os:`Macos ~sw () in
  let queued addr bound =
    let l = Eio.Net.listen ~sw ~backlog:1 net addr in
    let client = Eio.Net.connect ~sw net (Eio.Net.listening_addr l) in
    let _ = Eio.Net.accept ~sw l in
    let chunk = Cstruct.create 4096 and total = ref 0 in
    let blocked =
      Eio.Fiber.first
        (fun () ->
          while !total <= 4 * bound do
            total := !total + Eio.Flow.single_write client [ chunk ]
          done;
          false)
        (fun () ->
          Eio.Fiber.yield ();
          true)
    in
    Alcotest.(check bool) "the writer waits" true blocked;
    Alcotest.(check int) "the bytes queued" bound !total
  in
  queued (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) (1024 * 1024);
  queued (`Unix "/tmp/queued-macos.sock") 8192;
  let client, _server = Eio_net_mem.pair ~os:`Macos () in
  let n = Eio.Flow.single_write client [ Cstruct.create (2 * 1024 * 1024) ] in
  Alcotest.(check int) "a pipe queues as TCP" (1024 * 1024) n

(* Under [`Macos] a connect to an address of 127.0.0.0/8 other than
   127.0.0.1 is never answered, and times out after 75 seconds on the
   clock with eio_posix's error; macOS's TCP ports count up, shared by
   listeners and clients. *)
let test_macos_connect () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let net = Eio_net_mem.v ~os:`Macos ~mono:clock ~sw () in
  let port l =
    match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let l1 = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  let l2 = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  Alcotest.(check (list int))
    "listeners count up" [ 49152; 49153 ]
    [ port l1; port l2 ];
  let t0 = Eio.Time.Mono.now clock in
  let ip = Eio.Net.Ipaddr.of_raw "\127\000\000\002" in
  (match Eio.Net.connect ~sw net (`Tcp (ip, port l1)) with
  | _ -> Alcotest.fail "the connect returned"
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, _, _)), _)
    ->
      ());
  let waited = Mtime.span t0 (Eio.Time.Mono.now clock) in
  Alcotest.(check bool)
    "after 75 s" true
    (Mtime.Span.equal waited Mtime.Span.(75 * s))

(* A TCP connection that goes silent [after] seconds past its last byte, its
   client set up by [setup]: how the client's [op] ends, and when, in seconds
   past that byte, then what a read, a write and a read do. The times are
   those test/interop/silent-peer measures over loopback: on Linux with a
   peer that sends through a source route the client drops, and on macOS
   with a peer that sends through an interface that is down. *)
let silent ?(os = `Linux) ?(after = 0.5) ?(setup = ignore) op =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let net = Eio_net_mem.v ~os ~capacity:4096 ~mono:clock ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c = Eio.Net.connect ~sw net addr in
  let s, _ = Eio.Net.accept ~sw l in
  setup c;
  let byte = Cstruct.create 1 in
  Eio.Flow.copy_string "c" c;
  Eio.Flow.read_exact s byte;
  Eio.Flow.copy_string "s" s;
  Eio.Flow.read_exact c byte;
  let t0 = Eio.Time.Mono.now clock in
  Eio.Time.Mono.sleep clock after;
  Eio_net_mem.Private.silence c;
  let ended f =
    match Eio.Time.Timeout.(run_exn (seconds clock 3600.)) f with
    | () -> "returned"
    | exception Eio.Time.Timeout -> "waits"
    | exception End_of_file -> "eof"
    | exception
        Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, call, _)), _) ->
        "ETIMEDOUT " ^ call
    | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> "reset"
  in
  let first = ended (fun () -> op c) in
  let at = Mtime.span t0 (Eio.Time.Mono.now clock) in
  let buf = Cstruct.create 16 in
  let read () = ignore (Eio.Flow.single_read c buf : int) in
  let write () = Eio.Flow.copy_string "x" c in
  if first = "waits" then first
  else
    let r1 = ended read in
    let w = ended write in
    let r2 = ended read in
    Fmt.str "%s at %gs, then %s, %s, %s" first
      (Mtime.Span.to_float_ns at /. 1e9)
      r1 w r2

let read c = ignore (Eio.Flow.single_read c (Cstruct.create 16) : int)

let fill c =
  while true do
    Eio.Flow.copy_string (String.make 1024 'x') c
  done

let write_read c =
  Eio.Flow.copy_string (String.make 100 'x') c;
  read c

let keepalive ?user_timeout (idle, intvl, cnt) c =
  let module O = Eio.Net.Sockopt in
  Eio.Net.setsockopt c O.SO_KEEPALIVE true;
  Eio.Net.setsockopt c O.TCP_KEEPIDLE idle;
  Eio.Net.setsockopt c O.TCP_KEEPINTVL intvl;
  Eio.Net.setsockopt c O.TCP_KEEPCNT cnt;
  Option.iter (Eio.Net.setsockopt c O.TCP_USER_TIMEOUT) user_timeout

let user_timeout ms c = Eio.Net.setsockopt c Eio.Net.Sockopt.TCP_USER_TIMEOUT ms

(* Linux: with nothing unacknowledged, keepalive probes once the idle time has
   passed since the last segment received, a probe the live peer answered
   included, then gives up after the count of probes with ETIMEDOUT; with
   TCP_USER_TIMEOUT, at the first probe that time past that segment. Data
   unacknowledged stops the probes: the connection then fails once it has
   waited TCP_USER_TIMEOUT, or 948.6 s, as tcp_retries2 = 15 does on
   loopback. The first read or write reports the error, then reads end and
   writes raise EPIPE. With nothing to send and no keepalive, a read waits for
   ever. *)
let test_silent_linux () =
  let check name expected got = Alcotest.(check string) name expected got in
  let ends = "then eof, reset, eof" in
  check "keepalive"
    ("ETIMEDOUT readv at 5s, " ^ ends)
    (silent ~setup:(keepalive (2, 1, 3)) read);
  check "keepalive, answered once"
    ("ETIMEDOUT readv at 7s, " ^ ends)
    (silent ~after:3.5 ~setup:(keepalive (2, 1, 3)) read);
  check "keepalive and a user timeout"
    ("ETIMEDOUT readv at 3s, " ^ ends)
    (silent ~setup:(keepalive ~user_timeout:3000 (2, 1, 3)) read);
  check "keepalive and a longer user timeout"
    ("ETIMEDOUT readv at 4s, " ^ ends)
    (silent ~setup:(keepalive ~user_timeout:3500 (2, 1, 10)) read);
  check "a user timeout"
    ("ETIMEDOUT readv at 3.5s, " ^ ends)
    (silent ~setup:(user_timeout 3000) write_read);
  check "a user timeout, nothing sent" "waits"
    (silent ~setup:(user_timeout 3000) read);
  check "a blocked writer" ("ETIMEDOUT writev at 949.1s, " ^ ends) (silent fill);
  check "a blocked writer with keepalive"
    ("ETIMEDOUT writev at 949.1s, " ^ ends)
    (silent ~setup:(keepalive (2, 1, 3)) fill);
  check "data unacknowledged"
    ("ETIMEDOUT readv at 949.1s, " ^ ends)
    (silent write_read);
  check "nothing" "waits" (silent read)

(* macOS: keepalive gives up as on Linux, data unacknowledged or not, and data
   unacknowledged for 18 s fails the connection. A write raises EPIPE and
   leaves ETIMEDOUT to the next read. *)
let test_silent_macos () =
  let check name expected got = Alcotest.(check string) name expected got in
  let os = `Macos in
  check "keepalive" "ETIMEDOUT readv at 5s, then eof, reset, eof"
    (silent ~os ~setup:(keepalive (2, 1, 3)) read);
  check "keepalive, answered once" "ETIMEDOUT readv at 7s, then eof, reset, eof"
    (silent ~os ~after:3.5 ~setup:(keepalive (2, 1, 3)) read);
  check "a blocked writer" "reset at 18.5s, then ETIMEDOUT readv, reset, eof"
    (silent ~os fill);
  check "a blocked writer with keepalive"
    "reset at 5s, then ETIMEDOUT readv, reset, eof"
    (silent ~os ~setup:(keepalive (2, 1, 3)) fill);
  check "data unacknowledged" "ETIMEDOUT readv at 18.5s, then eof, reset, eof"
    (silent ~os write_read);
  check "nothing" "waits" (silent ~os read)

(* Each end gives up on its own: the peer of an end that timed out still
   waits. *)
let test_silent_peer_waits () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let net = Eio_net_mem.v ~mono:clock ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c = Eio.Net.connect ~sw net addr in
  let s, _ = Eio.Net.accept ~sw l in
  keepalive (1, 1, 1) c;
  Eio_net_mem.Private.silence c;
  let ended f =
    match Eio.Time.Timeout.(run_exn (seconds clock 60.)) f with
    | () -> "returned"
    | exception Eio.Time.Timeout -> "waits"
    | exception Eio.Io _ -> "failed"
  in
  Alcotest.(check (pair string string))
    "client, server" ("failed", "waits")
    (Eio.Fiber.pair
       (fun () -> ended (fun () -> read c))
       (fun () -> ended (fun () -> read s)))

(* Under [`Macos] a Unix-domain address that starts with a NUL names no
   file, the empty path binds nothing, and getnameinfo names no Unix
   address. *)
let test_macos_unix () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~os:`Macos ~sw () in
  let raised f =
    match f () with
    | _ -> "ok"
    | exception Unix.Unix_error (e, _, _) -> Unix.error_message e
    | exception Not_found -> "Not_found"
    | exception Eio.Io _ -> "io"
  in
  Alcotest.(check string)
    "abstract"
    (Unix.error_message Unix.ENOENT)
    (raised (fun () -> Eio.Net.listen ~sw ~backlog:1 net (`Unix "\000x")));
  Alcotest.(check string)
    "empty"
    (Unix.error_message Unix.EINVAL)
    (raised (fun () -> Eio.Net.listen ~sw ~backlog:1 net (`Unix "")));
  Alcotest.(check string)
    "getnameinfo" "Not_found"
    (raised (fun () -> Eio.Net.getnameinfo net (`Unix "x.sock")))

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
    let client, server = Eio_net_mem.pair ~capacity:1 () in
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
    let client, server = Eio_net_mem.pair ~capacity:1 () in
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

(* Connections the server closes first leave their ends in TIME_WAIT on the
   listener's port: 4000 of them connect with work, counted as the words
   allocated, that grows with their number, not with its square. *)
let test_time_wait_scales () =
  let words n =
    let before = Gc.minor_words () in
    ( Eio_mock.Backend.run @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio_mock.Clock.Mono.make () in
      let net = Eio_net_mem.v ~mono:clock ~sw () in
      let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
      let l = Eio.Net.listen ~sw ~backlog:5 net addr in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Net.run_server l ~on_error:ignore (fun _ _ -> ()));
      for _ = 1 to n do
        Eio.Switch.run @@ fun sw ->
        ignore (Eio.Flow.read_all (Eio.Net.connect ~sw net addr) : string)
      done );
    Gc.minor_words () -. before
  in
  let one = words 1000 and four = words 4000 in
  if four > 8. *. one then
    Alcotest.failf "%.0f words for 4000 connections, %.0f for 1000" four one

(* A read or a write cancelled while it waits leaves nothing behind: 100000
   cancelled reads and 100000 cancelled writes keep the live heap within a
   bound that a waiter left queued by each would pass. *)
let test_cancelled_waits_free () =
  Eio_mock.Backend.run @@ fun () ->
  let live () =
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  let reader, _ = Eio_net_mem.pair () in
  let writer, _ = Eio_net_mem.pair ~capacity:1 () in
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
  let client, server = Eio_net_mem.pair ~capacity:1 () in
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

(* The races across domains run a few hundred times by default;
   [dune build @test/conc-long] sets EIO_NET_LONG for the full counts. *)
let long = Sys.getenv_opt "EIO_NET_LONG" <> None
let iterations = if long then 2000 else 200

(* Runs [f i] for [i] from 1 to [n] while another domain takes the network's
   mutex over and over, listening and closing, so that a fiber cancelled on
   its way into the mutex meets it held; then checks that [port] binds
   again. *)
let contended ~port n f =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let stop = Atomic.make false in
  Eio.Fiber.both
    (fun () ->
      Eio.Domain_manager.run domain_mgr (fun () ->
          while not (Atomic.get stop) do
            Eio.Fiber.yield ();
            Eio.Switch.run (fun sw ->
                ignore
                  (Eio.Net.listen ~sw ~backlog:1 net
                     (`Tcp (Eio.Net.Ipaddr.V6.loopback, 0))))
          done))
    (fun () ->
      Fun.protect
        ~finally:(fun () -> Atomic.set stop true)
        (fun () ->
          for i = 1 to n do
            f ~sw net i
          done));
  match
    Eio.Switch.run (fun sw ->
        Eio.Net.listen ~sw ~backlog:1 net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)))
  with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Alcotest.failf "port %d is still held after %d cancelled fibers" port n

(* A connect cancelled while it releases its bound port releases it all the
   same: the port is free again after [iterations] such connects. Each listener
   closes with its switch and resets the connection it queued, which lets go
   of the client's port. *)
let test_cancelled_connect_frees_its_port () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let bind_to = `Tcp (Eio.Net.Ipaddr.V4.loopback, 40000) in
  let connect net =
    Eio.Switch.run @@ fun csw ->
    let l = Eio.Net.listen ~sw:csw ~backlog:1 net addr in
    ignore (Eio.Net.connect ~sw:csw ~bind_to net (Eio.Net.listening_addr l))
  in
  contended ~port:40000 iterations (fun ~sw:_ net i ->
      Eio.Fiber.first
        (fun () -> connect net)
        (fun () ->
          for _ = 1 to i mod 3 do
            Eio.Fiber.yield ()
          done))

(* A listener closed by a cancelled fiber frees its port all the same: the
   address binds again after [iterations] such listeners. *)
let test_cancelled_close_frees_its_port () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 41000) in
  contended ~port:41000 iterations (fun ~sw:_ net i ->
      Eio.Fiber.first
        (fun () ->
          Eio.Switch.run @@ fun sw ->
          let l = Eio.Net.listen ~sw ~backlog:1 net addr in
          Fun.protect
            ~finally:(fun () -> Eio.Net.close l)
            Eio.Fiber.await_cancel)
        (fun () ->
          for _ = 1 to 1 + (i mod 3) do
            Eio.Fiber.yield ()
          done))

(* A read into an empty buffer raises [End_of_file], as Eio's backends turn
   the 0 that read(2) returns then, since Eio's flows never return 0. *)
let test_empty_read () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
  Eio.Flow.copy_string "queued" server;
  match Eio.Flow.single_read client (Cstruct.create 0) with
  | exception End_of_file -> ()
  | n -> Alcotest.failf "an empty read returned %d" n

(* A close defers to the reads and writes in flight, as Eio's backends defer
   a descriptor's close to its users: a read waiting across its own flow's
   close still gets what the peer writes after, and only a later read raises
   [Invalid_argument]. The socket then closes and the peer reads end-of-file. *)
let test_close_defers_to_a_read () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
  let buf = Cstruct.create 4 in
  Eio.Fiber.both
    (fun () ->
      Alcotest.(check int)
        "the read in flight" 1
        (Eio.Flow.single_read client buf);
      match Eio.Flow.single_read client buf with
      | exception Invalid_argument _ -> ()
      | (_ : int) -> Alcotest.fail "read after close")
    (fun () ->
      Eio.Fiber.yield ();
      Eio.Flow.close client;
      Eio.Fiber.yield ();
      Eio.Flow.copy_string "z" server);
  match Eio.Flow.single_read server buf with
  | exception End_of_file -> ()
  | (_ : int) -> Alcotest.fail "the peer read data"

(* A write waiting for room across its own flow's close finishes against the
   open socket, as a write in flight does under Eio's deferred close. *)
let test_close_defers_to_a_write () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair ~capacity:1 () in
  Eio.Flow.copy_string "a" client;
  Eio.Fiber.both
    (fun () ->
      Alcotest.(check int)
        "the write in flight" 1
        (Eio.Flow.single_write client [ Cstruct.of_string "b" ]))
    (fun () ->
      Eio.Fiber.yield ();
      Eio.Flow.close client;
      let buf = Cstruct.create 1 in
      Eio.Flow.read_exact server buf);
  let got = read_all server in
  match got with
  | "b", End_of_file -> ()
  | got, e -> Alcotest.failf "read %S then %a" got Fmt.exn e

let test_write_after_own_shutdown () =
  Eio_mock.Backend.run @@ fun () ->
  let client, _server = Eio_net_mem.pair () in
  Eio.Flow.shutdown client `Send;
  match Eio.Flow.single_write client [ Cstruct.of_string "ping" ] with
  | exception Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
  | exception Failure msg ->
      Alcotest.failf "write after own shutdown leaked Failure %S" msg
  | (_ : int) -> Alcotest.fail "write after own shutdown should raise"

(* A write to a full queue whose reader shut down both ways waits, as Linux
   holds data its peer's closed window refuses, and keeps waiting once the
   reader reads, as Linux then advertises no room; the reader's close ends
   it. *)
let test_full_write_after_peer_shutdown_all () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair ~capacity:1 () in
  Eio.Flow.copy_string "a" client;
  Eio.Flow.shutdown server `All;
  let blocked = ref true in
  Eio.Fiber.both
    (fun () ->
      (try ignore (Eio.Flow.single_write client [ Cstruct.of_string "b" ])
       with e when is_reset e -> ());
      blocked := false)
    (fun () ->
      Eio.Fiber.yield ();
      Alcotest.(check bool) "the write waits" true !blocked;
      let buf = Cstruct.create 1 in
      Eio.Flow.read_exact server buf;
      Eio.Fiber.yield ();
      Alcotest.(check bool) "a read frees no room" true !blocked;
      Eio.Flow.close server;
      Eio.Fiber.yield ();
      Alcotest.(check bool) "the close ends the write" false !blocked)

let test_read_after_peer_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
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

(* An option set at a connect is read back on the connection. *)
let test_connect_options () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8083) in
  let _listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  let flow =
    Eio.Net.connect ~sw
      ~options:Eio.Net.Sockopt.[ (TCP_NODELAY, true); (SO_KEEPALIVE, true) ]
      net addr
  in
  Alcotest.(check bool)
    "SO_KEEPALIVE" true
    (Eio.Net.getsockopt flow Eio.Net.Sockopt.SO_KEEPALIVE)

let invalid_option f =
  match f () with
  | exception Eio.Io (Eio.Net.E Invalid_option, _) -> ()
  | _ -> Alcotest.fail "expected Invalid_option"

(* A socket reads back what was set, and the kernel's default otherwise:
   Linux doubles a buffer size, macOS keeps it. An option the socket's kernel
   lacks for its family raises what eio_posix raises. *)
let test_sockopt_read_back () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let open Eio.Net.Sockopt in
  let net = Eio_net_mem.v ~sw () in
  let flow, _ = Eio_net_mem.pair () in
  Alcotest.(check bool)
    "SO_KEEPALIVE default" false
    (Eio.Net.getsockopt flow SO_KEEPALIVE);
  Eio.Net.setsockopt flow SO_KEEPALIVE true;
  Alcotest.(check bool)
    "SO_KEEPALIVE" true
    (Eio.Net.getsockopt flow SO_KEEPALIVE);
  Alcotest.(check int)
    "SO_RCVBUF default" 131072
    (Eio.Net.getsockopt flow SO_RCVBUF);
  Eio.Net.setsockopt flow SO_SNDBUF 10000;
  Alcotest.(check int)
    "SO_SNDBUF on Linux" 20000
    (Eio.Net.getsockopt flow SO_SNDBUF);
  let macos, _ = Eio_net_mem.pair ~os:`Macos () in
  Eio.Net.setsockopt macos SO_SNDBUF 10000;
  Alcotest.(check int)
    "SO_SNDBUF on macOS" 10000
    (Eio.Net.getsockopt macos SO_SNDBUF);
  invalid_option (fun () -> Eio.Net.setsockopt macos TCP_CORK true);
  let listener =
    Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8088))
  in
  Eio.Net.setsockopt listener TCP_DEFER_ACCEPT 5;
  Alcotest.(check int)
    "TCP_DEFER_ACCEPT, in retransmissions" 7
    (Eio.Net.getsockopt listener TCP_DEFER_ACCEPT);
  let datagram = Eio.Net.datagram_socket ~sw net `UdpV4 in
  invalid_option (fun () -> Eio.Net.setsockopt datagram TCP_NODELAY true)

(* The send and receive buffers bound what a connection queues: a writer to
   a peer that does not read waits sooner. *)
let test_buffers_bound_the_queue () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
  Eio.Net.setsockopt client Eio.Net.Sockopt.SO_SNDBUF 2000;
  Eio.Net.setsockopt server Eio.Net.Sockopt.SO_RCVBUF 2000;
  let queued = ref 0 in
  let chunk = Cstruct.create 1024 in
  Eio.Fiber.first
    (fun () ->
      while true do
        queued := !queued + Eio.Flow.single_write client [ chunk ]
      done)
    (fun () ->
      for _ = 1 to 64 do
        Eio.Fiber.yield ()
      done);
  Alcotest.(check int) "32 KiB in flight and half the window" 34768 !queued

(* A TCP socket that lingers for no time resets its peer as it closes. *)
let test_linger_zero_resets () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pair () in
  Eio.Net.setsockopt client Eio.Net.Sockopt.SO_LINGER (Some 0);
  Eio.Resource.close client;
  match Eio.Flow.single_read server (Cstruct.create 1) with
  | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ()
  | _ -> Alcotest.fail "expected a reset"

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
   spins after the reader asks, 0 to 63, so the write falls at every point of
   the reader's path from finding nothing to suspending: 256 rounds try each
   four times, 4096 under [@conc-long]. *)
let test_a_write_in_another_domain_wakes_the_read () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let near, far = Eio_net_mem.pair () in
  let rounds = if long then 4096 else 256 in
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
      for _ = 1 to 5 do
        ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket)
      done;
      Eio.Fiber.fork ~sw (fun () -> waiting := Some (refused ()));
      Eio.Resource.close listener);
  Eio.Fiber.yield ();
  Alcotest.(check (option bool)) "the waiting connect" (Some true) !waiting;
  Alcotest.(check bool) "a later connect" true (refused ())

(* A connect under a finished switch fails before it reaches the listener,
   as [listen] does: no connection waits there to be accepted. *)
let test_connect_under_a_finished_switch () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8091) in
  let listener = Eio.Net.listen ~sw ~backlog:4 net addr in
  let finished = Eio.Switch.run Fun.id in
  (match Eio.Net.connect ~sw:finished net addr with
  | exception Invalid_argument _ -> ()
  | (_ : _ Eio.Net.stream_socket) -> Alcotest.fail "connected");
  let accepted =
    Eio.Fiber.first
      (fun () ->
        ignore (Eio.Net.accept ~sw listener);
        true)
      (fun () ->
        Eio.Fiber.yield ();
        false)
  in
  Alcotest.(check bool) "a connection waits" false accepted

(* A listener queues [backlog + 1] connections, as Linux's accept queue
   takes one more than its backlog; a further connect waits until an accept
   makes room. *)
let test_backlog () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  List.iteri
    (fun i backlog ->
      let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8100 + i) in
      let listener = Eio.Net.listen ~sw ~backlog net addr in
      let connected = ref 0 in
      Eio.Fiber.fork ~sw (fun () ->
          for _ = 1 to backlog + 2 do
            ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket);
            incr connected
          done);
      Eio.Fiber.yield ();
      Alcotest.(check int)
        (Fmt.str "backlog %d queues" backlog)
        (backlog + 1) !connected;
      ignore (Eio.Net.accept ~sw listener);
      Eio.Fiber.yield ();
      Alcotest.(check int)
        (Fmt.str "backlog %d, an accept makes room" backlog)
        (backlog + 2) !connected)
    [ 0; 1; 2; 5 ]

(* Under [`Macos] a connect past a full accept queue waits for an accept to
   make room, as on Linux, so an accept loop serves every client of a
   listener with a backlog of 1. *)
let test_macos_backlog () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~os:`Macos ~sw () in
  let l =
    Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let addr = Eio.Net.listening_addr l in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        ignore (Eio.Net.accept ~sw l)
      done;
      `Stop_daemon);
  let clients = 4 in
  let outcomes =
    Eio.Fiber.List.map
      (fun () ->
        match Eio.Net.connect ~sw net addr with
        | _ -> "connected"
        | exception Eio.Io _ -> "failed")
      (List.init clients (fun _ -> ()))
  in
  Alcotest.(check (list string))
    "every client connects"
    (List.init clients (fun _ -> "connected"))
    outcomes

(* A listener's close defers to the accept waiting on it, as Eio defers a
   descriptor's close to its users: the listener takes connections until
   that accept returns one, and only then closes, so a later accept raises
   [Invalid_argument] and a later connect is refused. *)
let test_close_defers_to_an_accept () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8150) in
  let l = Eio.Net.listen ~sw ~backlog:4 net addr in
  let accepted = ref false in
  Eio.Fiber.both
    (fun () ->
      ignore (Eio.Net.accept ~sw l);
      accepted := true)
    (fun () ->
      Eio.Fiber.yield ();
      Eio.Resource.close l;
      ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket));
  Alcotest.(check bool) "the waiting accept" true !accepted;
  (match Eio.Net.accept ~sw l with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "accepted after close");
  match Eio.Net.connect ~sw net addr with
  | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> ()
  | _ -> Alcotest.fail "connected after close"

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

(* The two ends of a connection name the same connection, and no other
   connection does; a socket of another library names none. *)
let test_connection () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let listener = Eio.Net.listen ~sw ~backlog:2 net addr in
  let c1 = Eio.Net.connect ~sw net addr in
  let c2 = Eio.Net.connect ~sw net addr in
  let s1, _ = Eio.Net.accept ~sw listener in
  let s2, _ = Eio.Net.accept ~sw listener in
  let a, b = Eio_net_mem.pair () in
  let id s = Option.get (Eio_net_mem.Private.connection_id s) in
  Alcotest.(check bool) "first connection" true (id c1 = id s1);
  Alcotest.(check bool) "second connection" true (id c2 = id s2);
  Alcotest.(check bool) "a pipe" true (id a = id b);
  Alcotest.(check bool)
    "three connections" true
    (id c1 <> id c2 && id c1 <> id a && id c2 <> id a);
  let mock = Eio_mock.Flow.make "mock" in
  Alcotest.(check bool)
    "another socket" true
    (Eio_net_mem.Private.connection_id (mock :> _ Eio.Net.stream_socket) = None)

let suite =
  ( "eio_net_mem",
    [
      Alcotest.test_case "tcp roundtrip" `Quick test_tcp_roundtrip;
      Alcotest.test_case "localhost resolution" `Quick test_resolve_localhost;
      Alcotest.test_case "numeric resolution" `Quick test_resolve_numbers;
      Alcotest.test_case "name information" `Quick test_name_info;
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
      Alcotest.test_case "a capacity above the default" `Quick
        test_large_capacity;
      Alcotest.test_case "macOS: buffers" `Quick test_macos_capacity;
      Alcotest.test_case "macOS: a connect to 127.0.0.2 times out" `Quick
        test_macos_connect;
      Alcotest.test_case "a silent connection gives up as Linux does" `Quick
        test_silent_linux;
      Alcotest.test_case "a silent connection gives up as macOS does" `Quick
        test_silent_macos;
      Alcotest.test_case "a silent connection's peer still waits" `Quick
        test_silent_peer_waits;
      Alcotest.test_case "macOS: Unix addresses" `Quick test_macos_unix;
      Alcotest.test_case "a connection's queue is bounded by default" `Quick
        test_default_capacity;
      Alcotest.test_case "write after own shutdown" `Quick
        test_write_after_own_shutdown;
      Alcotest.test_case "an empty read" `Quick test_empty_read;
      Alcotest.test_case "a close defers to a read" `Quick
        test_close_defers_to_a_read;
      Alcotest.test_case "a close defers to a write" `Quick
        test_close_defers_to_a_write;
      Alcotest.test_case "a full write after the peer shut down both ways"
        `Quick test_full_write_after_peer_shutdown_all;
      Alcotest.test_case "read after peer close" `Quick
        test_read_after_peer_close;
      Alcotest.test_case "connect ignores a latency hint" `Quick
        test_connect_ignores_latency_hint;
      Alcotest.test_case "connect options" `Quick test_connect_options;
      Alcotest.test_case "socket options read back" `Quick
        test_sockopt_read_back;
      Alcotest.test_case "buffers bound the queue" `Quick
        test_buffers_bound_the_queue;
      Alcotest.test_case "SO_LINGER 0 resets" `Quick test_linger_zero_resets;
      Alcotest.test_case "connect closes at switch end" `Quick
        test_connect_closes_at_switch_end;
      Alcotest.test_case "a closed listener resets its queue" `Quick
        test_a_closed_listener_resets_its_queue;
      Alcotest.test_case "a reset is read once" `Quick test_reset_read_once;
      Alcotest.test_case "a write after shutdown reports a reset" `Quick
        test_write_after_shutdown_reports_reset;
      Alcotest.test_case "a cancelled connect frees its port" `Quick
        test_cancelled_connect_frees_its_port;
      Alcotest.test_case "a cancelled close frees its port" `Quick
        test_cancelled_close_frees_its_port;
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
      Alcotest.test_case "TIME_WAIT on one port scales" `Quick
        test_time_wait_scales;
      Alcotest.test_case "a Unix-domain write after peer close" `Quick
        test_unix_write_after_peer_close;
      Alcotest.test_case "a closed listener refuses connects" `Quick
        test_a_closed_listener_refuses_connects;
      Alcotest.test_case "a listener's backlog" `Quick test_backlog;
      Alcotest.test_case "macOS: a full accept queue makes a connect wait"
        `Quick test_macos_backlog;
      Alcotest.test_case "a close defers to an accept" `Quick
        test_close_defers_to_an_accept;
      Alcotest.test_case "a connect under a finished switch" `Quick
        test_connect_under_a_finished_switch;
      Alcotest.test_case "a closed connection leaves its switch" `Quick
        test_a_closed_connection_leaves_its_switch;
      Alcotest.test_case "a write in another domain wakes the read" `Quick
        test_a_write_in_another_domain_wakes_the_read;
      Alcotest.test_case "the ends of a connection name it" `Quick
        test_connection;
    ] )
