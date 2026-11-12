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
  let client, server = Eio_net_mem.pipe () in
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
  | exception e -> Alcotest.failf "the read raised %a" Fmt.exn e
  | (_ : int) -> Alcotest.fail "the read returned bytes"

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

(* A peer that shuts its receive side still reads what is written to it, as on
   Linux, then end-of-file once nothing is queued. *)
let test_write_after_peer_shutdown_receive () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server = Eio_net_mem.pipe () in
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
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
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

(* Connections the server closes first leave their ends in TIME_WAIT on the
   listener's port: 4000 of them connect with work, counted as the words
   allocated, that grows with their number, not with its square. *)
let test_time_wait_scales () =
  let words n =
    let before = Gc.minor_words () in
    ( Eio_mock.Backend.run @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio_mock.Clock.Mono.make () in
      let net = Eio_net_mem.v ~clock ~sw () in
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

(* Runs [f i] for [i] from 1 to [n] while another domain takes the network's
   mutex over and over, listening and closing, so that a fiber cancelled on
   its way into the mutex meets it held; then checks that [port] binds
   again. *)
(* The races across domains run a few hundred times by default;
   [dune build @test/conc-long] sets EIO_NET_LONG for the full counts. *)
let long = Sys.getenv_opt "EIO_NET_LONG" <> None
let iterations = if long then 2000 else 200

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
  let client, server = Eio_net_mem.pipe () in
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
  let client, server = Eio_net_mem.pipe () in
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
  let client, server = Eio_net_mem.pipe ~capacity:1 () in
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
  let client, _server = Eio_net_mem.pipe () in
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
  let client, server = Eio_net_mem.pipe ~capacity:1 () in
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
   spins after the reader asks, 0 to 63, so the write falls at every point of
   the reader's path from finding nothing to suspending: 256 rounds try each
   four times, 4096 under [@conc-long]. *)
let test_a_write_in_another_domain_wakes_the_read () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let near, far = Eio_net_mem.pipe () in
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

(* A listen raises EADDRINUSE raw, as eio_posix does, and a connect as an
   [Eio.Io] error. *)
let in_use f =
  match f () with
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> true
  | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> true
  | () -> false

let listens net ?(reuse_addr = false) addr () =
  Eio.Switch.run @@ fun sw ->
  ignore (Eio.Net.listen ~sw ~reuse_addr ~backlog:1 net addr)

(* A bind in [listen] or [datagram_socket] raises the raw [Unix.Unix_error]
   eio_posix raises there, and a connect an [Eio.Io] error marked by the code:
   either form of the pair passes, and any other exception, [Failure] included,
   fails the test. *)
let fails (code, errno) f =
  match f () with
  | exception Eio.Io (Eio.Exn.X c, _) when c = code -> true
  | exception Unix.Unix_error (e, _, _) when e = errno -> true
  | exception e -> Alcotest.failf "raised %a" Fmt.exn e
  | _ -> false

(* A connection opened and accepted under [sw], at [addr]; the listener is
   closed. *)
let connection ~sw net ?bind_to ?(reuse_addr = false) addr =
  let listener = Eio.Net.listen ~sw ~reuse_addr ~backlog:1 net addr in
  let client = Eio.Net.connect ~sw ?bind_to net addr in
  let server, _ = Eio.Net.accept ~sw listener in
  Eio.Resource.close listener;
  (client, server)

(* An accepted connection holds its listener's port, as on Linux: a listen
   there is refused, with [reuse_addr] or not, while the connection is open,
   though its listener closed. *)
let test_accepted_holds_the_port () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8110) in
  let client, server = connection ~sw net addr in
  Alcotest.(check bool) "relisten" true (in_use (listens net addr));
  Alcotest.(check bool)
    "relisten with reuse_addr" true
    (in_use (listens net ~reuse_addr:true addr));
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool) "relisten once closed" false (in_use (listens net addr))

(* The end that closes first holds its address in TIME_WAIT for 60 seconds on
   the network's clock, as Linux does; a bind there is refused unless both
   sockets have [reuse_addr], which an accepted socket takes from its
   listener. *)
let test_time_wait () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let wait secs =
    Eio_mock.Clock.Mono.set_time clock
      (Mtime.add_span (Eio.Time.Mono.now clock) Mtime.Span.(secs * s)
      |> Option.get)
  in
  (* The server closes first. *)
  let addr = `Tcp (lo, 8111) in
  let client, server = connection ~sw net addr in
  Eio.Resource.close server;
  Eio.Resource.close client;
  Alcotest.(check bool)
    "server first, relisten" true
    (in_use (listens net addr));
  Alcotest.(check bool)
    "server first, relisten with reuse_addr" true
    (in_use (listens net ~reuse_addr:true addr));
  wait 59;
  Alcotest.(check bool) "after 59 s" true (in_use (listens net addr));
  wait 1;
  Alcotest.(check bool) "after 60 s" false (in_use (listens net addr));
  (* The client closes first. *)
  let addr = `Tcp (lo, 8112) in
  let client, server = connection ~sw net addr in
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool)
    "client first, relisten" false
    (in_use (listens net addr));
  (* A listener with reuse_addr. *)
  let addr = `Tcp (lo, 8113) in
  let client, server = connection ~sw net ~reuse_addr:true addr in
  Eio.Resource.close server;
  Eio.Resource.close client;
  Alcotest.(check bool) "reused, relisten" true (in_use (listens net addr));
  Alcotest.(check bool)
    "reused, relisten with reuse_addr" false
    (in_use (listens net ~reuse_addr:true addr));
  (* A client bound at a port that closes first. *)
  let addr = `Tcp (lo, 8114) and bound = `Tcp (lo, 8115) in
  let client, server = connection ~sw net ~bind_to:bound addr in
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool)
    "bound client first, listen there" true
    (in_use (listens net ~reuse_addr:true bound));
  let listener = Eio.Net.listen ~sw ~backlog:1 net addr in
  Alcotest.(check bool)
    "bound client first, connect from there" true
    (in_use (fun () -> ignore (Eio.Net.connect ~sw ~bind_to:bound net addr)));
  Eio.Resource.close listener;
  wait 60;
  Alcotest.(check bool)
    "bound client, after 60 s" false
    (in_use (listens net bound))

(* An address no interface has fails to bind with EADDRNOTAVAIL, as on
   Linux: the network has the loopback interface only. A Unix path of 108
   bytes or more, longer than [sun_path] holds, fails with ENAMETOOLONG. *)
let test_unavailable_addresses () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = Eio.Net.Ipaddr.of_raw in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let unavailable =
    fails (Eio_net_mem.Address_not_available, Unix.EADDRNOTAVAIL)
  in
  List.iter
    (fun raw ->
      let a = ip raw in
      let name = Fmt.str "%a" Eio.Net.Ipaddr.pp a in
      Alcotest.(check bool)
        ("listen at " ^ name) true
        (unavailable (fun () ->
             ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (a, 8120)))));
      Alcotest.(check bool)
        ("datagram socket at " ^ name)
        true
        (unavailable (fun () ->
             ignore (Eio.Net.datagram_socket ~sw net (`Udp (a, 8120))))))
    [
      "\010\255\255\001";
      "\192\000\002\001";
      "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002";
      "\000\000\000\000\000\000\000\000\000\000\255\255\010\255\255\001";
    ];
  let addr = `Tcp (lo, 8121) in
  ignore (Eio.Net.listen ~sw ~backlog:1 net addr);
  Alcotest.(check bool)
    "connect bound at 10.255.255.1" true
    (unavailable (fun () ->
         ignore
           (Eio.Net.connect ~sw
              ~bind_to:(`Tcp (ip "\010\255\255\001", 0))
              net addr)));
  Alcotest.(check bool)
    "listen at 127.0.0.2" false
    (unavailable (fun () ->
         ignore
           (Eio.Net.listen ~sw ~backlog:1 net
              (`Tcp (ip "\127\000\000\002", 8121)))));
  let long = `Unix ("/tmp/" ^ String.make 103 'x') in
  let too_long = fails (Eio_net_mem.Name_too_long, Unix.ENAMETOOLONG) in
  Alcotest.(check bool)
    "listen at a long path" true
    (too_long (fun () -> ignore (Eio.Net.listen ~sw ~backlog:1 net long)));
  Alcotest.(check bool)
    "connect to a long path" true
    (too_long (fun () -> ignore (Eio.Net.connect ~sw net long)));
  Alcotest.(check bool)
    "datagram socket at a long path" true
    (too_long (fun () -> ignore (Eio.Net.datagram_socket ~sw net long)));
  Alcotest.(check bool)
    "listen at a path of 107 bytes" false
    (too_long (fun () ->
         ignore
           (Eio.Net.listen ~sw ~backlog:1 net
              (`Unix ("/tmp/" ^ String.make 102 'x')))))

(* A connect bound to an address its socket's family cannot bind fails as
   bind(2) fails it on Linux: an IPv6 or Unix-domain socket with EINVAL, an
   IPv4 one with EAFNOSUPPORT. *)
let test_bind_to_another_family () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let v4 = Eio.Net.Ipaddr.V4.loopback and v6 = Eio.Net.Ipaddr.V6.loopback in
  let raises code ~bind_to addr =
    match Eio.Net.connect ~sw ~bind_to net addr with
    | exception Eio.Io (Eio.Exn.X c, _) -> c = code
    | (_ : _ Eio.Net.stream_socket) -> false
  in
  Alcotest.(check bool)
    "IPv4 for IPv6" true
    (raises Eio_net_mem.Invalid_address ~bind_to:(`Tcp (v4, 0)) (`Tcp (v6, 80)));
  Alcotest.(check bool)
    "TCP for a path" true
    (raises Eio_net_mem.Invalid_address
       ~bind_to:(`Tcp (v4, 0))
       (`Unix "/tmp/s.sock"));
  Alcotest.(check bool)
    "IPv6 for IPv4" true
    (raises Eio_net_mem.Address_family ~bind_to:(`Tcp (v6, 0)) (`Tcp (v4, 80)));
  Alcotest.(check bool)
    "a path for TCP" true
    (raises Eio_net_mem.Address_family ~bind_to:(`Unix "/tmp/c.sock")
       (`Tcp (v4, 80)))

(* A client that binds no address sends from 127.0.0.1 to any address of
   127.0.0.0/8, as Linux picks the loopback interface's own address, for
   streams and datagrams alike. *)
let test_source_address () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let other = Eio.Net.Ipaddr.of_raw "\127\000\000\002" in
  let pp = Fmt.to_to_string Eio.Net.Sockaddr.pp in
  let l = Eio.Net.listen ~sw ~backlog:1 net (`Tcp (other, 8130)) in
  ignore (Eio.Net.connect ~sw net (`Tcp (other, 8130)));
  (match Eio.Net.accept ~sw l with
  | _, `Tcp (ip, _) ->
      Alcotest.(check string)
        "stream peer" "127.0.0.1"
        (Fmt.to_to_string Eio.Net.Ipaddr.pp ip)
  | _, peer -> Alcotest.failf "peer %s" (pp (peer :> Eio.Net.Sockaddr.t)));
  let r = Eio.Net.datagram_socket ~sw net (`Udp (other, 8131)) in
  let s = Eio.Net.datagram_socket ~sw net `UdpV4 in
  Eio.Net.send s ~dst:(`Udp (other, 8131)) [ Cstruct.of_string "x" ];
  match Eio.Net.recv r (Cstruct.create 1) with
  | `Udp (ip, _), _ ->
      Alcotest.(check string)
        "datagram source" "127.0.0.1"
        (Fmt.to_to_string Eio.Net.Ipaddr.pp ip)
  | src, _ -> Alcotest.failf "source %s" (pp (src :> Eio.Net.Sockaddr.t))

(* A wildcard destination stands for the address the socket is bound at, as
   Linux routes it: 0.0.0.0 from 127.0.0.2 reaches 127.0.0.2, and :: from an
   IPv4-mapped address reaches 127.0.0.1, for streams and datagrams. *)
let test_wildcard_destination () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = Eio.Net.Ipaddr.of_raw in
  let other = ip "\127\000\000\002" in
  let mapped =
    ip "\000\000\000\000\000\000\000\000\000\000\255\255\127\000\000\001"
  in
  let v4 = Eio.Net.Ipaddr.V4.any and v6 = Eio.Net.Ipaddr.V6.any in
  let connects ~listen ~bind dst =
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~sw ~backlog:1 net (`Tcp (listen, 0)) in
    let port =
      match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
    in
    match
      Eio.Net.connect ~sw ~bind_to:(`Tcp (bind, 0)) net (`Tcp (dst, port))
    with
    | _ -> true
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> false
  in
  Alcotest.(check bool)
    "0.0.0.0 from 127.0.0.2 to 127.0.0.2" true
    (connects ~listen:other ~bind:other v4);
  Alcotest.(check bool)
    "0.0.0.0 from 127.0.0.2 to 127.0.0.1" false
    (connects ~listen:Eio.Net.Ipaddr.V4.loopback ~bind:other v4);
  Alcotest.(check bool)
    ":: from ::ffff:127.0.0.1" true
    (connects ~listen:Eio.Net.Ipaddr.V4.loopback ~bind:mapped v6);
  let r = Eio.Net.datagram_socket ~sw net (`Udp (other, 8140)) in
  let s = Eio.Net.datagram_socket ~sw net (`Udp (other, 8141)) in
  Eio.Net.send s ~dst:(`Udp (v4, 8140)) [ Cstruct.of_string "x" ];
  let r4 =
    Eio.Net.datagram_socket ~sw net (`Udp (Eio.Net.Ipaddr.V4.loopback, 8142))
  in
  let s6 = Eio.Net.datagram_socket ~sw net (`Udp (mapped, 8143)) in
  Eio.Net.send s6 ~dst:(`Udp (v6, 8142)) [ Cstruct.of_string "y" ];
  let got sock =
    let buf = Cstruct.create 1 in
    let _, n = Eio.Net.recv sock buf in
    Cstruct.to_string ~len:n buf
  in
  Alcotest.(check string) "datagram to 0.0.0.0" "x" (got r);
  Alcotest.(check string) "datagram to ::" "y" (got r4)

(* Port 0 binds the lowest free port of the dynamic range, again once it is
   freed, for listeners and datagram sockets; clients take ports counted down
   from its top. *)
let test_port_choice () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let port l =
    match Eio.Net.listening_addr l with `Tcp (_, p) -> p | _ -> 0
  in
  let a = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  let b = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  Alcotest.(check (list int)) "listeners" [ 49152; 49153 ] [ port a; port b ];
  Eio.Resource.close a;
  let c = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  Alcotest.(check int) "the freed lowest port" 49152 (port c);
  let peer () =
    ignore (Eio.Net.connect ~sw net (`Tcp (lo, port b)));
    match Eio.Net.accept ~sw b with _, `Tcp (_, p) -> p | _ -> 0
  in
  let p1 = peer () in
  let p2 = peer () in
  Alcotest.(check (list int)) "clients" [ 65535; 65534 ] [ p1; p2 ];
  let udp () =
    let s = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
    let r = Eio.Net.datagram_socket ~sw net (`Udp (lo, 50000)) in
    Eio.Net.send s ~dst:(`Udp (lo, 50000)) [ Cstruct.of_string "x" ];
    let src, _ = Eio.Net.recv r (Cstruct.create 1) in
    Eio.Resource.close r;
    (s, match src with `Udp (_, p) -> p | `Unix _ -> 0)
  in
  let s, p = udp () in
  Alcotest.(check int) "a datagram socket" 49152 p;
  Eio.Resource.close s;
  let _, p = udp () in
  Alcotest.(check int) "a datagram socket, again" 49152 p

(* A bind at port 0 when every port of the dynamic range, 16384 of them, is
   held fails as bind(2) fails it on Linux, with EADDRINUSE raw, as a bind at
   an address a socket holds does, and not with [Failure]. The range is filled
   the cheapest way, a listener bound at each port, which holds them all until
   [sw] finishes and needs no clock; the port the [listen] under test picks is
   the one left. *)
let test_no_ephemeral_port_for_a_listen () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  for port = 49152 to 65535 do
    ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, port)))
  done;
  Alcotest.(check bool)
    "a listen at port 0" true
    (fails (Eio_net_mem.Address_in_use, Unix.EADDRINUSE) (fun () ->
         ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0)))))

(* A datagram socket at port 0 fails the same way, with EADDRINUSE raw and not
   with [Failure]. Datagram sockets hold their own ports, apart from the
   listeners' and the clients', so their range is filled with datagram
   sockets. *)
let test_no_ephemeral_port_for_a_datagram_socket () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  for port = 49152 to 65535 do
    ignore (Eio.Net.datagram_socket ~sw net (`Udp (lo, port)))
  done;
  Alcotest.(check bool)
    "a datagram socket at port 0" true
    (fails (Eio_net_mem.Address_in_use, Unix.EADDRINUSE) (fun () ->
         ignore (Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)))))

(* A [connect] whose socket binds no address takes a port of the range counted
   down from its top, so a range the listeners hold leaves it none: it fails as
   connect(2) fails it on Linux, with EADDRNOTAVAIL as an [Eio.Io] error, and
   not with [Failure]. One bound with [bind_to] at port 0 fails its bind(2)
   first, with EADDRINUSE, as eio_posix binds before it connects. The listener
   it reaches for is below the range, so it holds no port of it. *)
let test_no_ephemeral_port_for_a_connect () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let addr = `Tcp (lo, 8170) in
  ignore (Eio.Net.listen ~sw ~backlog:1 net addr);
  for port = 49152 to 65535 do
    ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, port)))
  done;
  Alcotest.(check bool)
    "a connect that binds no port" true
    (fails (Eio_net_mem.Address_not_available, Unix.EADDRNOTAVAIL) (fun () ->
         ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket)));
  let bind_to = `Tcp (lo, 0) in
  Alcotest.(check bool)
    "a connect bound at port 0" true
    (fails (Eio_net_mem.Address_in_use, Unix.EADDRINUSE) (fun () ->
         ignore
           (Eio.Net.connect ~sw ~bind_to net addr : _ Eio.Net.stream_socket)))

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

(* TIME_WAIT goes to the end whose FIN went first, as on Linux: a shutdown of
   the send side sends it, whoever closes first. *)
let test_time_wait_follows_the_first_fin () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let case port first second =
    let addr = `Tcp (lo, port) in
    let client, server = connection ~sw net addr in
    first client server;
    second client server;
    Eio.Resource.close server;
    Eio.Resource.close client;
    in_use (listens net addr)
  in
  let shut flow = Eio.Flow.shutdown flow `Send in
  Alcotest.(check bool)
    "the client shuts its send, the server closes first" false
    (case 8160 (fun c _ -> shut c) (fun _ _ -> ()));
  Alcotest.(check bool)
    "the client shuts down, the server closes first" false
    (case 8161 (fun c _ -> Eio.Flow.shutdown c `All) (fun _ _ -> ()));
  Alcotest.(check bool)
    "the server shuts its send, then the client" true
    (case 8162 (fun _ s -> shut s) (fun c _ -> shut c));
  Alcotest.(check bool)
    "the client shuts its receive side only" true
    (case 8163 (fun c _ -> Eio.Flow.shutdown c `Receive) (fun _ _ -> ()))

(* An end that closed after its FIN, whose peer never sends its own, holds its
   address in FIN_WAIT2 for 60 seconds, Linux's tcp_fin_timeout; a FIN that
   comes in that time starts TIME_WAIT. *)
let test_fin_wait2_times_out () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let at secs =
    Eio_mock.Clock.Mono.set_time clock
      (Mtime.of_uint64_ns (Int64.mul (Int64.of_int secs) 1_000_000_000L))
  in
  let a = `Tcp (lo, 8170) and b = `Tcp (lo, 8171) in
  let _client_a, server_a = connection ~sw net a in
  let client_b, server_b = connection ~sw net b in
  Eio.Resource.close server_a;
  Eio.Resource.close server_b;
  at 29;
  Eio.Resource.close client_b;
  at 59;
  Alcotest.(check bool) "an orphan at 59 s" true (in_use (listens net a));
  at 60;
  Alcotest.(check bool) "an orphan at 60 s" false (in_use (listens net a));
  Alcotest.(check bool) "the peer's FIN at 29 s" true (in_use (listens net b));
  at 89;
  Alcotest.(check bool) "TIME_WAIT from 29 s" false (in_use (listens net b))

(* A Unix-domain datagram socket bound at "" takes an abstract address of its
   own, as Linux autobinds it: "\000" and five hexadecimal digits, which a
   reply reaches, and which leaves no file behind. *)
let test_datagram_autobind () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let server = Eio.Net.datagram_socket ~sw net (`Unix "/tmp/autobind.sock") in
  let a = Eio.Net.datagram_socket ~sw net (`Unix "") in
  let b = Eio.Net.datagram_socket ~sw net (`Unix "") in
  let buf = Cstruct.create 8 in
  let from sock =
    Eio.Net.send sock ~dst:(`Unix "/tmp/autobind.sock")
      [ Cstruct.of_string "x" ];
    match Eio.Net.recv server buf with
    | `Unix name, _ -> name
    | `Udp _, _ -> Alcotest.fail "a UDP sender"
  in
  let na = from a and nb = from b in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (Fmt.str "%S is abstract" name)
        true
        (String.length name = 6 && name.[0] = '\000'))
    [ na; nb ];
  Alcotest.(check bool) "two addresses" true (na <> nb);
  Eio.Net.send server ~dst:(`Unix na) [ Cstruct.of_string "ok" ];
  let _, n = Eio.Net.recv a buf in
  Alcotest.(check string) "the reply" "ok" (Cstruct.to_string ~len:n buf);
  Eio.Switch.run (fun inner ->
      ignore (Eio.Net.datagram_socket ~sw:inner net (`Unix "\000named")));
  ignore (Eio.Net.datagram_socket ~sw net (`Unix "\000named"))

(* UDP sockets that both have [reuse_addr] share an address, as on Linux: a
   datagram reaches the one bound at the most specific address, an IPv4
   wildcard before an IPv6 one, and the latest bound of equals. *)
let test_datagram_reuse_addr () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = function
    | "127.0.0.1" -> Eio.Net.Ipaddr.V4.loopback
    | "0.0.0.0" -> Eio.Net.Ipaddr.V4.any
    | _ -> Eio.Net.Ipaddr.V6.any
  in
  let sender = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let reaches name port binds expected =
    Eio.Switch.run @@ fun sw ->
    let socks =
      List.map
        (fun a ->
          Eio.Net.datagram_socket ~sw ~reuse_addr:true net (`Udp (ip a, port)))
        binds
    in
    Eio.Net.send sender
      ~dst:(`Udp (Eio.Net.Ipaddr.V4.loopback, port))
      [ Cstruct.of_string "x" ];
    let got =
      List.mapi
        (fun i s ->
          Eio.Fiber.first
            (fun () ->
              ignore (Eio.Net.recv s (Cstruct.create 1));
              Some i)
            (fun () ->
              Eio.Fiber.yield ();
              None))
        socks
      |> List.filter_map Fun.id
    in
    Alcotest.(check (list int)) name [ expected ] got
  in
  reaches "two at 127.0.0.1" 8180 [ "127.0.0.1"; "127.0.0.1" ] 1;
  reaches "the wildcard first" 8181 [ "0.0.0.0"; "127.0.0.1" ] 1;
  reaches "the address first" 8182 [ "127.0.0.1"; "0.0.0.0" ] 0;
  reaches "IPv6 then IPv4 wildcard" 8183 [ "::"; "0.0.0.0" ] 1;
  reaches "IPv4 then IPv6 wildcard" 8184 [ "0.0.0.0"; "::" ] 0;
  let first =
    Eio.Net.datagram_socket ~sw ~reuse_addr:true net
      (`Udp (ip "127.0.0.1", 8185))
  in
  Eio.Switch.run (fun inner ->
      ignore
        (Eio.Net.datagram_socket ~sw:inner ~reuse_addr:true net
           (`Udp (ip "127.0.0.1", 8185))));
  Eio.Net.send sender
    ~dst:(`Udp (Eio.Net.Ipaddr.V4.loopback, 8185))
    [ Cstruct.of_string "y" ];
  ignore (Eio.Net.recv first (Cstruct.create 1));
  match Eio.Net.datagram_socket ~sw net (`Udp (ip "127.0.0.1", 8185)) with
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
  | _ -> Alcotest.fail "bound without reuse_addr"

(* A datagram's errors come in Linux's order: a Unix-domain socket refuses a
   UDP address with EINVAL before the length; an IPv4 socket refuses more than
   65535 bytes first, then an address of another family, port 0, or one off
   loopback, then a datagram too long for IPv4; an
   IPv6 socket refuses another family or an unreachable network first. *)
let test_datagram_error_order () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = Eio.Net.Ipaddr.of_raw in
  let v4 = Eio.Net.Ipaddr.V4.loopback and v6 = Eio.Net.Ipaddr.V6.loopback in
  let mapped =
    ip "\000\000\000\000\000\000\000\000\000\000\255\255\127\000\000\001"
  in
  let unix_sock = Eio.Net.datagram_socket ~sw net (`Unix "/tmp/order.sock") in
  let v4_sock = Eio.Net.datagram_socket ~sw net (`Udp (v4, 0)) in
  let v6_sock = Eio.Net.datagram_socket ~sw net (`Udp (v6, 0)) in
  let mapped_sock = Eio.Net.datagram_socket ~sw net (`Udp (mapped, 0)) in
  let error sock dst len =
    match Eio.Net.send sock ~dst [ Cstruct.create len ] with
    | () -> "sent"
    | exception Eio.Io (Eio.Exn.X code, _) -> (
        match code with
        | Eio_net_mem.Invalid_address | Eio_net_mem.Invalid_port -> "EINVAL"
        | Eio_net_mem.Address_family -> "EAFNOSUPPORT"
        | Eio_net_mem.Network_unreachable -> "ENETUNREACH"
        | Eio_net_mem.Message_too_long -> "EMSGSIZE"
        | _ -> "other")
    | exception e -> Printexc.to_string e
  in
  List.iter
    (fun (name, sock, dst, len, expected) ->
      Alcotest.(check string) name expected (error sock dst len))
    [
      ("Unix to UDP", unix_sock, `Udp (v4, 9), 10, "EINVAL");
      ("Unix to UDP, too long", unix_sock, `Udp (v4, 9), 300000, "EINVAL");
      ("IPv4 to IPv6, 65508", v4_sock, `Udp (v6, 9), 65508, "EAFNOSUPPORT");
      ("IPv4 to IPv6, 65536", v4_sock, `Udp (v6, 9), 65536, "EMSGSIZE");
      ("IPv4 to port 0, 65508", v4_sock, `Udp (v4, 0), 65508, "EINVAL");
      ("IPv4 to a short path", v4_sock, `Unix "/tmp/nowhere", 10, "EINVAL");
      ( "IPv4 to a long path",
        v4_sock,
        `Unix "/tmp/abcdefghijk",
        10,
        "EAFNOSUPPORT" );
      ( "loopback to 10.1.2.3",
        v4_sock,
        `Udp (ip "\010\001\002\003", 9),
        65508,
        "ENETUNREACH" );
      ( "loopback to 224.0.0.1",
        v4_sock,
        `Udp (ip "\224\000\000\001", 9),
        10,
        "sent" );
      ("IPv6 to a path", v6_sock, `Unix "/tmp/abcdefghijk", 300000, "EINVAL");
      ("::1 to IPv4, too long", v6_sock, `Udp (v4, 9), 300000, "ENETUNREACH");
      ( "mapped to IPv6, too long",
        mapped_sock,
        `Udp (v6, 9),
        300000,
        "EAFNOSUPPORT" );
    ]

(* Linux's errors at the edges of the address space: broadcast and IPv4
   multicast addresses bind; link-local IPv6 ones without a scope do not,
   and TCP binds no IPv6 multicast one; a connect to broadcast or multicast
   finds no route, and one from a loopback address to an address outside
   127.0.0.0/8 cannot be routed. *)
let test_edge_addresses () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip s = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string s) in
  let outcome f =
    match f () with
    | () -> "ok"
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Invalid_address, _) -> "EINVAL"
    | exception Unix.Unix_error (Unix.EINVAL, "bind", _) -> "EINVAL"
    | exception Unix.Unix_error (Unix.EADDRNOTAVAIL, "bind", _) ->
        "EADDRNOTAVAIL"
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Network_unreachable, _) ->
        "ENETUNREACH"
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
        "REFUSED"
    | exception e -> Printexc.to_string e
  in
  let listen a () =
    Eio.Switch.run @@ fun sw ->
    ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip a, 8190)))
  in
  let dgram a () =
    Eio.Switch.run @@ fun sw ->
    ignore (Eio.Net.datagram_socket ~sw net (`Udp (ip a, 8191)))
  in
  let connect ?bind d () =
    Eio.Switch.run @@ fun sw ->
    let bind_to = Option.map (fun b -> `Tcp (ip b, 0)) bind in
    ignore (Eio.Net.connect ~sw ?bind_to net (`Tcp (ip d, 8192)))
  in
  List.iter
    (fun (a, l, d) ->
      Alcotest.(check string) ("listen at " ^ a) l (outcome (listen a));
      Alcotest.(check string)
        ("a datagram socket at " ^ a)
        d
        (outcome (dgram a)))
    [
      ("255.255.255.255", "ok", "ok");
      ("224.0.0.1", "ok", "ok");
      ("::ffff:224.0.0.1", "ok", "ok");
      ("ff02::1", "EINVAL", "EINVAL");
      ("fe80::1", "EINVAL", "EINVAL");
      ("ff05::1", "EINVAL", "ok");
      ("::2", "EADDRNOTAVAIL", "EADDRNOTAVAIL");
    ];
  List.iter
    (fun (d, bind, expected) ->
      Alcotest.(check string)
        (Fmt.str "connect to %s from %s" d (Option.value bind ~default:"-"))
        expected
        (outcome (connect ?bind d)))
    [
      ("255.255.255.255", None, "ENETUNREACH");
      ("224.0.0.1", None, "ENETUNREACH");
      ("::ffff:224.0.0.1", None, "ENETUNREACH");
      ("ff02::1", None, "ENETUNREACH");
      ("fe80::1", None, "EINVAL");
      ("10.1.2.3", Some "127.0.0.1", "ENETUNREACH");
      ("::ffff:10.1.2.3", Some "::ffff:127.0.0.1", "ENETUNREACH");
      ("127.0.0.2", Some "127.0.0.1", "REFUSED");
    ]

(* A connect bound at its own destination, where no listener is, connects
   to itself, as Linux's simultaneous open does: what it writes, it reads,
   and it holds the address. *)
let test_self_connect () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8193) in
  let flow = Eio.Net.connect ~sw ~bind_to:addr net addr in
  Eio.Flow.copy_string "hi" flow;
  let buf = Cstruct.create 4 in
  let n = Eio.Flow.single_read flow buf in
  Alcotest.(check string)
    "it reads its own bytes" "hi"
    (Cstruct.to_string ~len:n buf);
  Alcotest.(check bool) "it holds the address" true (in_use (listens net addr));
  Eio.Flow.shutdown flow `Send;
  (match Eio.Flow.single_read flow buf with
  | exception End_of_file -> ()
  | n -> Alcotest.failf "read %d after its own FIN" n);
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw ~bind_to:addr net addr));
  Alcotest.(check bool) "TIME_WAIT once closed" true (in_use (listens net addr))

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
  (* What the receive buffer holds of datagrams this short. *)
  let n = 199 in
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
   with the raw EADDRINUSE eio_posix raises; the wildcard address
   of a port overlaps every address of it. *)
let test_datagram_address_in_use () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let in_use addr =
    match Eio.Net.datagram_socket ~sw net addr with
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> true
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
  let a, b = Eio_net_mem.pipe () in
  let id s = Option.get (Eio_net_mem.connection s) in
  Alcotest.(check bool) "first connection" true (id c1 = id s1);
  Alcotest.(check bool) "second connection" true (id c2 = id s2);
  Alcotest.(check bool) "a pipe" true (id a = id b);
  Alcotest.(check bool)
    "three connections" true
    (id c1 <> id c2 && id c1 <> id a && id c2 <> id a);
  let mock = Eio_mock.Flow.make "mock" in
  Alcotest.(check bool)
    "another socket" true
    (Eio_net_mem.connection (mock :> _ Eio.Net.stream_socket) = None)

(* The datagram a socket has queued, if any, without waiting. *)
let poll sock buf =
  Eio.Fiber.first
    (fun () -> Some (Eio.Net.recv sock buf))
    (fun () ->
      Eio.Fiber.yield ();
      None)

(* A UDP socket that does not read keeps what Linux's default receive buffer
   holds and drops the rest, each datagram counted with the memory Linux
   gives it: the counts kept of 300 datagrams of each length, measured on
   Linux over IPv4 and IPv6. Once read, the buffer takes as many again. *)
let test_udp_receive_buffer () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let port = ref 6000 in
  let kept ip n =
    incr port;
    Eio.Switch.run @@ fun sw ->
    let r = Eio.Net.datagram_socket ~sw net (`Udp (ip, !port)) in
    let s = Eio.Net.datagram_socket ~sw net (`Udp (ip, 0)) in
    let data = Cstruct.create n and buf = Cstruct.create 65536 in
    let fill () =
      for _ = 1 to 300 do
        Eio.Net.send s ~dst:(`Udp (ip, !port)) [ data ]
      done
    in
    let rec drain k =
      match poll r buf with Some _ -> drain (k + 1) | None -> k
    in
    fill ();
    let k = drain 0 in
    fill ();
    let again = drain 0 in
    Alcotest.(check int) (Fmt.str "%d bytes, again" n) k again;
    k
  in
  List.iter
    (fun (n, v4, v6) ->
      Alcotest.(check int)
        (Fmt.str "%d bytes over IPv4" n)
        v4
        (kept Eio.Net.Ipaddr.V4.loopback n);
      Alcotest.(check int)
        (Fmt.str "%d bytes over IPv6" n)
        v6
        (kept Eio.Net.Ipaddr.V6.loopback n))
    [
      (1, 199, 199);
      (444, 199, 179);
      (500, 179, 179);
      (1000, 99, 99);
      (1600, 99, 52);
      (4000, 27, 27);
      (10000, 13, 13);
      (20000, 10, 10);
      (60000, 3, 3);
    ]

(* A Unix-domain datagram send waits while its receiver holds more than 10
   datagrams, Linux's max_dgram_qlen, or while the sender's datagrams not yet
   received fill its send buffer, and goes on once the receiver takes one:
   the counts sent before the first wait, measured on Linux. *)
let test_unix_dgram_queue () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let sent_before_wait n =
    Eio.Switch.run @@ fun sw ->
    let path = Fmt.str "/tmp/r%d.sock" n in
    let r = Eio.Net.datagram_socket ~sw net (`Unix path) in
    let s = Eio.Net.datagram_socket ~sw net (`Unix "") in
    let sent = ref 0 in
    let data = Cstruct.create n in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        for _ = 1 to 20 do
          Eio.Net.send s ~dst:(`Unix path) [ data ];
          incr sent
        done;
        `Stop_daemon);
    for _ = 1 to 10 do
      Eio.Fiber.yield ()
    done;
    let before = !sent in
    ignore (Eio.Net.recv r (Cstruct.create 300_000));
    for _ = 1 to 10 do
      Eio.Fiber.yield ()
    done;
    Alcotest.(check int)
      (Fmt.str "%d bytes, after a receive" n)
      (before + 1) !sent;
    before
  in
  List.iter
    (fun (n, sent) ->
      Alcotest.(check int) (Fmt.str "%d bytes" n) sent (sent_before_wait n))
    [ (1, 11); (20000, 11); (30000, 7); (65536, 4); (200000, 2) ]

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
      Alcotest.test_case "a datagram too long" `Quick test_datagram_too_long;
      Alcotest.test_case "a datagram reaches as a kernel delivers it" `Quick
        test_datagram_reaches_like_a_kernel;
      Alcotest.test_case "a closed datagram socket's path" `Quick
        test_closed_datagram_path;
      Alcotest.test_case "TIME_WAIT on one port scales" `Quick
        test_time_wait_scales;
      Alcotest.test_case "a UDP receive buffer drops past Linux's" `Quick
        test_udp_receive_buffer;
      Alcotest.test_case "a Unix-domain datagram send waits for room" `Quick
        test_unix_dgram_queue;
      Alcotest.test_case "a Unix-domain write after peer close" `Quick
        test_unix_write_after_peer_close;
      Alcotest.test_case "a closed listener refuses connects" `Quick
        test_a_closed_listener_refuses_connects;
      Alcotest.test_case "a listener's backlog" `Quick test_backlog;
      Alcotest.test_case "a close defers to an accept" `Quick
        test_close_defers_to_an_accept;
      Alcotest.test_case "an accepted connection holds the port" `Quick
        test_accepted_holds_the_port;
      Alcotest.test_case "TIME_WAIT" `Quick test_time_wait;
      Alcotest.test_case "TIME_WAIT follows the first FIN" `Quick
        test_time_wait_follows_the_first_fin;
      Alcotest.test_case "FIN_WAIT2 times out" `Quick test_fin_wait2_times_out;
      Alcotest.test_case "a datagram socket at \"\" autobinds" `Quick
        test_datagram_autobind;
      Alcotest.test_case "datagram sockets with reuse_addr" `Quick
        test_datagram_reuse_addr;
      Alcotest.test_case "the order of a datagram's errors" `Quick
        test_datagram_error_order;
      Alcotest.test_case "addresses at the edges" `Quick test_edge_addresses;
      Alcotest.test_case "a connect to itself" `Quick test_self_connect;
      Alcotest.test_case "addresses no interface has" `Quick
        test_unavailable_addresses;
      Alcotest.test_case "bind_to another family" `Quick
        test_bind_to_another_family;
      Alcotest.test_case "the source address" `Quick test_source_address;
      Alcotest.test_case "the choice of ports" `Quick test_port_choice;
      Alcotest.test_case "no ephemeral port for a listen" `Quick
        test_no_ephemeral_port_for_a_listen;
      Alcotest.test_case "no ephemeral port for a datagram socket" `Quick
        test_no_ephemeral_port_for_a_datagram_socket;
      Alcotest.test_case "no ephemeral port for a connect" `Quick
        test_no_ephemeral_port_for_a_connect;
      Alcotest.test_case "a wildcard destination" `Quick
        test_wildcard_destination;
      Alcotest.test_case "a connect under a finished switch" `Quick
        test_connect_under_a_finished_switch;
      Alcotest.test_case "a closed connection leaves its switch" `Quick
        test_a_closed_connection_leaves_its_switch;
      Alcotest.test_case "a write in another domain wakes the read" `Quick
        test_a_write_in_another_domain_wakes_the_read;
      Alcotest.test_case "the ends of a connection name it" `Quick
        test_connection;
    ] )
