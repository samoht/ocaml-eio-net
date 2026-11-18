(* A bind in [listen] or [datagram_socket] raises the [Eio.Io] error eio_posix
   raises there, and a connect the one marked by the code:
   [fails error f] is [true] if [f] raises either form of [error], and any
   other exception, [Failure] included, fails the test. *)
let fails error f =
  match f () with exception e when error e -> true | _ -> false

let in_use = function
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _)
  | Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) ->
      true
  | _ -> false

let not_available = function
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRNOTAVAIL, _, _)), _)
  | Eio.Io (Eio.Exn.X Eio_net_mem.Address_not_available, _) ->
      true
  | _ -> false

let name_too_long = function
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENAMETOOLONG, _, _)), _)
  | Eio.Io (Eio.Exn.X Eio_net_mem.Name_too_long, _) ->
      true
  | _ -> false

let listens net ?(reuse_addr = false) addr () =
  Eio.Switch.run @@ fun sw ->
  ignore (Eio.Net.listen ~sw ~reuse_addr ~backlog:1 net addr)

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
  Alcotest.(check bool) "relisten" true (fails in_use (listens net addr));
  Alcotest.(check bool)
    "relisten with reuse_addr" true
    (fails in_use (listens net ~reuse_addr:true addr));
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool)
    "relisten once closed" false
    (fails in_use (listens net addr))

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
    (fails in_use (listens net addr));
  Alcotest.(check bool)
    "server first, relisten with reuse_addr" true
    (fails in_use (listens net ~reuse_addr:true addr));
  wait 59;
  Alcotest.(check bool) "after 59 s" true (fails in_use (listens net addr));
  wait 1;
  Alcotest.(check bool) "after 60 s" false (fails in_use (listens net addr));
  (* The client closes first. *)
  let addr = `Tcp (lo, 8112) in
  let client, server = connection ~sw net addr in
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool)
    "client first, relisten" false
    (fails in_use (listens net addr));
  (* A listener with reuse_addr. *)
  let addr = `Tcp (lo, 8113) in
  let client, server = connection ~sw net ~reuse_addr:true addr in
  Eio.Resource.close server;
  Eio.Resource.close client;
  Alcotest.(check bool)
    "reused, relisten" true
    (fails in_use (listens net addr));
  Alcotest.(check bool)
    "reused, relisten with reuse_addr" false
    (fails in_use (listens net ~reuse_addr:true addr));
  (* A client bound at a port that closes first. *)
  let addr = `Tcp (lo, 8114) and bound = `Tcp (lo, 8115) in
  let client, server = connection ~sw net ~bind_to:bound addr in
  Eio.Resource.close client;
  Eio.Resource.close server;
  Alcotest.(check bool)
    "bound client first, listen there" true
    (fails in_use (listens net ~reuse_addr:true bound));
  let listener = Eio.Net.listen ~sw ~backlog:1 net addr in
  Alcotest.(check bool)
    "bound client first, connect from there" true
    (fails in_use (fun () ->
         ignore (Eio.Net.connect ~sw ~bind_to:bound net addr)));
  Eio.Resource.close listener;
  wait 60;
  Alcotest.(check bool)
    "bound client, after 60 s" false
    (fails in_use (listens net bound))

(* An address no interface has fails to bind with EADDRNOTAVAIL, as on
   Linux: the network has the loopback interface only. A Unix path of 108
   bytes or more, longer than [sun_path] holds, fails with ENAMETOOLONG. *)
let test_unavailable_addresses () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let ip = Eio.Net.Ipaddr.of_raw in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let unavailable = fails not_available in
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
  let too_long = fails name_too_long in
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
  let raises ~bind_to addr =
    match Eio.Net.connect ~sw ~bind_to net addr with
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Invalid_address, _) -> "EINVAL"
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_family, _) ->
        "EAFNOSUPPORT"
    | (_ : _ Eio.Net.stream_socket) -> "connected"
  in
  Alcotest.(check string)
    "IPv4 for IPv6" "EINVAL"
    (raises ~bind_to:(`Tcp (v4, 0)) (`Tcp (v6, 80)));
  Alcotest.(check string)
    "TCP for a path" "EINVAL"
    (raises ~bind_to:(`Tcp (v4, 0)) (`Unix "/tmp/s.sock"));
  Alcotest.(check string)
    "IPv6 for IPv4" "EAFNOSUPPORT"
    (raises ~bind_to:(`Tcp (v6, 0)) (`Tcp (v4, 80)));
  Alcotest.(check string)
    "a path for TCP" "EAFNOSUPPORT"
    (raises ~bind_to:(`Unix "/tmp/c.sock") (`Tcp (v4, 80)))

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
    (fails in_use (fun () ->
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
    (fails in_use (fun () ->
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
    (fails not_available (fun () ->
         ignore (Eio.Net.connect ~sw net addr : _ Eio.Net.stream_socket)));
  let bind_to = `Tcp (lo, 0) in
  Alcotest.(check bool)
    "a connect bound at port 0" true
    (fails in_use (fun () ->
         ignore
           (Eio.Net.connect ~sw ~bind_to net addr : _ Eio.Net.stream_socket)))

(* [n] short connections to [addr] that each leave the client's port in
   TIME_WAIT, as the client closes first; [false] once a connect finds no
   port. *)
let short_connections ~sw net l addr n =
  let rec go i =
    i = 0
    ||
    match Eio.Net.connect ~sw net addr with
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Address_not_available, _) -> false
    | c ->
        let s, _ = Eio.Net.accept ~sw l in
        Eio.Resource.close c;
        Eio.Resource.close s;
        go (i - 1)
  in
  go n

(* An unbound connect's port is unique to its destination, as on Linux, so
   20000 short connections to two listeners within a second pass, though
   their clients hold their ports in TIME_WAIT; one destination still runs
   out once its 16384 ports are held, with EADDRNOTAVAIL, while the other
   connects. *)
let test_ports_per_destination () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let a = `Tcp (lo, 8172) and b = `Tcp (lo, 8173) in
  let la = Eio.Net.listen ~sw ~backlog:1 net a in
  let lb = Eio.Net.listen ~sw ~backlog:1 net b in
  Alcotest.(check bool)
    "10000 to each" true
    (List.for_all Fun.id
       (List.init 20 (fun i ->
            if i mod 2 = 0 then short_connections ~sw net la a 1000
            else short_connections ~sw net lb b 1000)));
  Alcotest.(check bool)
    "6384 more to one" true
    (short_connections ~sw net la a 6384);
  Alcotest.(check bool)
    "one more to it" true
    (fails not_available (fun () ->
         ignore (Eio.Net.connect ~sw net a : _ Eio.Net.stream_socket)));
  Alcotest.(check bool)
    "one more to the other" true
    (short_connections ~sw net lb b 1)

(* A port in TIME_WAIT toward a destination is free again for a connect to
   it once a second has passed since the peer's FIN, as Linux reuses it on
   loopback with [tcp_tw_reuse] at its default, 2. A port held by a connect
   bound with [bind_to] stays its own, open or in TIME_WAIT. *)
let test_time_wait_reuse () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  let wait n =
    Eio_mock.Clock.Mono.set_time clock
      (Mtime.add_span (Eio.Time.Mono.now clock) Mtime.Span.(n * ms)
      |> Option.get)
  in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let a = `Tcp (lo, 8174) and b = `Tcp (lo, 8175) in
  let la = Eio.Net.listen ~sw ~backlog:1 net a in
  let lb = Eio.Net.listen ~sw ~backlog:1 net b in
  let bound =
    let c = Eio.Net.connect ~sw ~bind_to:(`Tcp (lo, 0)) net b in
    let s, peer = Eio.Net.accept ~sw lb in
    Eio.Resource.close c;
    Eio.Resource.close s;
    match peer with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  Alcotest.(check bool)
    "16383 to one" true
    (short_connections ~sw net la a 16383);
  let unavailable () =
    fails not_available (fun () ->
        ignore (Eio.Net.connect ~sw net a : _ Eio.Net.stream_socket))
  in
  Alcotest.(check bool) "at once" true (unavailable ());
  wait 1000;
  Alcotest.(check bool) "after 1 s" true (unavailable ());
  wait 1;
  let c = Eio.Net.connect ~sw net a in
  let _, peer = Eio.Net.accept ~sw la in
  Eio.Resource.close c;
  Alcotest.(check bool)
    "after 1.001 s, not at the bound port" true
    (match peer with `Tcp (_, p) -> p <> bound | `Unix _ -> false)

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
    fails in_use (listens net addr)
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
  Alcotest.(check bool) "an orphan at 59 s" true (fails in_use (listens net a));
  at 60;
  Alcotest.(check bool) "an orphan at 60 s" false (fails in_use (listens net a));
  Alcotest.(check bool)
    "the peer's FIN at 29 s" true
    (fails in_use (listens net b));
  at 89;
  Alcotest.(check bool)
    "TIME_WAIT from 29 s" false
    (fails in_use (listens net b))

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
    | exception
        Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EINVAL, "bind", _)), _) ->
        "EINVAL"
    | exception
        Eio.Io
          (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRNOTAVAIL, "bind", _)), _)
      ->
        "EADDRNOTAVAIL"
    | exception Eio.Io (Eio.Exn.X Eio_net_mem.Network_unreachable, _) ->
        "ENETUNREACH"
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
        "REFUSED"
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
  Alcotest.(check bool)
    "it holds the address" true
    (fails in_use (listens net addr));
  Eio.Flow.shutdown flow `Send;
  (match Eio.Flow.single_read flow buf with
  | exception End_of_file -> ()
  | n -> Alcotest.failf "read %d after its own FIN" n);
  let clock = Eio_mock.Clock.Mono.make () in
  let net = Eio_net_mem.v ~clock ~sw () in
  Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw ~bind_to:addr net addr));
  Alcotest.(check bool)
    "TIME_WAIT once closed" true
    (fails in_use (listens net addr))

let suite =
  ( "ports",
    [
      Alcotest.test_case "an accepted connection holds the port" `Quick
        test_accepted_holds_the_port;
      Alcotest.test_case "TIME_WAIT" `Quick test_time_wait;
      Alcotest.test_case "TIME_WAIT follows the first FIN" `Quick
        test_time_wait_follows_the_first_fin;
      Alcotest.test_case "FIN_WAIT2 times out" `Quick test_fin_wait2_times_out;
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
      Alcotest.test_case "ports per destination" `Quick
        test_ports_per_destination;
      Alcotest.test_case "TIME_WAIT reuse" `Quick test_time_wait_reuse;
      Alcotest.test_case "a wildcard destination" `Quick
        test_wildcard_destination;
    ] )
