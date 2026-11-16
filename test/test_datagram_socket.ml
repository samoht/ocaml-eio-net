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

(* A Unix-domain datagram socket that sends to its own path reads its
   datagram back, from itself, on Linux and on macOS. *)
let test_unix_dgram_self_send () =
  Eio_main.run @@ fun _ ->
  List.iter
    (fun os ->
      Eio.Switch.run @@ fun sw ->
      let net = Eio_net_mem.v ~os ~sw () in
      let self = `Unix "/tmp/self.sock" in
      let s = Eio.Net.datagram_socket ~sw net self in
      Eio.Net.send s ~dst:self [ Cstruct.of_string "me" ];
      let src, got = recv_string s in
      Alcotest.(check string) "read back" "me" got;
      Alcotest.check datagram_addr "from itself" self src)
    [ `Linux; `Macos ]

let suite =
  ( "datagram_socket",
    [
      Alcotest.test_case "a datagram roundtrip" `Quick test_datagram_roundtrip;
      Alcotest.test_case "datagrams keep their order" `Quick
        test_datagrams_keep_order;
      Alcotest.test_case "a datagram to nobody is lost" `Quick
        test_datagram_to_nobody_is_lost;
      Alcotest.test_case "a datagram address is freed at switch end" `Quick
        test_datagram_address_freed_at_switch_end;
      Alcotest.test_case "a datagram address in use" `Quick
        test_datagram_address_in_use;
      Alcotest.test_case "a datagram too long" `Quick test_datagram_too_long;
      Alcotest.test_case "a datagram reaches as a kernel delivers it" `Quick
        test_datagram_reaches_like_a_kernel;
      Alcotest.test_case "a closed datagram socket's path" `Quick
        test_closed_datagram_path;
      Alcotest.test_case "a UDP receive buffer drops past Linux's" `Quick
        test_udp_receive_buffer;
      Alcotest.test_case "a Unix-domain datagram send waits for room" `Quick
        test_unix_dgram_queue;
      Alcotest.test_case "a Unix-domain datagram sent to itself" `Quick
        test_unix_dgram_self_send;
      Alcotest.test_case "a datagram socket at \"\" autobinds" `Quick
        test_datagram_autobind;
      Alcotest.test_case "datagram sockets with reuse_addr" `Quick
        test_datagram_reuse_addr;
      Alcotest.test_case "the order of a datagram's errors" `Quick
        test_datagram_error_order;
    ] )
