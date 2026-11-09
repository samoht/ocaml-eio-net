(* Socket behaviour that gVisor's syscall tests pin, adapted from
   https://github.com/google/gvisor/tree/master/test/syscalls/linux
   (Apache License 2.0), run against eio-net.mem and real sockets.

   Each case keeps the name and file of the gVisor test it adapts. A gVisor
   test states Linux's behaviour; where the kernel's own answer is wrapped by
   Eio (a shutdown's ENOTCONN, which Eio ignores), the expectation is the
   answer an Eio program sees. *)

open Oracle

let all_kinds =
  [
    Stream Tcp4;
    Stream Tcp6;
    Stream Unix_stream;
    Dgram Udp4;
    Dgram Udp6;
    Dgram Unix_dgram;
  ]

let per_kind ?capacity ~file ~test kinds expect body =
  List.map
    (fun k ->
      case ?capacity ~file ~test ~variant:(kind_name k) ~ipv6:(needs_ipv6 k)
        (expect k) (fun ctx -> body ctx k))
    kinds

let per_stream ?capacity ~file ~test kinds expect body =
  per_kind ?capacity ~file ~test
    (List.map (fun k -> Stream k) kinds)
    (function Stream k -> expect k | Dgram _ -> assert false)
    (fun ctx -> function Stream k -> body ctx k | Dgram _ -> assert false)

let per_dgram ~file ~test kinds expect body =
  per_kind ~file ~test
    (List.map (fun k -> Dgram k) kinds)
    (function Dgram k -> expect k | Stream _ -> assert false)
    (fun ctx -> function Dgram k -> body ctx k | Stream _ -> assert false)

let intact sent got =
  if String.equal sent got then Fmt.str "%d bytes" (String.length got)
  else Fmt.str "%d bytes, corrupted" (String.length got)

(* socket_generic_test_cases.cc: AllSocketPairTest, over every pair kind. *)

let generic = "socket_generic_test_cases.cc"

(* macOS caps a UDP datagram at net.inet.udp.maxdgram (9216 bytes) and a
   Unix-domain one at net.local.dgram.maxdgram (2048 bytes). *)
let large_send n kind =
  let macos =
    match kind with
    | Dgram (Udp4 | Udp6) when n > 9216 -> Some "EMSGSIZE"
    | Dgram Unix_dgram when n > 2048 -> Some "EMSGSIZE"
    | _ -> None
  in
  Fmt.kstr (expect ?macos) "%d bytes" n

let generic_cases =
  List.concat
    [
      (* BasicReadWrite: three bytes written are read back. *)
      per_kind ~file:generic ~test:"BasicReadWrite" all_kinds
        (fun _ -> expect "abc")
        (fun ctx k ->
          let p = pair ctx k in
          p.send "abc";
          p.recv 3);
      (* BasicSendRecv: 512 bytes arrive in one receive. *)
      per_kind ~file:generic ~test:"BasicSendRecv" all_kinds
        (fun _ -> expect "512 bytes")
        (fun ctx k ->
          let p = pair ctx k in
          let data = payload 512 in
          p.send data;
          intact data (p.recv 512));
      (* SendmsgRecvmsg10KB and SendmsgRecvmsg16KB: a large message arrives
         whole. *)
      per_kind ~file:generic ~test:"SendmsgRecvmsg10KB" all_kinds
        (large_send 10240) (fun ctx k ->
          outcome @@ fun () ->
          let p = pair ctx k in
          let data = payload 10240 in
          (* macOS gives a Unix-domain stream 8 KiB of send buffer: the
             receive runs alongside the send. *)
          let got = ref "" in
          Eio.Fiber.both
            (fun () -> p.send data)
            (fun () -> got := p.recv_all 10240);
          intact data !got);
      per_kind ~file:generic ~test:"SendmsgRecvmsg16KB" all_kinds
        (large_send 16384) (fun ctx k ->
          outcome @@ fun () ->
          let p = pair ctx k in
          let data = payload 16384 in
          (* macOS gives a Unix-domain stream 8 KiB of send buffer: the
             receive runs alongside the send. *)
          let got = ref "" in
          Eio.Fiber.both
            (fun () -> p.send data)
            (fun () -> got := p.recv_all 16384);
          intact data !got);
      (* RecvWaitAll: a receive that waits for the whole buffer gets it. *)
      per_kind ~file:generic ~test:"RecvWaitAll" all_kinds
        (fun _ -> expect "100 bytes")
        (fun ctx k ->
          let p = pair ctx k in
          let data = payload 100 in
          p.send data;
          intact data (p.recv_all 100));
    ]

(* socket_stream.cc and socket_stream_blocking.cc: StreamSocketPairTest, which
   gVisor runs over Unix-domain stream pairs, and
   BlockingStreamSocketPairTest, over TCP and Unix-domain pairs. *)

let stream = "socket_stream.cc"
let stream_blocking = "socket_stream_blocking.cc"

(* A write on a Unix-domain stream whose peer has closed fails at once with
   EPIPE. *)
let write_one_side_closed ~file =
  per_stream ~file ~test:"WriteOneSideClosed" [ Unix_stream ]
    (fun _ -> expect "reset")
    (fun ctx k ->
      let c, a, _ = stream_pair ctx k in
      Eio.Flow.close c;
      outcome @@ fun () ->
      Fmt.str "wrote %d" (Eio.Flow.single_write a [ Cstruct.of_string "abc" ]))

let stream_cases =
  List.concat
    [
      (* SplitRecv: two receives of half the bytes get each half in turn. *)
      per_stream ~file:stream ~test:"SplitRecv" [ Unix_stream ]
        (fun _ -> expect "first half; second half")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let data = payload 512 in
          write c data;
          let first = read a 256 in
          let second = read a 256 in
          Fmt.str "%s; %s"
            (if first = String.sub data 0 256 then "first half" else "?")
            (if second = String.sub data 256 256 then "second half" else "?"));
      (* CoalescedRecv: two writes are read with one receive. *)
      per_stream ~file:stream ~test:"CoalescedRecv" [ Unix_stream ]
        (fun _ -> expect "40 bytes")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let one = payload 20 and two = payload ~seed:20 20 in
          write c one;
          write c two;
          intact (one ^ two) (read a 40));
      write_one_side_closed ~file:stream;
      (* RecvLessThanBuffer: a receive into a larger buffer returns what was
         written. *)
      per_stream ~file:stream_blocking ~test:"RecvLessThanBuffer"
        [ Tcp4; Unix_stream ]
        (fun _ -> expect "100 bytes")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let data = payload 100 in
          write c data;
          intact data (read a 200));
      (* RecvLessThanBufferWaitAll: a receive that waits for the whole buffer
         returns once a later write fills it. *)
      per_stream ~file:stream_blocking ~test:"RecvLessThanBufferWaitAll"
        [ Tcp4; Unix_stream ]
        (fun _ -> expect "200 bytes")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let data = payload 100 in
          write c data;
          let got = ref "" in
          Eio.Fiber.both
            (fun () -> got := read_exact a 200)
            (fun () -> write c data);
          intact (data ^ data) !got);
    ]

(* socket_unix_stream.cc and socket_unix_stream_listener_close.cc. *)

let unix_stream = "socket_unix_stream.cc"
let listener_close = "socket_unix_stream_listener_close.cc"

let unix_stream_cases =
  List.concat
    [
      write_one_side_closed ~file:unix_stream;
      (* ReadOneSideClosed: a read after the peer closed returns end of
         file. *)
      per_stream ~file:unix_stream ~test:"ReadOneSideClosed" [ Unix_stream ]
        (fun _ -> expect "EOF")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          Eio.Flow.close c;
          outcome (fun () -> read a 10));
      (* RecvmsgOneSideClosed: so does a read that waits for the whole
         buffer. *)
      per_stream ~file:unix_stream ~test:"RecvmsgOneSideClosed" [ Unix_stream ]
        (fun _ -> expect "EOF")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          Eio.Flow.close c;
          outcome (fun () -> read_exact a 10));
      (* ReadOneSideClosedWithUnreadData: a peer that shuts down both ways
         reads as end of file, and once it closes with bytes of ours unread,
         as a reset. macOS keeps reading end of file, and so does
         eio-net.mem, whose receive shutdown drops what is queued. *)
      per_stream ~file:unix_stream ~test:"ReadOneSideClosedWithUnreadData"
        [ Unix_stream ]
        (fun _ -> expect ~macos:"EOF; EOF" ~mem:"EOF; EOF" "EOF; reset")
        (fun ctx k ->
          let first, second, _ = stream_pair ctx k in
          write second (payload 10);
          Eio.Flow.shutdown first `All;
          let before = outcome (fun () -> read second 10) in
          Eio.Flow.close first;
          let after = outcome (fun () -> read second 10) in
          Fmt.str "%s; %s" before after);
      (* PendingConnectionGetsECONNRESET: a connection its listener closed
         before accepting it reads a reset once, then end of file. *)
      per_stream ~file:listener_close ~test:"PendingConnectionGetsECONNRESET"
        [ Unix_stream ]
        (fun _ -> expect ~macos:"EOF; EOF" "reset; EOF")
        (fun ctx k ->
          let l = listen ctx (stream_addr ctx k) in
          let c = connect ctx (Eio.Net.listening_addr l) in
          Eio.Net.close l;
          let first = outcome (fun () -> read c 10) in
          let second = outcome (fun () -> read c 10) in
          Fmt.str "%s; %s" first second);
    ]

(* socket_unix_dgram.cc, socket_unix_dgram_non_blocking.cc and
   socket_unix_non_stream.cc. gVisor's pairs are connected; Eio's datagram
   sockets are not, so each send names the peer's path. *)

let unix_dgram = "socket_unix_dgram.cc"
let unix_dgram_nb = "socket_unix_dgram_non_blocking.cc"
let unix_non_stream = "socket_unix_non_stream.cc"

let unix_dgram_cases =
  List.concat
    [
      (* WriteOneSideClosed: a send to a peer that has closed is refused. The
         peer's file stays: Eio's POSIX backend removes no datagram socket's
         path. *)
      per_dgram ~file:unix_dgram ~test:"WriteOneSideClosed" [ Unix_dgram ]
        (fun _ -> expect "refused")
        (fun ctx k ->
          let a, a_addr = dgram_socket ctx k in
          let b, _ = dgram_socket ctx k in
          Eio.Net.close a;
          outcome (fun () ->
              send b ~dst:a_addr "abc";
              "sent"));
      (* ReadOneSideClosed: a receive after the peer closed waits. *)
      per_dgram ~file:unix_dgram_nb ~test:"ReadOneSideClosed" [ Unix_dgram ]
        (fun _ -> expect "blocked")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, _ = dgram_socket ctx k in
          Eio.Net.close a;
          outcome_now (fun () -> recv b 10));
      (* PeerWriteShutdown: after shutting down its sending side a socket's
         sends fail with EPIPE; what it sent before is still received, and
         nothing after. On macOS the shutdown of an unconnected socket fails
         with ENOTCONN, which Eio ignores, so the second send goes through. *)
      per_dgram ~file:unix_non_stream ~test:"PeerWriteShutdown" [ Unix_dgram ]
        (fun _ ->
          expect ~macos:"sent; abc; abc" ~mem:"sent; abc; abc"
            "reset; abc; blocked")
        (fun ctx k ->
          let a, a_addr = dgram_socket ctx k in
          let b, _ = dgram_socket ctx k in
          send b ~dst:a_addr "abc";
          dgram_shutdown b `Send;
          let again =
            outcome (fun () ->
                send b ~dst:a_addr "abc";
                "sent")
          in
          let first = outcome (fun () -> recv a 3) in
          let second = outcome_now (fun () -> recv a 3) in
          Fmt.str "%s; %s; %s" again first second);
    ]

(* socket_non_stream.cc and socket_non_stream_blocking.cc:
   NonStreamSocketPairTest, over UDP and Unix-domain datagram pairs. *)

let non_stream = "socket_non_stream.cc"
let non_stream_blocking = "socket_non_stream_blocking.cc"
let dgram_kinds = [ Udp4; Udp6; Unix_dgram ]

let non_stream_cases =
  List.concat
    [
      (* SendMsgTooLarge: a datagram larger than the send buffer is refused
         with EMSGSIZE. gVisor sends three times SO_SNDBUF; 1 MiB is more than
         any platform's default. *)
      per_dgram ~file:non_stream ~test:"SendMsgTooLarge" dgram_kinds
        (fun _ ->
          expect ~mem:"sent"
            ~mem_bug:
              "Refuse a datagram longer than a socket could send (65507 bytes \
               of UDP payload over IPv4) with an EMSGSIZE error instead of \
               queueing it."
            "EMSGSIZE")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let _, b_addr = dgram_socket ctx k in
          outcome (fun () ->
              send a ~dst:b_addr (String.make (1024 * 1024) 'a');
              "sent"));
      (* SplitRecv: a receive into half a datagram's length gets its first
         half, and the rest is discarded. *)
      per_dgram ~file:non_stream ~test:"SplitRecv" dgram_kinds
        (fun _ -> expect "first half; blocked")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          let data = payload 512 in
          send a ~dst:b_addr data;
          let first = recv b 256 in
          Fmt.str "%s; %s"
            (if first = String.sub data 0 256 then "first half" else "?")
            (outcome_now (fun () -> recv b 256)));
      (* SingleRecv: a receive gets one datagram, never two. *)
      per_dgram ~file:non_stream ~test:"SingleRecv" dgram_kinds
        (fun _ -> expect "20 bytes")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          let one = payload 20 in
          send a ~dst:b_addr one;
          send a ~dst:b_addr (payload ~seed:20 20);
          intact one (recv b 40));
      (* RecvLessThanBufferWaitAll: on a datagram socket, a receive that waits
         for the whole buffer returns one datagram. *)
      per_dgram ~file:non_stream_blocking ~test:"RecvLessThanBufferWaitAll"
        dgram_kinds
        (fun _ -> expect "100 bytes")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          let data = payload 100 in
          send a ~dst:b_addr data;
          intact data (recv b 200));
    ]

(* udp_socket.cc: UdpSocketTest, over IPv4 and IPv6. gVisor's sockets are
   bound or connected by hand; Eio's are bound when they are made, and never
   connected. *)

let udp = "udp_socket.cc"
let udp_kinds = [ Udp4; Udp6 ]

let unbound = function
  | Udp4 -> `UdpV4
  | Udp6 -> `UdpV6
  | Unix_dgram -> assert false

let loopback_of = function
  | Udp4 -> v4_loopback
  | Udp6 -> v6_loopback
  | Unix_dgram -> assert false

let udp_cases =
  List.concat
    [
      (* SendNotConnected: a send without a destination on a socket that is
         not connected fails with EDESTADDRREQ; a send with one goes. *)
      per_dgram ~file:udp ~test:"SendNotConnected" udp_kinds
        (fun _ -> expect "EDESTADDRREQ; sent")
        (fun ctx k ->
          let a, a_addr = dgram_socket ctx k in
          let without =
            outcome (fun () ->
                Eio.Net.send a [ Cstruct.of_string "abc" ];
                "sent")
          in
          let with_dst =
            outcome (fun () ->
                send a ~dst:a_addr "abc";
                "sent")
          in
          Fmt.str "%s; %s" without with_dst);
      (* BindInUse: a second socket cannot bind a held address. *)
      per_dgram ~file:udp ~test:"BindInUse" udp_kinds
        (fun _ -> expect "EADDRINUSE")
        (fun ctx k ->
          let _, addr = dgram_socket ctx k in
          outcome (fun () ->
              ignore (datagram_socket ctx addr);
              "bound"));
      (* ReceiveNotBound: a receive on an unbound socket waits. *)
      per_dgram ~file:udp ~test:"ReceiveNotBound" udp_kinds
        (fun _ -> expect "blocked")
        (fun ctx k ->
          let s : dgram =
            Eio.Net.datagram_socket ~sw:ctx.sw ctx.net (unbound k)
          in
          outcome_now (fun () -> recv s 512));
      (* ZerolengthWriteAllowed: an empty datagram is sent and received. It
         is sent as one empty buffer, as write(2) with a length of 0 sends
         it: on macOS a sendmsg(2) with no buffer at all fails with
         EMSGSIZE. *)
      per_dgram ~file:udp ~test:"ZerolengthWriteAllowed" udp_kinds
        (fun _ -> expect "0 bytes")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          outcome @@ fun () ->
          Eio.Net.send a ~dst:b_addr [ Cstruct.empty ];
          Fmt.str "%d bytes" (String.length (recv b 3)));
      (* SendAndReceiveNotConnected: a datagram sent to a bound socket is
         received. *)
      per_dgram ~file:udp ~test:"SendAndReceiveNotConnected" udp_kinds
        (fun _ -> expect "512 bytes")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          let data = payload 512 in
          send a ~dst:b_addr data;
          intact data (recv b 512));
      (* ReceiveFrom: a receive reports the sender's address. *)
      per_dgram ~file:udp ~test:"ReceiveFrom" udp_kinds
        (fun _ -> expect "from the sender")
        (fun ctx k ->
          let a, a_addr = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          send a ~dst:b_addr "abc";
          let from, _ = Eio.Net.recv b (Cstruct.create 3) in
          let pp = Fmt.to_to_string Eio.Net.Sockaddr.pp in
          if String.equal (pp from) (pp a_addr) then "from the sender"
          else Fmt.str "from %a" Eio.Net.Sockaddr.pp from);
      (* BoundaryPreserved_SendRecv: three datagrams are received as three. *)
      per_dgram ~file:udp ~test:"BoundaryPreserved_SendRecv" udp_kinds
        (fun _ -> expect "3 datagrams of 100 bytes")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let b, b_addr = dgram_socket ctx k in
          let sent = List.init 3 (fun i -> payload ~seed:(100 * i) 100) in
          List.iter (send a ~dst:b_addr) sent;
          let got = List.init 3 (fun _ -> recv b 100) in
          if List.sort String.compare got = List.sort String.compare sent then
            "3 datagrams of 100 bytes"
          else "different datagrams");
      (* SendToZeroPort: a send to port 0 fails with EINVAL; to a port no
         socket binds it goes. macOS sends to port 0 over IPv4. *)
      per_dgram ~file:udp ~test:"SendToZeroPort" udp_kinds
        (function
          | Udp4 -> expect ~macos:"sent; sent" ~mem:"sent; sent" "EINVAL; sent"
          | _ ->
              expect ~macos:"EADDRNOTAVAIL; sent" ~mem:"sent; sent"
                ~mem_bug:"Refuse a send to port 0 with an EINVAL error."
                "EINVAL; sent")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let to_port port =
            outcome (fun () ->
                send a ~dst:(`Udp (loopback_of k, port)) "abcdefgh";
                "sent")
          in
          let zero = to_port 0 in
          Fmt.str "%s; %s" zero (to_port 1234));
      (* ReadShutdownOnBoundSocket: shutting down a bound socket that is not
         connected fails with ENOTCONN, which Eio ignores. *)
      per_dgram ~file:udp ~test:"ReadShutdownOnBoundSocket" udp_kinds
        (fun _ -> expect "ok; ok; ok")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          String.concat "; "
            (List.map
               (fun cmd ->
                 outcome (fun () ->
                     dgram_shutdown a cmd;
                     "ok"))
               [ `Receive; `Send; `All ]));
      (* WriteShutdownNotConnected: so does shutting down an unbound one. *)
      per_dgram ~file:udp ~test:"WriteShutdownNotConnected" udp_kinds
        (fun _ -> expect "ok")
        (fun ctx k ->
          let s : dgram =
            Eio.Net.datagram_socket ~sw:ctx.sw ctx.net (unbound k)
          in
          outcome (fun () ->
              dgram_shutdown s `Send;
              "ok"));
      (* ReadShutdown, up to its connect: a receive waits before and after a
         read shutdown that fails with ENOTCONN. *)
      per_dgram ~file:udp ~test:"ReadShutdown" udp_kinds
        (fun _ -> expect "blocked; ok; blocked")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          let before = outcome_now (fun () -> recv a 512) in
          let shut =
            outcome (fun () ->
                dgram_shutdown a `Receive;
                "ok")
          in
          let after = outcome_now (fun () -> recv a 512) in
          Fmt.str "%s; %s; %s" before shut after);
      (* WriteShutdown, up to its connect: a write shutdown of a bound socket
         that is not connected fails with ENOTCONN. *)
      per_dgram ~file:udp ~test:"WriteShutdown" udp_kinds
        (fun _ -> expect "ok")
        (fun ctx k ->
          let a, _ = dgram_socket ctx k in
          outcome (fun () ->
              dgram_shutdown a `Send;
              "ok"));
    ]

(* tcp_socket.cc: TcpSocketTest, over a connected pair, and
   SimpleTcpSocketTest, over IPv4 and IPv6. *)

let tcp = "tcp_socket.cc"
let tcp_kinds = [ Tcp4; Tcp6 ]

let ip_of = function
  | Tcp4 -> v4_loopback
  | Tcp6 -> v6_loopback
  | Unix_stream -> assert false

let tcp_cases =
  List.concat
    [
      (* ConnectedAcceptedPeerAndLocalAreReciprocals: the accepted end's peer
         is the connecting end's local address. Eio reports the first only,
         from accept: a TCP address on loopback. *)
      per_stream ~file:tcp ~test:"ConnectedAcceptedPeerAndLocalAreReciprocals"
        tcp_kinds
        (fun k ->
          expect ~mem:"unix:in-memory-client"
            ~mem_bug:
              "Give each accepted connection a TCP peer address, the \
               listener's loopback address and a port of the dynamic range."
            ("tcp:" ^ pp_ip (ip_of k)))
        (fun ctx k ->
          let l = listen ctx (stream_addr ctx k) in
          let _c = connect ctx (Eio.Net.listening_addr l) in
          let _, peer = Eio.Net.accept ~sw:ctx.sw l in
          match peer with
          | `Tcp (ip, _) -> "tcp:" ^ pp_ip ip
          | `Unix path -> "unix:" ^ path);
      (* DataCoalesced: two writes are read with one receive once both have
         arrived. *)
      per_stream ~file:tcp ~test:"DataCoalesced" tcp_kinds
        (fun _ -> expect "10 bytes")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let data = payload 10 in
          write c (String.sub data 0 5);
          write c (String.sub data 5 5);
          wait_queued ctx a 10;
          intact data (read a 10));
      (* ZeroWriteAllowed: an empty write writes nothing, and the peer has
         nothing to read. *)
      per_stream ~file:tcp ~test:"ZeroWriteAllowed" tcp_kinds
        (fun _ -> expect "wrote 0; blocked")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let n = Eio.Flow.single_write c [ Cstruct.empty ] in
          Fmt.str "wrote %d; %s" n (outcome_now (fun () -> read a 3)));
      (* WritevZeroIovec: a vector that ends with an empty buffer writes the
         others. *)
      per_stream ~file:tcp ~test:"WritevZeroIovec" tcp_kinds
        (fun _ -> expect "wrote 1; x")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let n =
            Eio.Flow.single_write c [ Cstruct.of_string "x"; Cstruct.empty ]
          in
          Fmt.str "wrote %d; %s" n (read a 1));
      (* NonblockingLargeWrite: a write of more than the send buffer holds
         writes part of it. gVisor writes three times SO_SNDBUF; 16 MiB is
         more than any platform's default buffers. eio-net.mem bounds its
         queues only when asked, so it runs with a socket buffer's 64 KiB. *)
      per_stream ~capacity:65536 ~file:tcp ~test:"NonblockingLargeWrite"
        tcp_kinds
        (fun _ -> expect "part of it")
        (fun ctx k ->
          let c, _, _ = stream_pair ctx k in
          let size = 16 * 1024 * 1024 in
          let n = Eio.Flow.single_write c [ Cstruct.create size ] in
          if n = size then "all of it"
          else if n > 0 then "part of it"
          else "none");
      (* ShutdownWriteInFinWait1: shutting down the sending side again, and
         once more after the peer has seen it, succeeds. The peer reading end
         of file stands for gVisor's sleep. *)
      per_stream ~file:tcp ~test:"ShutdownWriteInFinWait1" tcp_kinds
        (fun _ -> expect "ok; ok; EOF; ok")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          let shut () =
            outcome (fun () ->
                Eio.Flow.shutdown a `Send;
                "ok")
          in
          let one = shut () in
          let two = shut () in
          let eof = outcome (fun () -> read c 1) in
          Fmt.str "%s; %s; %s; %s" one two eof (shut ()));
      (* ShutdownWriteInTimeWait: once both ends have shut down, a further
         shutdown fails with ENOTCONN, which Eio ignores. The connecting end
         reads the accepted end's end of file before it shuts down, in place
         of gVisor's sleep. macOS then sends no FIN: its shutdown(SHUT_RDWR)
         fails with ENOTCONN once the receiving side has seen end of file,
         before it shuts the sending side, and Eio ignores that error. *)
      per_stream ~file:tcp ~test:"ShutdownWriteInTimeWait" tcp_kinds
        (fun _ -> expect ~macos:"EOF; blocked; ok" "EOF; EOF; ok")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          Eio.Flow.shutdown a `Send;
          let c_eof = outcome (fun () -> read c 1) in
          Eio.Flow.shutdown c `All;
          let a_eof = outcome_now (fun () -> read a 1) in
          Fmt.str "%s; %s; %s" c_eof a_eof
            (outcome (fun () ->
                 Eio.Flow.shutdown a `Send;
                 "ok")));
      (* BlockingConnectRefused: a connect to a port no listener holds is
         refused. *)
      per_stream ~file:tcp ~test:"BlockingConnectRefused" tcp_kinds
        (fun _ -> expect "refused")
        (fun ctx k ->
          let l = listen ctx (stream_addr ctx k) in
          let addr = Eio.Net.listening_addr l in
          Eio.Net.close l;
          outcome (fun () ->
              ignore (connect ctx addr);
              "connected"));
      (* ConnectUnspecifiedAddress: a connect to the unspecified address and
         port 0 is refused, and over IPv6 also to its IPv4-mapped form. macOS
         has no route to it. *)
      per_stream ~file:tcp ~test:"ConnectUnspecifiedAddress" tcp_kinds
        (fun _ -> expect ~macos:"EADDRNOTAVAIL" "refused")
        (fun ctx k ->
          let ips =
            match k with
            | Tcp4 -> [ Eio.Net.Ipaddr.V4.any ]
            | Tcp6 -> [ Eio.Net.Ipaddr.V6.any; v4_mapped_any ]
            | Unix_stream -> assert false
          in
          String.concat "; "
            (List.map
               (fun ip ->
                 outcome (fun () ->
                     ignore (connect ctx (`Tcp (ip, 0)));
                     "connected"))
               ips)
          |> function
          | "refused; refused" -> "refused"
          | "EADDRNOTAVAIL; EADDRNOTAVAIL" -> "EADDRNOTAVAIL"
          | s -> s);
    ]

(* socket_inet_loopback.cc. *)

let inet = "socket_inet_loopback.cc"

type addr =
  | V4_any
  | V4_loopback
  | V4_mapped_any
  | V4_mapped_loopback
  | V6_any
  | V6_loopback

let addr_name = function
  | V4_any -> "V4Any"
  | V4_loopback -> "V4Loopback"
  | V4_mapped_any -> "V4MappedAny"
  | V4_mapped_loopback -> "V4MappedLoopback"
  | V6_any -> "V6Any"
  | V6_loopback -> "V6Loopback"

let ip_of_addr = function
  | V4_any -> Eio.Net.Ipaddr.V4.any
  | V4_loopback -> v4_loopback
  | V4_mapped_any -> v4_mapped_any
  | V4_mapped_loopback -> v4_mapped_loopback
  | V6_any -> Eio.Net.Ipaddr.V6.any
  | V6_loopback -> v6_loopback

let is_v4 = function V4_any | V4_loopback -> true | _ -> false

(* SocketInetLoopbackTestValues: the listener and connector address pairs. *)
let inet_params =
  [
    (V4_any, V4_any);
    (V4_any, V4_loopback);
    (V4_any, V4_mapped_any);
    (V4_any, V4_mapped_loopback);
    (V4_loopback, V4_any);
    (V4_loopback, V4_loopback);
    (V4_loopback, V4_mapped_loopback);
    (V4_mapped_any, V4_any);
    (V4_mapped_any, V4_loopback);
    (V4_mapped_any, V4_mapped_any);
    (V4_mapped_any, V4_mapped_loopback);
    (V4_mapped_loopback, V4_any);
    (V4_mapped_loopback, V4_loopback);
    (V4_mapped_loopback, V4_mapped_loopback);
    (V6_any, V4_any);
    (V6_any, V4_loopback);
    (V6_any, V4_mapped_any);
    (V6_any, V4_mapped_loopback);
    (V6_any, V6_any);
    (V6_any, V6_loopback);
    (V6_loopback, V6_any);
    (V6_loopback, V6_loopback);
  ]

let param_name (l, c) = Fmt.str "Listen%s_Connect%s" (addr_name l) (addr_name c)
let needs_v6 (l, c) = not (is_v4 l && is_v4 c)

(* tcpSimpleConnectTest: a connect from [connector] reaches a listener at
   [listen_addr], and both shut down. *)
let simple_connect ctx listen_addr connector =
  outcome @@ fun () ->
  let l = listen ctx listen_addr in
  let port =
    match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  let c = connect ctx (`Tcp (ip_of_addr connector, port)) in
  let a, _ = Eio.Net.accept ~sw:ctx.sw l in
  Eio.Flow.shutdown c `All;
  Eio.Flow.close a;
  "connected"

(* A mem connect finds its listener at the address it names or at the
   wildcard address of that address's family, so an IPv4 address and its
   IPv4-mapped IPv6 form never meet, an IPv6 wildcard listener does not take
   IPv4 connections, and a connect to a wildcard address reaches no
   loopback listener. [mem_connect] says which pairs meet. *)
let mem_dual_stack =
  "Treat an IPv4-mapped IPv6 address as the IPv4 address it maps, and let an \
   IPv6 wildcard socket hold its port at IPv4 addresses too, as a dual-stack \
   socket does."

let mem_connect_bug =
  "Treat an IPv4-mapped IPv6 address as the IPv4 address it maps, let an IPv6 \
   wildcard listener take IPv4 connections, and send a connect to a wildcard \
   address to the loopback address of its family, as both kernels do."

let mem_connect (l, c) =
  let family_any = function
    | V4_any | V4_loopback -> `V4
    | V4_mapped_any | V4_mapped_loopback | V6_any | V6_loopback -> `V6
  in
  let any_l = match l with V4_any | V6_any -> true | _ -> false in
  let same = l = c in
  if same || (any_l && family_any l = family_any c) then "connected"
  else "refused"

let tcp_listen_expect param =
  let mem = mem_connect param in
  if mem = "connected" then expect "connected"
  else expect ~mem ~mem_bug:mem_connect_bug "connected"

let inet_cases =
  List.concat
    [
      (* TCP: a listener bound to no address, the wildcard address of its
         family, takes a connect from each connector. *)
      List.map
        (fun ((l, c) as param) ->
          let listen_ip =
            if is_v4 l then Eio.Net.Ipaddr.V4.any else Eio.Net.Ipaddr.V6.any
          in
          let mem_param = ((if is_v4 l then V4_any else V6_any), c) in
          case ~file:inet ~test:"TCP" ~variant:(param_name param)
            ~ipv6:(needs_v6 param) (tcp_listen_expect mem_param) (fun ctx ->
              simple_connect ctx (`Tcp (listen_ip, 0)) c))
        inet_params;
      (* TCPListenUnbound: so does a listener bound to its address. *)
      List.map
        (fun ((l, c) as param) ->
          case ~file:inet ~test:"TCPListenUnbound" ~variant:(param_name param)
            ~ipv6:(needs_v6 param) (tcp_listen_expect param) (fun ctx ->
              simple_connect ctx (`Tcp (ip_of_addr l, 0)) c))
        inet_params;
      (* LoopbackAddressRangeConnect: a wildcard listener takes a connect to
         any address of 127.0.0.0/8. macOS assigns 127.0.0.1 only, and a
         connect to another address of the range never completes, so the case
         does not try it there. *)
      List.map
        (fun ip ->
          let addr = Eio.Net.Ipaddr.of_raw ip in
          let only_one = ip = "\127\000\000\001" in
          case ~file:inet ~test:"LoopbackAddressRangeConnect"
            ~variant:(pp_ip addr)
            (if only_one then expect "connected"
             else expect ~macos:"never completes" "connected")
            (fun ctx ->
              if ctx.backend = Real && platform = Macos && not only_one then
                "never completes"
              else
                outcome @@ fun () ->
                let l = listen ctx (`Tcp (Eio.Net.Ipaddr.V4.any, 0)) in
                let port =
                  match Eio.Net.listening_addr l with
                  | `Tcp (_, p) -> p
                  | `Unix _ -> 0
                in
                let c = connect ctx (`Tcp (addr, port)) in
                let a, _ = Eio.Net.accept ~sw:ctx.sw l in
                Eio.Flow.shutdown c `All;
                Eio.Flow.close a;
                "connected"))
        [
          "\127\000\000\001";
          "\127\000\000\002";
          "\127\000\001\001";
          "\127\001\001\001";
          "\127\254\255\255";
        ];
      (* V6ListenerV4Connect and V4ListenerV6Connect: a TCP socket bound to
         an address of one family cannot connect to the other's. *)
      [
        case ~file:inet ~test:"V6ListenerV4Connect" ~variant:"TCP" ~ipv6:true
          (expect ~macos:"EAFNOSUPPORT" ~mem:"unsupported"
             ~mem_bug:
               "Accept bind_to, and refuse a connect to the other family."
             "ENETUNREACH") (fun ctx ->
            outcome @@ fun () ->
            let l = listen ctx (`Tcp (v4_loopback, 0)) in
            let port =
              match Eio.Net.listening_addr l with
              | `Tcp (_, p) -> p
              | `Unix _ -> 0
            in
            ignore
              (Eio.Net.connect ~sw:ctx.sw
                 ~bind_to:(`Tcp (v6_loopback, 0))
                 ctx.net
                 (`Tcp (v4_mapped_loopback, port)));
            "connected");
        case ~file:inet ~test:"V4ListenerV6Connect" ~variant:"TCP" ~ipv6:true
          (expect ~mem:"unsupported"
             ~mem_bug:
               "Accept bind_to, and refuse a connect to the other family."
             "EAFNOSUPPORT") (fun ctx ->
            outcome @@ fun () ->
            let l = listen ctx (`Tcp (v6_loopback, 0)) in
            let port =
              match Eio.Net.listening_addr l with
              | `Tcp (_, p) -> p
              | `Unix _ -> 0
            in
            ignore
              (Eio.Net.connect ~sw:ctx.sw
                 ~bind_to:(`Tcp (v4_mapped_loopback, 0))
                 ctx.net
                 (`Tcp (v6_loopback, port)));
            "connected");
      ];
      (* UdpV6ListenerV4SendTo and UdpV4ListenerV6SendTo: a UDP socket bound
         to an address of one family cannot send to the other's. *)
      [
        case ~file:inet ~test:"UdpV6ListenerV4SendTo" ~ipv6:true
          (expect ~macos:"EADDRNOTAVAIL" ~mem:"sent"
             ~mem_bug:
               "Refuse a send from an IPv6 address to an IPv4-mapped one, as \
                both kernels do."
             "ENETUNREACH") (fun ctx ->
            let s, _ = datagram_socket ctx (`Udp (v6_loopback, 0)) in
            outcome (fun () ->
                send s ~dst:(`Udp (v4_mapped_loopback, 1337)) "hello";
                "sent"));
        case ~file:inet ~test:"UdpV4ListenerV6SendTo" ~ipv6:true
          (expect ~macos:"EHOSTUNREACH" ~mem:"sent"
             ~mem_bug:
               "Refuse a send from an IPv4-mapped address to an IPv6 one, as \
                both kernels do."
             "EAFNOSUPPORT") (fun ctx ->
            let s, _ = datagram_socket ctx (`Udp (v4_mapped_loopback, 0)) in
            outcome (fun () ->
                send s ~dst:(`Udp (v6_loopback, 1337)) "hello";
                "sent"));
      ];
      (* DualStackV6AnyReservesEverything: an IPv6 wildcard socket holds its
         port at every address of both families. *)
      (let others = [ V6_loopback; V4_mapped_loopback; V4_loopback; V4_any ] in
       let bind_all bind = String.concat "; " (List.map bind others) in
       [
         case ~file:inet ~test:"DualStackV6AnyReservesEverything" ~variant:"TCP"
           ~ipv6:true
           (expect ~mem:"EADDRINUSE; EADDRINUSE; bound; EADDRINUSE"
              ~mem_bug:mem_dual_stack
              "EADDRINUSE; EADDRINUSE; EADDRINUSE; EADDRINUSE") (fun ctx ->
             let l = listen ctx (`Tcp (Eio.Net.Ipaddr.V6.any, 0)) in
             let port =
               match Eio.Net.listening_addr l with
               | `Tcp (_, p) -> p
               | `Unix _ -> 0
             in
             bind_all (fun a ->
                 outcome (fun () ->
                     ignore (listen ctx (`Tcp (ip_of_addr a, port)));
                     "bound")));
         case ~file:inet ~test:"DualStackV6AnyReservesEverything" ~variant:"UDP"
           ~ipv6:true
           (expect ~mem:"EADDRINUSE; EADDRINUSE; bound; EADDRINUSE"
              ~mem_bug:mem_dual_stack
              "EADDRINUSE; EADDRINUSE; EADDRINUSE; EADDRINUSE") (fun ctx ->
             let _, addr =
               datagram_socket ctx (`Udp (Eio.Net.Ipaddr.V6.any, 0))
             in
             let port = match addr with `Udp (_, p) -> p | `Unix _ -> 0 in
             bind_all (fun a ->
                 outcome (fun () ->
                     ignore (datagram_socket ctx (`Udp (ip_of_addr a, port)));
                     "bound")));
       ]);
      (* TCPResetAfterClose: after the peer closed, a first write goes and
         draws a reset; the next write fails with EPIPE, and reads return end
         of file. The writes are tried until one fails, in place of gVisor's
         sleep. macOS reports the reset to one read before end of file. *)
      per_stream ~file:inet ~test:"TCPResetAfterClose" tcp_kinds
        (fun _ ->
          expect ~macos:"wrote; reset; reset; EOF" "wrote; reset; EOF; EOF")
        (fun ctx k ->
          let c, a, _ = stream_pair ctx k in
          Eio.Flow.close c;
          let data = Cstruct.of_string "1234" in
          let first =
            outcome (fun () ->
                ignore (Eio.Flow.single_write a [ data ]);
                "wrote")
          in
          let rec until_refused n =
            if n = 0 then "never refused"
            else
              match Eio.Flow.single_write a [ data ] with
              | _ ->
                  Eio.Fiber.yield ();
                  until_refused (n - 1)
              | exception e -> classify e
          in
          let later = until_refused 1000 in
          let r1 = outcome (fun () -> read a 4) in
          let r2 = outcome (fun () -> read a 4) in
          Fmt.str "%s; %s; %s; %s" first later r1 r2);
      (* TCPListenCloseDuringConnect: a connection its listener closes before
         accepting it is reset. *)
      per_stream ~file:inet ~test:"TCPListenCloseDuringConnect" tcp_kinds
        (fun _ -> expect "reset")
        (fun ctx k ->
          let l = listen ~backlog:0 ctx (stream_addr ctx k) in
          let c = connect ctx (Eio.Net.listening_addr l) in
          Eio.Net.close l;
          outcome (fun () -> read c 1));
      (* TCPBacklogAcceptAll: with a backlog of one, a connection beyond it
         waits and is accepted once the first one is. macOS resets it unless
         the first accept comes before its stack handles the connect. *)
      per_stream ~file:inet ~test:"TCPBacklogAcceptAll" tcp_kinds
        (fun _ -> expect ~macos:"accepted 1; reset | accepted 2" "accepted 2")
        (fun ctx k ->
          let l = listen ~backlog:1 ctx (stream_addr ctx k) in
          let addr = Eio.Net.listening_addr l in
          let _established = connect ctx addr in
          let waiting = ref "" in
          Eio.Fiber.both
            (fun () ->
              waiting :=
                outcome (fun () ->
                    write (connect ctx addr) "c";
                    "connected"))
            (fun () -> ignore (Eio.Net.accept ~sw:ctx.sw l));
          match !waiting with
          | "connected" ->
              ignore (Eio.Net.accept ~sw:ctx.sw l);
              "accepted 2"
          | e -> "accepted 1; " ^ e);
    ]

let suite =
  Oracle.suite "gvisor pairs" (generic_cases @ stream_cases @ non_stream_cases)

let unix_suite =
  Oracle.suite "gvisor unix" (unix_stream_cases @ unix_dgram_cases)

let udp_suite = Oracle.suite "gvisor udp" udp_cases
let tcp_suite = Oracle.suite "gvisor tcp" (tcp_cases @ inet_cases)
