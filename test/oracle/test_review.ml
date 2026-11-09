(* Socket behaviour a review of eio-net.mem raised, measured on Linux and
   macOS. A case names what it checks; none comes from gVisor. *)

open Oracle

let file = "review"

let per_stream ~test kinds expect body =
  List.map
    (fun k ->
      case ~file ~test ~variant:(kind_name (Stream k))
        ~ipv6:(needs_ipv6 (Stream k)) (expect k) (fun ctx -> body ctx k))
    kinds

(* A loopback port no socket holds: a listener's, closed before any
   connection. *)
let free_port ctx =
  let l = listen ctx (`Tcp (v4_loopback, 0)) in
  let port =
    match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0
  in
  Eio.Net.close l;
  port

let cases =
  List.concat
    [
      (* An empty write after our own send shutdown writes nothing on Linux;
         macOS fails it with EPIPE. *)
      per_stream ~test:"EmptyWriteAfterShutdownSend" [ Tcp4; Unix_stream ]
        (fun _ -> expect ~macos:"reset" "wrote 0")
        (fun ctx k ->
          let c, _, _ = stream_pair ctx k in
          Eio.Flow.shutdown c `Send;
          outcome (fun () ->
              Fmt.str "wrote %d" (Eio.Flow.single_write c [ Cstruct.empty ])));
      (* A connect bound to an address of another family fails: EINVAL on
         Linux, EAFNOSUPPORT on macOS. A path for TCP fails with EAFNOSUPPORT
         on Linux 7.2, where the review measured EINVAL on another Linux. *)
      [
        case ~file ~test:"BindToOtherFamily" ~variant:"127.0.0.1 to ::1"
          ~ipv6:true (expect ~macos:"EAFNOSUPPORT" ~mem:"EAFNOSUPPORT" "EINVAL")
          (fun ctx ->
            let l = listen ctx (`Tcp (v6_loopback, 0)) in
            outcome (fun () ->
                ignore
                  (Eio.Net.connect ~sw:ctx.sw
                     ~bind_to:(`Tcp (v4_loopback, 0))
                     ctx.net (Eio.Net.listening_addr l));
                "connected"));
        case ~file ~test:"BindToOtherFamily" ~variant:"unix path to tcp"
          (expect ~macos:"EAFNOSUPPORT" ~mem:"EAFNOSUPPORT"
             "EAFNOSUPPORT | EINVAL") (fun ctx ->
            let l = listen ctx (`Tcp (v4_loopback, 0)) in
            let bind_to = stream_addr ctx Unix_stream in
            outcome (fun () ->
                ignore
                  (Eio.Net.connect ~sw:ctx.sw ~bind_to ctx.net
                     (Eio.Net.listening_addr l));
                "connected"));
      ];
      (* A datagram sent to an address of another family or socket type:
         from IPv4 to IPv6 it fails; from IPv6 to IPv4 it takes the IPv4 path
         on Linux, failing from ::1 and going from ::, where macOS fails it
         with EINVAL; between UDP and a Unix path it fails, with EAFNOSUPPORT
         on Linux 7.2 where the review measured EINVAL on another Linux. *)
      List.map
        (fun (variant, ipv6, src, dst, e) ->
          case ~file ~test:"DatagramAcrossFamilies" ~variant ~ipv6 e (fun ctx ->
              let r, r_addr =
                match dst with
                | `Udp ip -> datagram_socket ctx (`Udp (ip, 0))
                | `Unix -> dgram_socket ctx Unix_dgram
              in
              let s, _ =
                match src with
                | `Udp ip -> datagram_socket ctx (`Udp (ip, 0))
                | `Unix -> dgram_socket ctx Unix_dgram
              in
              outcome (fun () ->
                  send s ~dst:r_addr "abc";
                  ignore (recv r 3);
                  "delivered")))
        [
          ( "127.0.0.1 to ::1",
            true,
            `Udp v4_loopback,
            `Udp v6_loopback,
            expect ~macos:"EHOSTUNREACH" "EAFNOSUPPORT" );
          ( "::1 to 127.0.0.1",
            true,
            `Udp v6_loopback,
            `Udp v4_loopback,
            expect ~macos:"EINVAL" "ENETUNREACH" );
          ( ":: to 127.0.0.1",
            true,
            `Udp Eio.Net.Ipaddr.V6.any,
            `Udp v4_loopback,
            expect ~macos:"EINVAL" "delivered" );
          ( "udp to unix path",
            false,
            `Udp v4_loopback,
            `Unix,
            expect ~macos:"EADDRNOTAVAIL" ~mem:"EAFNOSUPPORT"
              "EAFNOSUPPORT | EINVAL" );
          ( "unix to udp",
            false,
            `Unix,
            `Udp v4_loopback,
            expect ~macos:"EAFNOSUPPORT" ~mem:"EAFNOSUPPORT" "EINVAL" );
        ];
      (* A datagram's source address is the sender's as the receiver sees
         it: a wildcard sender sends from the loopback address the
         destination reaches, and an IPv4 sender reaches an IPv6 receiver
         IPv4-mapped. *)
      List.map
        (fun (variant, src, dst, sent_to, e) ->
          case ~file ~test:"DatagramSource" ~variant ~ipv6:true e (fun ctx ->
              let r, r_addr = datagram_socket ctx (`Udp (dst, 0)) in
              let s, _ = datagram_socket ctx (`Udp (src, 0)) in
              let to_ =
                match (sent_to, r_addr) with
                | Some ip, `Udp (_, port) -> `Udp (ip, port)
                | _ -> r_addr
              in
              send s ~dst:to_ "abc";
              match Eio.Net.recv r (Cstruct.create 3) with
              | `Udp (ip, _), _ -> "from " ^ pp_ip ip
              | `Unix path, _ -> "from " ^ path))
        [
          ( "127.0.0.1 to [::]",
            v4_loopback,
            Eio.Net.Ipaddr.V6.any,
            Some v4_loopback,
            expect "from ::ffff:127.0.0.1" );
          ( "0.0.0.0 to 127.0.0.1",
            Eio.Net.Ipaddr.V4.any,
            v4_loopback,
            None,
            expect "from 127.0.0.1" );
          ( ":: to ::ffff:127.0.0.1",
            Eio.Net.Ipaddr.V6.any,
            v4_loopback,
            Some v4_mapped_loopback,
            expect "from 127.0.0.1" );
        ];
      (* A connected client holds its address: a second connect bound to it,
         and a listen there, find it in use. *)
      [
        case ~file ~test:"ClientPortHeld" ~variant:"second bound connect"
          (expect "EADDRINUSE") (fun ctx ->
            let l = listen ctx (`Tcp (v4_loopback, 0)) in
            let bind_to = `Tcp (v4_loopback, free_port ctx) in
            let connect () =
              ignore
                (Eio.Net.connect ~sw:ctx.sw ~bind_to ctx.net
                   (Eio.Net.listening_addr l))
            in
            connect ();
            outcome (fun () ->
                connect ();
                "connected"));
        case ~file ~test:"ClientPortHeld" ~variant:"listen at it"
          (expect "EADDRINUSE") (fun ctx ->
            let l = listen ctx (`Tcp (v4_loopback, 0)) in
            let port = free_port ctx in
            ignore
              (Eio.Net.connect ~sw:ctx.sw
                 ~bind_to:(`Tcp (v4_loopback, port))
                 ctx.net (Eio.Net.listening_addr l));
            outcome (fun () ->
                ignore (listen ctx (`Tcp (v4_loopback, port)));
                "listening"));
      ];
      (* Stream and datagram sockets share one namespace of Unix paths. *)
      [
        case ~file ~test:"UnixPathShared" ~variant:"listen at a datagram path"
          (expect "EADDRINUSE") (fun ctx ->
            let _, addr = dgram_socket ctx Unix_dgram in
            let path = match addr with `Unix p -> p | `Udp _ -> "" in
            outcome (fun () ->
                ignore (listen ctx (`Unix path));
                "listening"));
        case ~file ~test:"UnixPathShared" ~variant:"datagram at a listener path"
          (expect "EADDRINUSE") (fun ctx ->
            let addr = stream_addr ctx Unix_stream in
            ignore (listen ctx addr);
            let path = match addr with `Unix p -> p | `Tcp _ -> "" in
            outcome (fun () ->
                ignore (datagram_socket ctx (`Unix path));
                "bound"));
        case ~file ~test:"UnixPathShared"
          ~variant:"datagram sent to a listener path" (expect "EPROTOTYPE")
          (fun ctx ->
            let addr = stream_addr ctx Unix_stream in
            ignore (listen ctx addr);
            let path = match addr with `Unix p -> p | `Tcp _ -> "" in
            let s, _ = dgram_socket ctx Unix_dgram in
            outcome (fun () ->
                send s ~dst:(`Unix path) "abc";
                "sent"));
        case ~file ~test:"UnixPathShared"
          ~variant:"connect bound to a listener path" (expect "EADDRINUSE")
          (fun ctx ->
            let addr = stream_addr ctx Unix_stream in
            let l = listen ctx addr in
            let other = stream_addr ctx Unix_stream in
            ignore (listen ctx other);
            outcome (fun () ->
                ignore
                  (Eio.Net.connect ~sw:ctx.sw ~bind_to:other ctx.net
                     (Eio.Net.listening_addr l));
                "connected"));
      ];
      (* A listener closed by hand leaves its file: a listen there finds it
         in use, and a connect there is refused. *)
      [
        case ~file ~test:"ListenerClosedByHand" ~variant:"listen there"
          (expect "EADDRINUSE") (fun ctx ->
            let addr = stream_addr ctx Unix_stream in
            Eio.Net.close (listen ctx addr);
            outcome (fun () ->
                ignore (listen ctx addr);
                "listening"));
        case ~file ~test:"ListenerClosedByHand" ~variant:"connect there"
          (expect "refused") (fun ctx ->
            let addr = stream_addr ctx Unix_stream in
            Eio.Net.close (listen ctx addr);
            outcome (fun () ->
                ignore (connect ctx addr);
                "connected"));
      ];
    ]

let suite = Oracle.suite "review" cases
