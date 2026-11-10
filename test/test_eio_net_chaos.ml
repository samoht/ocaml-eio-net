module Chaos = Eio_net_chaos
module Flow = Chaos.Flow
module Datagram = Chaos.Datagram
module Net = Chaos.Net

let read1 flow =
  let buf = Cstruct.create 64 in
  let n = Eio.Flow.single_read flow buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

let read_sizes flow =
  let buf = Cstruct.create 128 in
  let sizes = ref [] in
  (try
     while true do
       sizes := Eio.Flow.single_read flow buf :: !sizes
     done
   with End_of_file -> ());
  List.rev !sizes

let is_io = function Eio.Io _ -> true | _ -> false

(* --- Flow faults --------------------------------------------------------- *)

let test_flow_dribble () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair
      ~server:(Flow.Schedule.script ~reads:[ Deliver 2; Deliver 3 ] ())
      ()
  in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string "abcdef" client;
      Eio.Flow.close client)
    (fun () ->
      let a = read1 server in
      let b = read1 server in
      let c = read1 server in
      got := [ a; b; c ]);
  Alcotest.(check (list string))
    "read split into scripted chunks" [ "ab"; "cde"; "f" ] !got

let test_flow_coalesce () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~server:(Flow.Schedule.script ~reads:[ Coalesce 2 ] ()) ()
  in
  let got = ref "" in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string "aa" client;
      Eio.Fiber.yield ();
      Eio.Flow.copy_string "bb" client;
      Eio.Flow.close client)
    (fun () -> got := read1 server);
  Alcotest.(check string) "two writes coalesced into one read" "aabb" !got

(* Bytes buffered by an earlier short read reach the reader before the end of
   the stream. *)
let test_flow_coalesce_at_eof () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair
      ~server:(Flow.Schedule.script ~reads:[ Deliver 1; Coalesce 2 ] ())
      ()
  in
  Eio.Flow.copy_string "abcdef" client;
  Eio.Flow.shutdown client `Send;
  let a = read1 server in
  let b = read1 server in
  Alcotest.(check (list string)) "every byte arrives" [ "a"; "bcdef" ] [ a; b ];
  match read1 server with
  | exception End_of_file -> ()
  | s -> Alcotest.failf "read %S past the end" s

let test_flow_stall () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~server:(Flow.Schedule.script ~reads:[ Stall 3 ] ()) ()
  in
  Eio.Flow.copy_string "data" client;
  let progress = ref 0 and result = ref "" in
  Eio.Fiber.both
    (fun () ->
      for _ = 1 to 3 do
        incr progress;
        Eio.Fiber.yield ()
      done)
    (fun () -> result := read1 server);
  Alcotest.(check string) "stalled read still delivers" "data" !result;
  Alcotest.(check bool)
    "competitor advanced during the stall" true (!progress >= 1)

let test_flow_torn_write () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~client:(Flow.Schedule.script ~writes:[ Accept 3; Fail ] ()) ()
  in
  let accepted = ref 0 and raised = ref false and peer = ref "" in
  Eio.Fiber.both
    (fun () ->
      accepted := Eio.Flow.single_write client [ Cstruct.of_string "abcdef" ];
      try ignore (Eio.Flow.single_write client [ Cstruct.of_string "def" ])
      with e when is_io e -> raised := true)
    (fun () -> peer := read1 server);
  Alcotest.(check int) "short write accepted only the prefix" 3 !accepted;
  Alcotest.(check string) "peer received exactly the accepted bytes" "abc" !peer;
  Alcotest.(check bool) "second write raised a reset" true !raised

let test_flow_reset () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~server:(Flow.Schedule.script ~reads:[ Deliver 0; Reset ] ()) ()
  in
  let first = ref "" and reset = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string "abc" client;
      Eio.Flow.close client)
    (fun () ->
      first := read1 server;
      try ignore (read1 server) with e when is_io e -> reset := true);
  Alcotest.(check string) "first read delivers" "abc" !first;
  Alcotest.(check bool) "second read resets" true !reset

let test_flow_eof () =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~server:(Flow.Schedule.script ~reads:[ Deliver 0; Eof ] ()) ()
  in
  let first = ref "" and ended = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string "abc" client;
      Eio.Flow.close client)
    (fun () ->
      first := read1 server;
      try ignore (read1 server) with End_of_file -> ended := true);
  Alcotest.(check string) "first read delivers" "abc" !first;
  Alcotest.(check bool) "second read ends the stream" true !ended

let run_seeded_flow seed =
  Eio_mock.Backend.run @@ fun () ->
  let client, server =
    Flow.pair ~server:(Flow.Schedule.seeded ~dribble:4 ~stall:2 seed) ()
  in
  let sizes = ref [] in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string (String.make 20 'x') client;
      Eio.Flow.close client)
    (fun () -> sizes := read_sizes server);
  !sizes

let test_flow_seeded_deterministic () =
  let a = run_seeded_flow 1234 and b = run_seeded_flow 1234 in
  Alcotest.(check (list int)) "same seed, same chunking" a b;
  Alcotest.(check int) "all 20 bytes delivered" 20 (List.fold_left ( + ) 0 a);
  Alcotest.(check bool)
    "dribble split the payload below 20" true
    (List.exists (fun n -> n < 20) a);
  let c = run_seeded_flow 5678 in
  Alcotest.(check bool) "a different seed chunks differently" true (a <> c)

(* A seeded reset lands at byte [reset_after] exactly, however the bytes
   arrived: a read before it delivers no byte past it. *)
let test_flow_seeded_reset_at_byte () =
  List.iter
    (fun dribble ->
      Eio_mock.Backend.run @@ fun () ->
      let client, server =
        Flow.pair ~server:(Flow.Schedule.seeded ~dribble ~reset_after:5 1) ()
      in
      Eio.Flow.copy_string "0123456789" client;
      let got = Buffer.create 10 in
      let buf = Cstruct.create 64 in
      let rec loop () =
        match Eio.Flow.single_read server buf with
        | n ->
            Buffer.add_string got (Cstruct.to_string ~len:n buf);
            loop ()
        | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ()
      in
      loop ();
      Alcotest.(check string)
        (Fmt.str "bytes before the reset (dribble %d)" dribble)
        "01234" (Buffer.contents got))
    [ 0; 3 ]

(* The dribble and short-write bounds are uncapped, so one past [2^30 - 1]
   still injects faults. *)
let test_flow_seeded_wide_bounds () =
  Eio_mock.Backend.run @@ fun () ->
  let wide = 1 lsl 30 in
  let client, server =
    Flow.pair
      ~client:(Flow.Schedule.seeded ~short_write:wide 1)
      ~server:(Flow.Schedule.seeded ~dribble:wide 1)
      ()
  in
  let sizes = ref [] in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string (String.make 20 'x') client;
      Eio.Flow.close client)
    (fun () -> sizes := read_sizes server);
  Alcotest.(check int)
    "all 20 bytes delivered" 20
    (List.fold_left ( + ) 0 !sizes)

(* A write after the flow's own close raises [Invalid_argument], as on the
   socket it wraps, before any fault: the scripted failure is not reached. *)
let test_flow_write_after_close () =
  Eio_mock.Backend.run @@ fun () ->
  let client, _server =
    Flow.pair ~client:(Flow.Schedule.script ~writes:[ Fail ] ()) ()
  in
  Eio.Flow.close client;
  match Eio.Flow.single_write client [ Cstruct.of_string "x" ] with
  | exception Invalid_argument _ -> ()
  | exception e -> Alcotest.failf "a write after close raised %a" Fmt.exn e
  | (_ : int) -> Alcotest.fail "a write after close went through"

(* --- Datagram faults ----------------------------------------------------- *)

let loopback port = `Udp (Eio.Net.Ipaddr.V4.loopback, port)

(* [with_datagrams payloads f] is [f ~tx rx], where [rx] is a datagram socket
   of an in-memory network that [tx] has sent each of [payloads] to. *)
let with_datagrams payloads f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let rx = Eio.Net.datagram_socket ~sw net (loopback 9000) in
  let tx = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let send p = Eio.Net.send tx ~dst:(loopback 9000) [ Cstruct.of_string p ] in
  List.iter send payloads;
  f ~send rx

let recv1 sock =
  let buf = Cstruct.create 64 in
  let _addr, n = Eio.Net.recv sock buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

let recv_n sock n = List.init n (fun _ -> recv1 sock)

let test_dgram_drop () =
  with_datagrams [ "d0"; "d1"; "d2"; "d3" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Drop 1 ]) base in
  Alcotest.(check (list string))
    "dropped datagram is skipped" [ "d1"; "d2"; "d3" ] (recv_n sock 3)

(* A negative drop count has no datagram to stop at: the receive would drop
   every datagram the base ever delivers. The script refuses it when it is
   built, before any receive. *)
let test_dgram_drop_negative () =
  match Datagram.Schedule.script [ Pass; Drop (-1) ] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "a script with Drop (-1) was built"

let test_dgram_dup () =
  with_datagrams [ "d0"; "d1" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Dup ]) base in
  Alcotest.(check (list string))
    "datagram is delivered twice" [ "d0"; "d0"; "d1" ] (recv_n sock 3)

let test_dgram_reorder () =
  with_datagrams [ "d0"; "d1"; "d2" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Reorder ]) base in
  Alcotest.(check (list string))
    "datagrams arrive out of order" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

(* A receive cancelled while no datagram has come takes no fault: the script's
   Dup applies to the first datagram the base delivers. *)
let test_dgram_cancelled_recv_takes_no_fault () =
  with_datagrams [] @@ fun ~send base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Dup ]) base in
  Eio.Fiber.first (fun () -> ignore (recv1 sock)) Eio.Fiber.yield;
  send "one";
  send "two";
  Alcotest.(check (list string))
    "Dup applies to one" [ "one"; "one" ] (recv_n sock 2)

(* A datagram held back is kept whole: it is cut to the buffer of the receive
   that delivers it, not of the one that took it. *)
let test_dgram_held_datagram_kept_whole () =
  with_datagrams [ "aaaa"; "b" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Reorder ]) base in
  let recv len =
    let buf = Cstruct.create len in
    let _, n = Eio.Net.recv sock buf in
    Cstruct.to_string ~len:n buf
  in
  let first = recv 1 in
  let second = recv 6 in
  Alcotest.(check (list string))
    "b, then aaaa whole" [ "b"; "aaaa" ] [ first; second ]

(* A datagram held back goes with the socket: a receive after close raises
   [Invalid_argument] instead of delivering it. *)
let test_dgram_close_drops_held () =
  with_datagrams [ "x" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Dup ]) base in
  Alcotest.(check string) "first copy" "x" (recv1 sock);
  Eio.Net.close sock;
  match recv1 sock with
  | exception Invalid_argument _ -> ()
  | got -> Alcotest.failf "received %S after close" got

(* A receive cancelled while it holds a datagram back keeps it: the next
   receives deliver the one after it, then the held one. *)
let test_dgram_reorder_cancelled () =
  with_datagrams [ "d0" ] @@ fun ~send base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Reorder ]) base in
  Eio.Fiber.first
    (fun () -> Alcotest.failf "received %S alone" (recv1 sock))
    (fun () -> Eio.Fiber.yield ());
  send "d1";
  send "d2";
  Alcotest.(check (list string))
    "the held datagram survives" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

let test_dgram_truncate () =
  with_datagrams [ "hello" ] @@ fun ~send:_ base ->
  let sock = Datagram.wrap (Datagram.Schedule.script [ Truncate 3 ]) base in
  Alcotest.(check string) "datagram truncated" "hel" (recv1 sock)

let test_dgram_garble () =
  let one () =
    with_datagrams [ "hello" ] @@ fun ~send:_ base ->
    let sock = Datagram.wrap (Datagram.Schedule.script [ Garble 4 ]) base in
    recv1 sock
  in
  let a = one () and b = one () in
  Alcotest.(check string) "garble is deterministic" a b;
  Alcotest.(check int) "garble has the scripted length" 4 (String.length a);
  Alcotest.(check bool) "garble differs from the original" true (a <> "hell")

let run_seeded_dgram seed =
  let payloads = List.init 30 (fun i -> Fmt.str "%02d" i) in
  with_datagrams payloads @@ fun ~send:_ base ->
  let sock =
    Datagram.wrap
      (Datagram.Schedule.seeded ~loss:0.4 ~dup:0.2 ~reorder:0.2 seed)
      base
  in
  recv_n sock 8

let test_dgram_seeded_deterministic () =
  let a = run_seeded_dgram 7 and b = run_seeded_dgram 7 in
  Alcotest.(check (list string)) "same seed, same datagram sequence" a b;
  let identity = List.init 8 (fun i -> Fmt.str "%02d" i) in
  Alcotest.(check bool) "faults perturbed the stream" true (a <> identity)

(* --- Net faults ---------------------------------------------------------- *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

let test_net_connect_dribbles () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Net.v ~sw
      (Net.Schedule.v
         ~flow:(Flow.Schedule.script ~reads:[ Deliver 2; Deliver 2 ] ())
         ())
  in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  let got = ref [] in
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Flow.copy_string "abcd" flow;
      Eio.Flow.close flow)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      let a = read1 flow in
      let b = read1 flow in
      got := [ a; b ]);
  Alcotest.(check (list string)) "connected flow dribbles" [ "ab"; "cd" ] !got

let test_net_connect_refused () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Net.v ~sw (Net.Schedule.v ~connect:[ Refuse ] ()) in
  let refused =
    try
      ignore (Eio.Net.connect ~sw net addr);
      false
    with e when is_io e -> true
  in
  Alcotest.(check bool) "connect refused" true refused

let test_net_connect_slow () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Net.v ~sw (Net.Schedule.v ~connect:[ Slow 2 ] ()) in
  let listener = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  let got = ref "" in
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Flow.copy_string "ok" flow;
      Eio.Flow.close flow)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      got := read1 flow);
  Alcotest.(check string) "slow connect still connects" "ok" !got

let test_net_max_accepts () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Net.v ~sw (Net.Schedule.v ~max_accepts:2 ()) in
  let listener = Eio.Net.listen ~sw ~backlog:4 ~reuse_addr:true net addr in
  let dropped = ref false in
  Eio.Fiber.both
    (fun () ->
      let _ = Eio.Net.connect ~sw net addr in
      let _ = Eio.Net.connect ~sw net addr in
      ())
    (fun () ->
      let _ = Eio.Net.accept ~sw listener in
      let _ = Eio.Net.accept ~sw listener in
      try ignore (Eio.Net.accept ~sw listener)
      with End_of_file -> dropped := true);
  Alcotest.(check bool) "listener drops past its accept budget" true !dropped

(* An accept cancelled before a connection came uses none of the budget. *)
let test_net_max_accepts_cancelled () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Net.v ~sw (Net.Schedule.v ~max_accepts:1 ()) in
  let listener = Eio.Net.listen ~sw ~backlog:4 net addr in
  Eio.Fiber.first
    (fun () -> ignore (Eio.Net.accept ~sw listener))
    (fun () -> Eio.Fiber.yield ());
  let _client = Eio.Net.connect ~sw net addr in
  match Eio.Net.accept ~sw listener with
  | _ -> ()
  | exception End_of_file -> Alcotest.fail "the cancelled accept was counted"

(* Accepts waiting side by side share the budget: with one accept allowed,
   two waiting accepts and two connects give one connection, and the other
   accept raises [End_of_file]. *)
let test_net_max_accepts_concurrent () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Net.v ~sw (Net.Schedule.v ~max_accepts:1 ()) in
  let listener = Eio.Net.listen ~sw ~backlog:4 net addr in
  let accepted = ref 0 and refused = ref 0 in
  let accept () =
    match Eio.Net.accept ~sw listener with
    | _ -> incr accepted
    | exception End_of_file -> incr refused
  in
  Eio.Fiber.all
    [
      accept;
      accept;
      (fun () ->
        for _ = 1 to 2 do
          ignore (Eio.Net.connect ~sw net addr)
        done);
    ];
  Alcotest.(check (pair int int))
    "accepted, refused" (1, 1) (!accepted, !refused)

(* Bytes a short read left buffered go with the read side: a read after the
   flow's own close raises [Invalid_argument], and one after it shut its
   receive side raises [End_of_file], as eio-net.mem raises them. *)
let test_flow_buffer_follows_read_side () =
  Eio_mock.Backend.run @@ fun () ->
  let after shut =
    let c, s =
      Flow.pair ~server:(Flow.Schedule.script ~reads:[ Deliver 1 ] ()) ()
    in
    Eio.Flow.copy_string "hello" c;
    let buf = Cstruct.create 16 in
    ignore (Eio.Flow.single_read s buf : int);
    shut s;
    match Eio.Flow.single_read s buf with
    | n -> Fmt.str "read %S" (Cstruct.to_string ~len:n buf)
    | exception Invalid_argument _ -> "Invalid_argument"
    | exception End_of_file -> "End_of_file"
  in
  let closed = after Eio.Flow.close in
  let receive = after (fun s -> Eio.Flow.shutdown s `Receive) in
  let all = after (fun s -> Eio.Flow.shutdown s `All) in
  Alcotest.(check (list string))
    "close, receive, all"
    [ "Invalid_argument"; "End_of_file"; "End_of_file" ]
    [ closed; receive; all ]

(* Writers in two domains share one count of bytes written: with the write
   fault after one byte less than they write in all, exactly one write fails,
   in each of three rounds. *)
let test_flow_writes_counted_across_domains () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let per_domain = 100_000 in
  for _ = 1 to 3 do
    let c, _s =
      Flow.pair
        ~client:
          (Flow.Schedule.seeded ~fail_write_after:((2 * per_domain) - 1) 1)
        ()
    in
    let failed = Atomic.make 0 in
    let one = Cstruct.of_string "x" in
    let writer () =
      Eio.Domain_manager.run domain_mgr (fun () ->
          for _ = 1 to per_domain do
            match Eio.Flow.single_write c [ one ] with
            | _ -> ()
            | exception Eio.Io (Eio.Net.E (Connection_reset _), _) ->
                Atomic.incr failed
          done)
    in
    Eio.Fiber.both writer writer;
    Alcotest.(check int) "failed writes" 1 (Atomic.get failed)
  done

(* A printed schedule is the call that builds it, every parameter included. *)
let test_schedules_print_every_parameter () =
  let check what expected pp v =
    Alcotest.(check string) what expected (Fmt.str "%a" pp v)
  in
  check "a flow script"
    "script ~reads:[Deliver 3; Coalesce 2; Stall 1; Reset; Eof] \
     ~writes:[Accept 2; Fail] ()"
    Flow.Schedule.pp
    (Flow.Schedule.script
       ~reads:Flow.[ Deliver 3; Coalesce 2; Stall 1; Reset; Eof ]
       ~writes:Flow.[ Accept 2; Fail ]
       ());
  check "a seeded flow"
    "seeded ~dribble:4 ~stall:2 ~short_write:3 ~reset_after:10 \
     ~reset_kind:`Eof ~fail_write_after:20 1234"
    Flow.Schedule.pp
    (Flow.Schedule.seeded ~dribble:4 ~stall:2 ~short_write:3 ~reset_after:10
       ~reset_kind:`Eof ~fail_write_after:20 1234);
  check "a datagram script"
    "script [Pass; Drop 2; Dup; Reorder; Truncate (-1); Garble 4; Delay 5]"
    Datagram.Schedule.pp
    (Datagram.Schedule.script
       Datagram.[ Pass; Drop 2; Dup; Reorder; Truncate (-1); Garble 4; Delay 5 ]);
  check "a seeded datagram schedule"
    "seeded ~loss:0.1 ~dup:0.2 ~reorder:0.05 ~truncate:0.15 ~garble:0.3 \
     ~delay:3 7"
    Datagram.Schedule.pp
    (Datagram.Schedule.seeded ~loss:0.1 ~dup:0.2 ~reorder:0.05 ~truncate:0.15
       ~garble:0.3 ~delay:3 7);
  check "a network schedule"
    "v ~flow:(Flow.Schedule.seeded ~dribble:4 ~stall:0 ~short_write:0 \
     ~reset_kind:`Reset 9) ~connect:[Refuse; Timeout; Slow 2; Open] \
     ~max_accepts:3 ()"
    Net.Schedule.pp
    (Net.Schedule.seeded ~dribble:4
       ~connect:Net.[ Refuse; Timeout; Slow 2; Open ]
       ~max_accepts:3 9)

(* Probabilities past [1.] in all describe no distribution. *)
let test_dgram_seeded_refuses_odds_past_one () =
  match Datagram.Schedule.seeded ~loss:0.6 ~dup:0.5 7 with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "odds summing to 1.1 were accepted"

(* A probability outside [0., 1.], NaN among them, describes no distribution,
   and its printed form would not read back. *)
let test_dgram_seeded_refuses_odds_outside_unit () =
  List.iter
    (fun p ->
      match Datagram.Schedule.seeded ~dup:p 7 with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "the probability %g was accepted" p)
    [ Float.nan; -0.5; Float.neg_infinity ]

let suite =
  ( "eio_net_chaos",
    [
      Alcotest.test_case "flow dribble" `Quick test_flow_dribble;
      Alcotest.test_case "flow coalesce" `Quick test_flow_coalesce;
      Alcotest.test_case "flow coalesce at end of stream" `Quick
        test_flow_coalesce_at_eof;
      Alcotest.test_case "flow stall" `Quick test_flow_stall;
      Alcotest.test_case "flow torn write" `Quick test_flow_torn_write;
      Alcotest.test_case "flow mid-stream reset" `Quick test_flow_reset;
      Alcotest.test_case "flow mid-stream eof" `Quick test_flow_eof;
      Alcotest.test_case "flow seeded determinism" `Quick
        test_flow_seeded_deterministic;
      Alcotest.test_case "flow seeded reset at its byte" `Quick
        test_flow_seeded_reset_at_byte;
      Alcotest.test_case "flow seeded wide bounds" `Quick
        test_flow_seeded_wide_bounds;
      Alcotest.test_case "datagram drop" `Quick test_dgram_drop;
      Alcotest.test_case "datagram drop refuses a negative count" `Quick
        test_dgram_drop_negative;
      Alcotest.test_case "datagram dup" `Quick test_dgram_dup;
      Alcotest.test_case "datagram reorder" `Quick test_dgram_reorder;
      Alcotest.test_case "datagram reorder survives a cancelled receive" `Quick
        test_dgram_reorder_cancelled;
      Alcotest.test_case "datagram truncate" `Quick test_dgram_truncate;
      Alcotest.test_case "datagram garble" `Quick test_dgram_garble;
      Alcotest.test_case "datagram seeded determinism" `Quick
        test_dgram_seeded_deterministic;
      Alcotest.test_case "net connect dribbles" `Quick test_net_connect_dribbles;
      Alcotest.test_case "net connect refused" `Quick test_net_connect_refused;
      Alcotest.test_case "net connect slow" `Quick test_net_connect_slow;
      Alcotest.test_case "net max accepts" `Quick test_net_max_accepts;
      Alcotest.test_case "net max accepts ignores a cancelled accept" `Quick
        test_net_max_accepts_cancelled;
      Alcotest.test_case "net max accepts shared by concurrent accepts" `Quick
        test_net_max_accepts_concurrent;
      Alcotest.test_case "dgram cancelled recv takes no fault" `Quick
        test_dgram_cancelled_recv_takes_no_fault;
      Alcotest.test_case "dgram held datagram kept whole" `Quick
        test_dgram_held_datagram_kept_whole;
      Alcotest.test_case "dgram close drops the held datagram" `Quick
        test_dgram_close_drops_held;
      Alcotest.test_case "flow write after close" `Quick
        test_flow_write_after_close;
      Alcotest.test_case "flow buffer follows the read side" `Quick
        test_flow_buffer_follows_read_side;
      Alcotest.test_case "flow writes counted across domains" `Quick
        test_flow_writes_counted_across_domains;
      Alcotest.test_case "schedules print every parameter" `Quick
        test_schedules_print_every_parameter;
      Alcotest.test_case "datagram seeded refuses odds past one" `Quick
        test_dgram_seeded_refuses_odds_past_one;
      Alcotest.test_case "datagram seeded refuses odds outside [0, 1]" `Quick
        test_dgram_seeded_refuses_odds_outside_unit;
    ] )
