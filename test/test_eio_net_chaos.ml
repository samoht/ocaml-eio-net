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

(* --- Datagram faults ----------------------------------------------------- *)

(* mem deliberately has no datagram support, so the fake lives here. *)
module Fake_dgram = struct
  type tag = [ `Generic ]

  type t = {
    inbound : (Eio.Net.Sockaddr.datagram * string) Eio.Stream.t;
    outbound : string list ref;
  }

  let recv t buf =
    let addr, data = Eio.Stream.take t.inbound in
    let n = min (String.length data) (Cstruct.length buf) in
    Cstruct.blit_from_string data 0 buf 0 n;
    (addr, n)

  let send t ?dst:_ bufs =
    t.outbound :=
      String.concat "" (List.map Cstruct.to_string bufs) :: !(t.outbound)

  let shutdown _ _ = ()
  let close _ = ()
  let setsockopt _ _ _ = ()

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "fake datagram: getsockopt unsupported"
end

let fake_handler = Eio.Net.Pi.datagram_socket (module Fake_dgram)
let loopback port = `Udp (Eio.Net.Ipaddr.V4.loopback, port)

let fake_dgram payloads =
  let inbound = Eio.Stream.create max_int in
  List.iter (fun p -> Eio.Stream.add inbound (loopback 9000, p)) payloads;
  let t = { Fake_dgram.inbound; outbound = ref [] } in
  (Eio.Resource.T (t, fake_handler)
    : [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t)

let recv1 sock =
  let buf = Cstruct.create 64 in
  let _addr, n = Eio.Net.recv sock buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

let recv_n sock n = List.init n (fun _ -> recv1 sock)

let test_dgram_drop () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1"; "d2"; "d3" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Drop 1 ]) base in
  Alcotest.(check (list string))
    "dropped datagram is skipped" [ "d1"; "d2"; "d3" ] (recv_n sock 3)

let test_dgram_dup () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Dup ]) base in
  Alcotest.(check (list string))
    "datagram is delivered twice" [ "d0"; "d0"; "d1" ] (recv_n sock 3)

let test_dgram_reorder () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1"; "d2" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Reorder ]) base in
  Alcotest.(check (list string))
    "datagrams arrive out of order" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

let test_dgram_truncate () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "hello" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Truncate 3 ]) base in
  Alcotest.(check string) "datagram truncated" "hel" (recv1 sock)

let test_dgram_garble () =
  let one () =
    Eio_mock.Backend.run @@ fun () ->
    let base = fake_dgram [ "hello" ] in
    let sock = Datagram.wrap (Datagram.Schedule.script [ Garble 4 ]) base in
    recv1 sock
  in
  let a = one () and b = one () in
  Alcotest.(check string) "garble is deterministic" a b;
  Alcotest.(check int) "garble has the scripted length" 4 (String.length a);
  Alcotest.(check bool) "garble differs from the original" true (a <> "hell")

let run_seeded_dgram seed =
  Eio_mock.Backend.run @@ fun () ->
  let payloads = List.init 30 (fun i -> Fmt.str "%02d" i) in
  let base = fake_dgram payloads in
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

let suite =
  ( "eio_net_chaos",
    [
      Alcotest.test_case "flow dribble" `Quick test_flow_dribble;
      Alcotest.test_case "flow coalesce" `Quick test_flow_coalesce;
      Alcotest.test_case "flow stall" `Quick test_flow_stall;
      Alcotest.test_case "flow torn write" `Quick test_flow_torn_write;
      Alcotest.test_case "flow mid-stream reset" `Quick test_flow_reset;
      Alcotest.test_case "flow mid-stream eof" `Quick test_flow_eof;
      Alcotest.test_case "flow seeded determinism" `Quick
        test_flow_seeded_deterministic;
      Alcotest.test_case "datagram drop" `Quick test_dgram_drop;
      Alcotest.test_case "datagram dup" `Quick test_dgram_dup;
      Alcotest.test_case "datagram reorder" `Quick test_dgram_reorder;
      Alcotest.test_case "datagram truncate" `Quick test_dgram_truncate;
      Alcotest.test_case "datagram garble" `Quick test_dgram_garble;
      Alcotest.test_case "datagram seeded determinism" `Quick
        test_dgram_seeded_deterministic;
      Alcotest.test_case "net connect dribbles" `Quick test_net_connect_dribbles;
      Alcotest.test_case "net connect refused" `Quick test_net_connect_refused;
      Alcotest.test_case "net connect slow" `Quick test_net_connect_slow;
      Alcotest.test_case "net max accepts" `Quick test_net_max_accepts;
    ] )
