module Chaos = Eio_net_chaos
module Flow = Chaos.Flow
module Datagram = Chaos.Datagram
module Net = Chaos.Net

let read1 flow =
  let buf = Cstruct.create 64 in
  let n = Eio.Flow.single_read flow buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

(* Read a flow to end-of-file, returning the size of every chunk delivered. The
   chunking is what the read-side faults shape. *)
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

(* A dribble schedule splits one buffered payload across several reads: the
   scripted [Deliver 2; Deliver 3] hands up "ab" then "cde", the rest last. *)
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

(* Coalescing forces two distinct transport reads into one application read:
   the peer writes "aa", yields, writes "bb"; [Coalesce 2] returns "aabb". *)
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

(* A stalled read yields the scheduler without delivering, letting a competing
   fiber advance, then delivers. The data is buffered up front. *)
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

(* A torn write accepts only the scripted prefix and then errors: the caller
   sees a short count of 3, the peer receives exactly "abc", and the next
   write raises a reset at the scripted point. *)
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

(* A mid-stream reset: the first read delivers, the scripted [Reset] on the
   second raises an Eio.Io error. *)
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

(* A mid-stream end-of-file: the scripted [Eof] on the second read raises
   End_of_file, and it stays ended. *)
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

(* Same seed, same fault sequence: a seeded dribble schedule chunks a fixed
   payload identically across two independent runs, and it does split the
   payload (some chunk is shorter than the whole). *)
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

(* A minimal in-memory datagram socket for hermetic datagram tests: [recv]
   pops from a pre-filled inbound queue, [send] records outbound payloads.
   mem deliberately has no datagram support, so the fake lives here. *)
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

(* A fake datagram socket pre-filled with [payloads] (all from the same
   loopback source), returned as an [Eio.Net.datagram_socket]. *)
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

(* Drop as scripted: [Drop 1] consumes and discards "d0", then delivers "d1". *)
let test_dgram_drop () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1"; "d2"; "d3" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Drop 1 ]) base in
  Alcotest.(check (list string))
    "dropped datagram is skipped" [ "d1"; "d2"; "d3" ] (recv_n sock 3)

(* Duplicate as scripted: [Dup] delivers "d0" now and again on the next recv. *)
let test_dgram_dup () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Dup ]) base in
  Alcotest.(check (list string))
    "datagram is delivered twice" [ "d0"; "d0"; "d1" ] (recv_n sock 3)

(* Reorder as scripted: [Reorder] holds "d0" and delivers "d1" first; "d0"
   surfaces on the following recv. *)
let test_dgram_reorder () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "d0"; "d1"; "d2" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Reorder ]) base in
  Alcotest.(check (list string))
    "datagrams arrive out of order" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

(* Truncate as scripted: only the first 3 bytes of "hello" are delivered. *)
let test_dgram_truncate () =
  Eio_mock.Backend.run @@ fun () ->
  let base = fake_dgram [ "hello" ] in
  let sock = Datagram.wrap (Datagram.Schedule.script [ Truncate 3 ]) base in
  Alcotest.(check string) "datagram truncated" "hel" (recv1 sock)

(* Garble as scripted replaces the payload with seed-derived bytes: the same
   script produces the same garbage on two wraps (deterministic), of the
   scripted length, and not the original payload. *)
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

(* Same seed, same datagram fault sequence: a seeded lossy schedule delivers an
   identical payload sequence across two runs, and it is not the untouched
   in-order stream. *)
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

(* connect returns an adversarial flow: the per-flow dribble schedule applies
   to the connected client flow, splitting the server's write. *)
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

(* A scripted connect fault refuses the connection with an Eio.Io error. *)
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

(* A slow connect yields before completing, then returns a usable flow. *)
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

(* A keep-alive-style listener drops after its accept budget: two accepts
   succeed, the third raises End_of_file. *)
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
