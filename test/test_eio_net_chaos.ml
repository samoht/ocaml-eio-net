module Chaos = Eio_net_chaos
module Op = Chaos.Op
module Fault = Chaos.Fault

let read1 flow =
  let buf = Cstruct.create 64 in
  let n = Eio.Flow.single_read flow buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

let is_reset = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> true
  | _ -> false

(* [nth kind n fault] injects [fault] on the [n]th operation of [kind], from
   1, and nothing else. *)
let nth kind n fault =
  let seen = ref 0 in
  fun (op : Op.t) ->
    if op.kind = kind then begin
      incr seen;
      if !seen = n then Some fault else None
    end
    else None

let script kind faults =
  let q = ref faults in
  fun (op : Op.t) ->
    if op.kind <> kind then None
    else
      match !q with
      | [] -> None
      | f :: rest ->
          q := rest;
          f

(* --- Flows --------------------------------------------------------------- *)

let test_flow_short () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server =
    Chaos.pair ~clock:env#clock
      (script `Read [ Some (Fault.Short 2); Some (Short 3) ])
  in
  Eio.Flow.copy_string "abcdef" client;
  Eio.Flow.close client;
  let a = read1 server in
  let b = read1 server in
  let c = read1 server in
  Alcotest.(check (list string))
    "reads cut to the faults" [ "ab"; "cde"; "f" ] [ a; b; c ]

(* A read that waits takes what several writes sent meanwhile. *)
let test_flow_stall_coalesces () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server =
    Chaos.pair ~clock:env#clock (nth `Read 1 (Fault.Stall 0.005))
  in
  let got = ref "" in
  Eio.Fiber.both
    (fun () ->
      Eio.Flow.copy_string "aa" client;
      Eio.Time.sleep env#clock 0.001;
      Eio.Flow.copy_string "bb" client)
    (fun () -> got := read1 server);
  Alcotest.(check string) "two writes, one read" "aabb" !got;
  Alcotest.(check (float 0.)) "the read waited" 0.005 (Eio.Time.now env#clock)

(* Bytes a short read left buffered reach the reader before the end of the
   stream. *)
let test_flow_buffered_before_eof () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server =
    Chaos.pair ~clock:env#clock
      (script `Read [ Some (Fault.Short 1); Some (Stall 0.005) ])
  in
  Eio.Flow.copy_string "abcdef" client;
  Eio.Flow.shutdown client `Send;
  let a = read1 server in
  let b = read1 server in
  Alcotest.(check (list string)) "every byte arrives" [ "a"; "bcdef" ] [ a; b ];
  match read1 server with
  | exception End_of_file -> ()
  | s -> Alcotest.failf "read %S past the end" s

(* A write cut short moves only its prefix; a reset after it reaches both
   ends. *)
let test_flow_torn_write () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server =
    Chaos.pair ~clock:env#clock
      (script `Write [ Some (Fault.Short 3); Some Reset ])
  in
  let n = Eio.Flow.single_write client [ Cstruct.of_string "abcdef" ] in
  Alcotest.(check int) "the prefix" 3 n;
  Alcotest.(check string) "the peer reads it" "abc" (read1 server);
  (match Eio.Flow.single_write client [ Cstruct.of_string "def" ] with
  | exception e when is_reset e -> ()
  | _ -> Alcotest.fail "the second write went through");
  match read1 server with
  | exception e when is_reset e -> ()
  | s -> Alcotest.failf "the peer read %S after the reset" s

let test_flow_reset_both_ends () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server = Chaos.pair ~clock:env#clock (nth `Read 2 Fault.Reset) in
  Eio.Flow.copy_string "abc" client;
  Alcotest.(check string) "first read" "abc" (read1 server);
  (match read1 server with
  | exception e when is_reset e -> ()
  | s -> Alcotest.failf "read %S" s);
  (match Eio.Flow.single_write client [ Cstruct.of_string "x" ] with
  | exception e when is_reset e -> ()
  | _ -> Alcotest.fail "the peer's write went through");
  match read1 client with
  | exception e when is_reset e -> ()
  | s -> Alcotest.failf "the peer read %S" s

(* An end drops the bytes not yet read, at both ends. *)
let test_flow_eof_both_ends () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client, server = Chaos.pair ~clock:env#clock (nth `Read 2 Fault.Eof) in
  Eio.Flow.copy_string "abc" client;
  Alcotest.(check string) "first read" "abc" (read1 server);
  Eio.Flow.copy_string "lost" client;
  (match read1 server with
  | exception End_of_file -> ()
  | s -> Alcotest.failf "read %S" s);
  (match Eio.Flow.single_write client [ Cstruct.of_string "x" ] with
  | exception e when is_reset e -> ()
  | _ -> Alcotest.fail "the peer's write went through");
  match read1 client with
  | exception End_of_file -> ()
  | s -> Alcotest.failf "the peer read %S" s

(* A reset on an accept is a connection whose client left first: the client
   reads the reset. *)
let test_accept_reset () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Accept 1 Fault.Reset)
      (Eio_net_mem.v ~sw ())
  in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let client = Eio.Net.connect ~sw net addr in
  let server, _ = Eio.Net.accept ~sw l in
  (match read1 client with
  | exception e when is_reset e -> ()
  | s -> Alcotest.failf "the client read %S" s);
  match read1 server with
  | exception e when is_reset e -> ()
  | s -> Alcotest.failf "the server read %S" s

let is_emfile = function
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, "accept", "")), _) ->
      true
  | _ -> false

(* With no descriptor left, as on Linux: the connection stays queued and its
   client sees nothing, every accept fails until a descriptor is freed, and the
   next accept then takes that connection with its data. *)
let test_accept_emfile () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Accept 2 Fault.Emfile)
      (Eio_net_mem.v ~sw ())
  in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:2 net addr in
  let first = Eio.Net.connect ~sw net addr in
  let held, _ = Eio.Net.accept ~sw l in
  let second = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "hi" second;
  for _ = 1 to 3 do
    match Eio.Net.accept ~sw l with
    | exception e when is_emfile e -> ()
    | _ -> Alcotest.fail "an accept went through with no descriptor left"
  done;
  (match Eio.Time.with_timeout env#clock 1.0 (fun () -> Ok (read1 second)) with
  | Error `Timeout -> ()
  | Ok s -> Alcotest.failf "the client read %S" s);
  Eio.Flow.close held;
  Eio.Flow.close first;
  let server, _ = Eio.Net.accept ~sw l in
  Alcotest.(check string) "the queued connection" "hi" (read1 server)

(* A write after the flow's own close raises [Invalid_argument], as on the
   socket it wraps, before [f] is asked. *)
let test_flow_write_after_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  let asked = ref 0 in
  let client, _server =
    Chaos.pair ~clock:env#clock (fun _ ->
        incr asked;
        Some Fault.Reset)
  in
  Eio.Flow.close client;
  (match Eio.Flow.single_write client [ Cstruct.of_string "x" ] with
  | exception Invalid_argument _ -> ()
  | (_ : int) -> Alcotest.fail "a write after close went through");
  Alcotest.(check int) "f was not asked" 0 !asked

(* Bytes a short read left buffered stand for the socket's queue: after a
   close, a receive shutdown or both, a read does what it does on
   eio-net.mem with the same bytes queued. *)
let test_flow_buffer_follows_read_side () =
  Eio_mock.Backend.run_full @@ fun env ->
  let after (c, s) shut =
    Eio.Flow.copy_string "hello" c;
    let buf = Cstruct.create 16 in
    let first = Eio.Flow.single_read s (Cstruct.sub buf 0 1) in
    assert (first = 1);
    shut s;
    match Eio.Flow.single_read s buf with
    | n -> Fmt.str "read %S" (Cstruct.to_string ~len:n buf)
    | exception Invalid_argument _ -> "Invalid_argument"
    | exception End_of_file -> "End_of_file"
  in
  let wrapped () = Chaos.pair ~clock:env#clock (nth `Read 1 (Fault.Short 1)) in
  let bare () = Eio_net_mem.pipe () in
  List.iter
    (fun (name, shut) ->
      Alcotest.(check string)
        name
        (after (bare ()) shut)
        (after (wrapped ()) shut))
    [
      ("close", Eio.Flow.close);
      ("receive", fun s -> Eio.Flow.shutdown s `Receive);
      ("all", fun s -> Eio.Flow.shutdown s `All);
    ]

(* --- Datagrams ----------------------------------------------------------- *)

let loopback port = `Udp (Eio.Net.Ipaddr.V4.loopback, port)

(* [with_datagrams f payloads k] is [k ~send rx], where [rx] is a datagram
   socket wrapped with [f] that each of [payloads] was sent to. *)
let with_datagrams f payloads k =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Chaos.wrap ~clock:env#clock f (Eio_net_mem.v ~sw ()) in
  let rx = Eio.Net.datagram_socket ~sw net (loopback 9000) in
  let tx = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let send p = Eio.Net.send tx ~dst:(loopback 9000) [ Cstruct.of_string p ] in
  List.iter send payloads;
  k ~send rx

let recv1 sock =
  let buf = Cstruct.create 64 in
  let _addr, n = Eio.Net.recv sock buf in
  Cstruct.to_string (Cstruct.sub buf 0 n)

let recv_n sock n = List.init n (fun _ -> recv1 sock)

let test_dgram_drop () =
  with_datagrams (nth `Recv 1 Fault.Drop) [ "d0"; "d1"; "d2"; "d3" ]
  @@ fun ~send:_ sock ->
  Alcotest.(check (list string))
    "the lost datagram is skipped" [ "d1"; "d2"; "d3" ] (recv_n sock 3)

let test_dgram_dup () =
  with_datagrams (nth `Recv 1 Fault.Dup) [ "d0"; "d1" ] @@ fun ~send:_ sock ->
  Alcotest.(check (list string))
    "the datagram arrives twice" [ "d0"; "d0"; "d1" ] (recv_n sock 3)

let test_dgram_reorder () =
  with_datagrams (nth `Recv 1 Fault.Reorder) [ "d0"; "d1"; "d2" ]
  @@ fun ~send:_ sock ->
  Alcotest.(check (list string))
    "the datagram arrives after the next" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

(* A receive cancelled while no datagram has come draws no fault: the Dup
   applies to the first datagram the base delivers. *)
let test_dgram_cancelled_recv_draws_no_fault () =
  let asked = ref 0 in
  let f (op : Op.t) =
    incr asked;
    if op.offset = 0 then Some Fault.Dup else None
  in
  with_datagrams f [] @@ fun ~send sock ->
  Eio.Fiber.first (fun () -> ignore (recv1 sock)) Eio.Fiber.yield;
  Alcotest.(check int) "no fault drawn" 0 !asked;
  send "one";
  send "two";
  Alcotest.(check (list string))
    "Dup applies to one" [ "one"; "one" ] (recv_n sock 2)

(* A datagram held back is kept whole: it is cut to the buffer of the receive
   that delivers it, not of the one that took it. *)
let test_dgram_held_kept_whole () =
  with_datagrams (nth `Recv 1 Fault.Reorder) [ "aaaa"; "b" ]
  @@ fun ~send:_ sock ->
  let recv len =
    let buf = Cstruct.create len in
    let _, n = Eio.Net.recv sock buf in
    Cstruct.to_string ~len:n buf
  in
  let first = recv 1 in
  let second = recv 6 in
  Alcotest.(check (list string))
    "b, then aaaa whole" [ "b"; "aaaa" ] [ first; second ]

(* A datagram kept back goes with the socket: a receive after close raises
   [Invalid_argument] instead of delivering it. *)
let test_dgram_close_drops_held () =
  with_datagrams (nth `Recv 1 Fault.Dup) [ "x" ] @@ fun ~send:_ sock ->
  Alcotest.(check string) "first copy" "x" (recv1 sock);
  Eio.Net.close sock;
  match recv1 sock with
  | exception Invalid_argument _ -> ()
  | got -> Alcotest.failf "received %S after close" got

(* A receive cancelled while it holds a datagram back keeps it: the next
   receives deliver the one after it, then the held one. *)
let test_dgram_reorder_cancelled () =
  with_datagrams (nth `Recv 1 Fault.Reorder) [ "d0" ] @@ fun ~send sock ->
  Eio.Fiber.first
    (fun () -> Alcotest.failf "received %S alone" (recv1 sock))
    Eio.Fiber.yield;
  send "d1";
  send "d2";
  Alcotest.(check (list string))
    "the held datagram survives" [ "d1"; "d0"; "d2" ] (recv_n sock 3)

(* A receive cancelled after a Drop, while it waits for the next datagram,
   keeps the count of datagrams taken: the next fault lands on the datagram
   it names. *)
let test_dgram_cancelled_drop_keeps_count () =
  let f (op : Op.t) =
    match op.offset with
    | 0 -> Some Fault.Drop
    | 2 -> Some (Fault.Garble 1)
    | _ -> None
  in
  with_datagrams f [] @@ fun ~send sock ->
  send "one";
  Eio.Fiber.first (fun () -> ignore (recv1 sock)) Eio.Fiber.yield;
  send "two";
  send "three";
  let two = recv1 sock in
  let three = recv1 sock in
  Alcotest.(check string) "two whole" "two" two;
  Alcotest.(check int) "three garbled" 1 (String.length three)

(* A datagram taken by a receive that stalls stays with the socket while the
   receive waits. *)
let test_dgram_stall_cancelled () =
  with_datagrams (nth `Recv 1 (Fault.Stall 1.)) [ "d0"; "d1" ]
  @@ fun ~send:_ sock ->
  Eio.Fiber.first
    (fun () -> Alcotest.failf "received %S" (recv1 sock))
    Eio.Fiber.yield;
  Alcotest.(check (list string)) "nothing lost" [ "d0"; "d1" ] (recv_n sock 2)

let test_dgram_garble () =
  let one () =
    with_datagrams (nth `Recv 1 (Fault.Garble 4)) [ "hello" ]
    @@ fun ~send:_ sock -> recv1 sock
  in
  let a = one () and b = one () in
  Alcotest.(check string) "garble is deterministic" a b;
  Alcotest.(check int) "garble has the given length" 4 (String.length a);
  Alcotest.(check bool) "garble differs from the original" true (a <> "hell")

(* --- Networks ------------------------------------------------------------ *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

let serve ~sw net payload =
  let l = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Eio.Fiber.fork ~sw (fun () ->
      let flow, _ = Eio.Net.accept ~sw l in
      Eio.Flow.copy_string payload flow;
      Eio.Flow.close flow)

let test_net_short_reads () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let f (op : Op.t) =
    if op.kind = `Read && op.side = `Client then Some (Fault.Short 2) else None
  in
  let net = Chaos.wrap ~clock:env#clock f (Eio_net_mem.v ~sw ()) in
  serve ~sw net "abcd";
  let flow = Eio.Net.connect ~sw net addr in
  let a = read1 flow in
  let b = read1 flow in
  Alcotest.(check (list string)) "connected flow cut" [ "ab"; "cd" ] [ a; b ]

let test_net_connect_refused () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Connect 1 Fault.Refuse)
      (Eio_net_mem.v ~sw ())
  in
  match Eio.Net.connect ~sw net addr with
  | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> ()
  | _ -> Alcotest.fail "the connect went through"

let test_net_connect_timeout () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Connect 1 Fault.Timeout)
      (Eio_net_mem.v ~sw ())
  in
  (match Eio.Net.connect ~sw net addr with
  | exception
      Eio.Io
        ( Eio.Exn.X
            (Eio_unix.Unix_error (Unix.ETIMEDOUT, "connect-in-progress", "")),
          _ ) ->
      ()
  | _ -> Alcotest.fail "the connect went through");
  Alcotest.(check (float 0.)) "after 133 s" 133. (Eio.Time.now env#clock)

(* An unreachable host fails a connect as eio_posix reports it, once the
   kernel gives up on its address, after 3 s. *)
let test_net_connect_unreachable () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Connect 1 Fault.Unreachable)
      (Eio_net_mem.v ~sw ())
  in
  (match Eio.Net.connect ~sw net addr with
  | exception
      Eio.Io
        ( Eio.Exn.X
            (Eio_unix.Unix_error (Unix.EHOSTUNREACH, "connect-in-progress", "")),
          _ ) ->
      ()
  | _ -> Alcotest.fail "the connect went through");
  Alcotest.(check (float 0.)) "after 3 s" 3. (Eio.Time.now env#clock)

let test_net_connect_stall () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:env#clock
      (nth `Connect 1 (Fault.Stall 2.))
      (Eio_net_mem.v ~sw ())
  in
  serve ~sw net "ok";
  let flow = Eio.Net.connect ~sw net addr in
  Alcotest.(check (float 0.)) "after 2 s" 2. (Eio.Time.now env#clock);
  Alcotest.(check string) "a stalled connect still connects" "ok" (read1 flow)

(* The operations [f] sees name each socket and end, and count from 1. *)
let test_ops () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ops = ref [] in
  let f op =
    ops := Fmt.str "%a" Op.pp op :: !ops;
    None
  in
  let net = Chaos.wrap ~clock:env#clock f (Eio_net_mem.v ~sw ()) in
  serve ~sw net "abcd";
  let flow = Eio.Net.connect ~sw net addr in
  let _ = read1 flow in
  Alcotest.(check (list string))
    "operations"
    [
      "#1 connect 1c @0";
      "#2 read 1c @0+64";
      "#3 accept 1s @0";
      "#4 write 1s @0+4";
    ]
    (List.rev !ops)

(* --- Domains ------------------------------------------------------------- *)

(* Writers in two domains share one count of bytes written: with a reset on
   the last byte they write in all, exactly one write fails, and [f] sees each
   offset once, in each of three rounds. *)
let test_writes_counted_across_domains () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let per_domain = 50_000 in
  for _ = 1 to 3 do
    let seen = Array.make (2 * per_domain) 0 in
    let f (op : Op.t) =
      if op.kind = `Write then begin
        seen.(op.offset) <- seen.(op.offset) + 1;
        if op.offset = (2 * per_domain) - 1 then Some Fault.Reset else None
      end
      else None
    in
    let c, s = Chaos.pair ~clock:(Eio_mock.Clock.make ()) f in
    let failed = Atomic.make 0 in
    let one = Cstruct.of_string "x" in
    let writer () =
      Eio.Domain_manager.run domain_mgr (fun () ->
          for _ = 1 to per_domain do
            match Eio.Flow.single_write c [ one ] with
            | _ -> ()
            | exception e when is_reset e -> Atomic.incr failed
          done)
    in
    Eio.Fiber.both writer writer;
    Eio.Flow.close s;
    Alcotest.(check int) "failed writes" 1 (Atomic.get failed);
    Alcotest.(check bool)
      "each offset once" true
      (Array.for_all (fun n -> n = 1) seen)
  done

(* Readers in two domains share a flow whose reads are cut short: together
   they read each byte once. *)
let test_reads_shared_across_domains () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let n = 200_000 in
  let f (op : Op.t) =
    if op.kind = `Read then Some (Fault.Short (1 + (op.index mod 7))) else None
  in
  let c, s = Chaos.pair ~clock:(Eio_mock.Clock.make ()) f in
  let counts = Array.init 256 (fun _ -> Atomic.make 0) in
  let reader () =
    Eio.Domain_manager.run domain_mgr (fun () ->
        let buf = Cstruct.create 64 in
        try
          while true do
            let k = Eio.Flow.single_read s buf in
            for i = 0 to k - 1 do
              Atomic.incr counts.(Cstruct.get_uint8 buf i)
            done
          done
        with End_of_file -> ())
  in
  let writer () =
    Eio.Flow.copy_string (String.init n (fun i -> Char.chr (i mod 256))) c;
    Eio.Flow.shutdown c `Send
  in
  Eio.Fiber.all [ reader; reader; writer ];
  Array.iteri
    (fun b count ->
      let expected = (n / 256) + if b < n mod 256 then 1 else 0 in
      Alcotest.(check int) (Fmt.str "byte %d" b) expected (Atomic.get count))
    counts

(* Receivers in two domains share a datagram socket that duplicates and holds
   back datagrams: together they receive each datagram, and as many twice as
   [f] duplicated. Which datagram a fault lands on depends on which receive
   takes the lock first. *)
let test_datagrams_shared_across_domains () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let k = 4_000 in
  let dups = ref 0 in
  let f (op : Op.t) =
    if op.offset >= k then None
    else
      match op.offset mod 4 with
      | 0 ->
          incr dups;
          Some Fault.Dup
      | 1 -> Some Fault.Reorder
      | _ -> None
  in
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:(Eio_mock.Clock.make ()) f (Eio_net_mem.v ~sw ())
  in
  let rx = Eio.Net.datagram_socket ~sw net (loopback 9000) in
  let tx = Eio.Net.datagram_socket ~sw net `UdpV4 in
  let counts = Array.init k (fun _ -> Atomic.make 0) in
  let receiver () =
    Eio.Domain_manager.run domain_mgr (fun () ->
        let rec loop () =
          match recv1 rx with
          | "stop" -> ()
          | s ->
              Atomic.incr counts.(int_of_string s);
              loop ()
        in
        loop ())
  in
  let sender () =
    for i = 0 to k - 1 do
      Eio.Net.send tx ~dst:(loopback 9000)
        [ Cstruct.of_string (string_of_int i) ]
    done;
    for _ = 1 to 2 do
      Eio.Net.send tx ~dst:(loopback 9000) [ Cstruct.of_string "stop" ]
    done
  in
  Eio.Fiber.all [ receiver; receiver; sender ];
  let times n =
    Array.fold_left
      (fun acc c -> if Atomic.get c = n then acc + 1 else acc)
      0 counts
  in
  Alcotest.(check int) "received once" (k - !dups) (times 1);
  Alcotest.(check int) "received twice" !dups (times 2)

(* Clients in two domains connect to one server: each connection's two ends
   are paired, so a reset of the server's end of an odd connection reaches
   its client, and only that one. *)
let test_connections_paired_across_domains () =
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let per_domain = 20 in
  let mine : int ref Eio.Fiber.key = Eio.Fiber.create_key () in
  let f (op : Op.t) =
    match op.kind with
    | `Connect ->
        Option.iter (fun r -> r := op.socket) (Eio.Fiber.get mine);
        None
    | `Read when op.side = `Server && op.socket mod 2 = 1 -> Some Fault.Reset
    | _ -> None
  in
  Eio.Switch.run @@ fun sw ->
  let net =
    Chaos.wrap ~clock:(Eio_mock.Clock.make ()) f (Eio_net_mem.v ~sw ())
  in
  let l = Eio.Net.listen ~sw ~backlog:64 net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string (read1 flow) flow));
  let wrong = Atomic.make 0 in
  let client () =
    Eio.Domain_manager.run domain_mgr (fun () ->
        for _ = 1 to per_domain do
          let socket = ref 0 in
          Eio.Fiber.with_binding mine socket (fun () ->
              Eio.Switch.run @@ fun sw ->
              let flow = Eio.Net.connect ~sw net addr in
              let reset =
                match
                  Eio.Flow.copy_string "ping" flow;
                  read1 flow
                with
                | "ping" -> false
                | _ -> true
                | exception e when is_reset e -> true
              in
              if reset <> (!socket mod 2 = 1) then Atomic.incr wrong)
        done)
  in
  Eio.Fiber.both client client;
  Alcotest.(check int) "misdirected faults" 0 (Atomic.get wrong)

let suite =
  ( "eio_net_chaos",
    [
      Alcotest.test_case "flow short reads" `Quick test_flow_short;
      Alcotest.test_case "flow stall coalesces writes" `Quick
        test_flow_stall_coalesces;
      Alcotest.test_case "flow buffered bytes before end of stream" `Quick
        test_flow_buffered_before_eof;
      Alcotest.test_case "flow torn write" `Quick test_flow_torn_write;
      Alcotest.test_case "flow reset at both ends" `Quick
        test_flow_reset_both_ends;
      Alcotest.test_case "flow end at both ends" `Quick test_flow_eof_both_ends;
      Alcotest.test_case "accept reset" `Quick test_accept_reset;
      Alcotest.test_case "accept EMFILE" `Quick test_accept_emfile;
      Alcotest.test_case "flow write after close" `Quick
        test_flow_write_after_close;
      Alcotest.test_case "flow buffer follows the read side" `Quick
        test_flow_buffer_follows_read_side;
      Alcotest.test_case "datagram drop" `Quick test_dgram_drop;
      Alcotest.test_case "datagram dup" `Quick test_dgram_dup;
      Alcotest.test_case "datagram reorder" `Quick test_dgram_reorder;
      Alcotest.test_case "datagram cancelled receive draws no fault" `Quick
        test_dgram_cancelled_recv_draws_no_fault;
      Alcotest.test_case "datagram held back kept whole" `Quick
        test_dgram_held_kept_whole;
      Alcotest.test_case "datagram close drops the held one" `Quick
        test_dgram_close_drops_held;
      Alcotest.test_case "datagram reorder survives a cancelled receive" `Quick
        test_dgram_reorder_cancelled;
      Alcotest.test_case "datagram cancelled drop keeps the count" `Quick
        test_dgram_cancelled_drop_keeps_count;
      Alcotest.test_case "datagram stall survives a cancelled receive" `Quick
        test_dgram_stall_cancelled;
      Alcotest.test_case "datagram garble" `Quick test_dgram_garble;
      Alcotest.test_case "net short reads" `Quick test_net_short_reads;
      Alcotest.test_case "net connect refused" `Quick test_net_connect_refused;
      Alcotest.test_case "net connect timeout" `Quick test_net_connect_timeout;
      Alcotest.test_case "net connect unreachable" `Quick
        test_net_connect_unreachable;
      Alcotest.test_case "net connect stall" `Quick test_net_connect_stall;
      Alcotest.test_case "net operations" `Quick test_ops;
      Alcotest.test_case "writes counted across domains" `Quick
        test_writes_counted_across_domains;
      Alcotest.test_case "reads shared across domains" `Quick
        test_reads_shared_across_domains;
      Alcotest.test_case "datagrams shared across domains" `Quick
        test_datagrams_shared_across_domains;
      Alcotest.test_case "connections paired across domains" `Quick
        test_connections_paired_across_domains;
    ] )
