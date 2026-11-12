open Design

(* The guards of a check. *)

let read ~clock f = App.Frame.read ~clock f

let mutant ?budget ?replay () =
  After.test_d ?budget ?replay ~read:(App.Frame.read ~bug:`Single_read) ()

let error_is expected f () =
  let error = Expect.field "error" (Expect.report f) in
  Alcotest.(check string) "error" expected error

let refused_with prefix f () =
  let m = Expect.report f in
  if not (String.starts_with ~prefix m) then
    Alcotest.failf "the check failed with %S" m

(* A download whose client logs what it got before the end or an error,
   and when. *)
let logged log () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "0123456789" flow));
  let got = Buffer.create 10 in
  let buf = Cstruct.create 64 in
  let ended =
    try
      Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net addr in
      let rec loop () =
        let n = Eio.Flow.single_read flow buf in
        Buffer.add_string got (Cstruct.to_string ~len:n buf);
        loop ()
      in
      loop ()
    with
    | End_of_file -> "end"
    | Eio.Io _ -> "error"
    | Eio.Time.Timeout -> "timeout"
  in
  Fmt.kstr (Buffer.add_string log) "%S then %s at %g\n" (Buffer.contents got)
    ended (Eio.Time.now env#clock)

(* Two checks run the same scenarios and see the same results. *)
let test_determinism () =
  let a = Buffer.create 1024 and b = Buffer.create 1024 in
  logged a ();
  logged b ();
  Alcotest.(check string) "two checks" (Buffer.contents a) (Buffer.contents b);
  Alcotest.(check string)
    "two failures"
    (Expect.report (fun () -> mutant ()))
    (Expect.report (fun () -> mutant ()))

(* A cut lands at its byte: some runs read nothing, nine bytes, or all ten
   before a reset. *)
let test_cut_positions () =
  let log = Buffer.create 1024 in
  logged log ();
  let lines = String.split_on_char '\n' (Buffer.contents log) in
  List.iter
    (fun got ->
      let prefix = got in
      if not (List.exists (String.starts_with ~prefix) lines) then
        Alcotest.failf "no run read %S" got)
    [
      "\"\" then error"; "\"012345678\" then error"; "\"0123456789\" then error";
    ]

let replay_of report =
  Scanf.sscanf (Expect.field "replay" report) "~replay:%S" Fun.id

let same_failure a b =
  List.iter
    (fun f -> Alcotest.(check string) f (Expect.field f a) (Expect.field f b))
    [ "faults"; "tier"; "error"; "replay" ]

(* A replay runs the scenario a failure named, and fails the same way; once
   the bug is fixed, it passes. *)
let test_replay () =
  let report = Expect.report (fun () -> mutant ()) in
  let replay = replay_of report in
  same_failure report (Expect.report (fun () -> mutant ~replay ()));
  After.test_d ~replay ~read ();
  let report = Expect.report (fun () -> resume ~bug:`Stale ()) in
  let replay = replay_of report in
  same_failure report (Expect.report (fun () -> resume ~replay ~bug:`Stale ()))

let hash replay = List.nth (String.split_on_char '/' replay) 1

(* A client that reads a message of ten bytes in reads of [size] bytes. With
   [bug] it takes a reset for the end of the message. *)
let message ?replay ~size ~bug () =
  Eio_net_chaos.check ?replay @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "0123456789" flow));
  let got = Buffer.create 10 in
  let buf = Cstruct.create size in
  let rec read flow =
    match Eio.Flow.single_read flow buf with
    | n ->
        Buffer.add_string got (Cstruct.to_string ~len:n buf);
        if Buffer.length got < 10 then read flow
    | exception Eio.Io (Eio.Net.E (Connection_reset _), _) when bug -> ()
  in
  match
    Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
    Eio.Switch.run @@ fun sw -> read (Eio.Net.connect ~sw env#net addr)
  with
  | () ->
      if Buffer.contents got <> "0123456789" then
        Fmt.failwith "got %S" (Buffer.contents got)
  | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) -> ()

(* A replay whose workload's first run changed, but which still has the
   scenario, runs it: the fix changed the size of the client's reads. *)
let test_rematched_replay () =
  let report = Expect.report (message ~size:64 ~bug:true) in
  let replay = replay_of report in
  message ~replay ~size:4 ~bug:false ();
  let again = Expect.report (message ~replay ~size:4 ~bug:true) in
  Alcotest.(check string)
    "faults"
    (Expect.field "faults" report)
    (Expect.field "faults" again);
  Alcotest.(check string)
    "re-matched"
    (Fmt.str "the replay named first run %s, and the workload's is now %s"
       (hash replay)
       (hash (replay_of again)))
    (Expect.field "matched" again)

(* A replay whose scenario the workload no longer has is refused. *)
let test_stale_replay () =
  let replay = replay_of (Expect.report (fun () -> mutant ())) in
  let other env =
    Eio.Switch.run @@ fun sw ->
    try ignore (Eio.Net.connect ~sw env#net addr) with Eio.Io _ -> ()
  in
  let key = List.nth (String.split_on_char '/' replay) 2 in
  refused_with
    (Fmt.str
       "Eio_net_chaos.check: the replay names %s, a scenario this check no \
        longer has: its first run was %s and is now"
       key (hash replay))
    (fun () -> Eio_net_chaos.check ~replay other)
    ()

let test_unknown_replay () =
  let replay = replay_of (Expect.report (fun () -> mutant ())) in
  refused_with
    "Eio_net_chaos.check: the replay names read9c@0.reset, a scenario"
    (fun () ->
      mutant ~replay:(Fmt.str "net/%s/read9c@0.reset" (hash replay)) ())
    ();
  refused_with
    "Eio_net_chaos.check: ~replay:\"fs/x/y\" is not a replay of eio-net"
    (fun () -> mutant ~replay:"fs/x/y" ())
    ()

let test_budget () =
  List.iter
    (fun budget ->
      match mutant ~budget () with
      | exception Invalid_argument _ -> ()
      | () -> Alcotest.failf "the budget %d was accepted" budget
      | exception Failure _ ->
          Alcotest.failf "the budget %d was accepted" budget)
    [ 0; -1 ]

(* A server that serves one connection, closes first, and listens again on
   its port. Its closed end holds the port in TIME_WAIT for 60 s, so the
   second listen needs [reuse_addr], as on Linux. *)
let restart ~reuse_addr () =
  Eio_net_chaos.check @@ fun env ->
  let serve_once () =
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~reuse_addr ~sw ~backlog:1 env#net addr in
    Eio.Fiber.both
      (fun () ->
        try
          Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
          Eio.Switch.run @@ fun sw ->
          let flow, _ = Eio.Net.accept ~sw l in
          Eio.Flow.copy_string "hi\n" flow
        with Eio.Io _ | Eio.Time.Timeout -> ())
      (fun () ->
        try
          Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
          Eio.Switch.run @@ fun sw ->
          let flow = Eio.Net.connect ~sw env#net addr in
          Eio.Time.sleep env#clock 0.1;
          ignore (Eio.Flow.read_all flow)
        with Eio.Io _ | Eio.Time.Timeout -> ())
  in
  serve_once ();
  serve_once ()

let test_restart () =
  Expect.passes (restart ~reuse_addr:true) ();
  let m = Expect.report (restart ~reuse_addr:false) in
  if
    not
      (String.starts_with
         ~prefix:
           "Eio_net_chaos.check: the run with no fault failed: \
            Unix.Unix_error(Unix.EADDRINUSE, \"bind\""
         m)
  then Alcotest.failf "failed with %S" m

let test_nothing () =
  refused_with "Eio_net_chaos.check: the workload makes no network operation"
    (fun () -> Eio_net_chaos.check ignore)
    ()

let test_nondeterministic () =
  let calls = ref 0 in
  let run env =
    incr calls;
    Eio.Switch.run @@ fun sw ->
    for _ = 1 to 1 + (!calls mod 2) do
      try ignore (Eio.Net.connect ~sw env#net addr) with Eio.Io _ -> ()
    done
  in
  refused_with
    "Eio_net_chaos.check: two runs with no fault made different operations"
    (fun () -> Eio_net_chaos.check run)
    ()

(* A run with no fault that fails is reported with its operations and the
   replay that runs it again. *)
let test_first_run_fails () =
  let run ?replay () =
    Eio_net_chaos.check ?replay (fun env ->
        Eio.Switch.run @@ fun sw -> ignore (Eio.Net.connect ~sw env#net addr))
  in
  refused_with "Eio_net_chaos.check: the run with no fault failed: Eio.Io" run
    ();
  let report = Expect.report run in
  Alcotest.(check string) "trace" "" (Expect.field "trace" report);
  if not (List.mem "    #1 connect 1c @0" (String.split_on_char '\n' report))
  then Alcotest.failf "no operation in %S" report;
  let replay = replay_of report in
  Alcotest.(check bool) "replay" true (String.ends_with ~suffix:"/none" replay);
  Alcotest.(check string) "replayed" report (Expect.report (run ~replay))

(* A check runs inside another event loop as it does outside one. *)
let test_nested () =
  Eio_mock.Backend.run @@ fun () ->
  After.test_d ~read ();
  ignore (Expect.report (fun () -> mutant ()))

(* A client that retries a refused connect at once spins in the outage,
   where every connect is refused and no time passes. *)
let spin () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun _ _ -> ()));
  (* Its own bound, well past the guard's: without the guard, the run fails
     here instead of spinning for ever. *)
  let rec go n =
    if n > 50_000 then failwith "the spin was not stopped";
    match
      Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw env#net addr))
    with
    | () -> ()
    | exception Eio.Io _ -> go (n + 1)
  in
  go 0

(* A client that reconnects at once after each read that fails or ends, to
   a server that closes every connection unanswered, spins even with no
   fault: an end of file is no failure, but the run makes more network
   operations than a check plans from. *)
let reconnect () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun _ _ -> ()));
  (* Its own bound, well past the guard's: without the guard, the run fails
     here instead of spinning for ever. *)
  let rec go n =
    if n > 2_000_000 then failwith "the spin was not stopped";
    match
      Eio.Switch.run (fun sw ->
          let flow = Eio.Net.connect ~sw env#net addr in
          Eio.Flow.copy_string "hi\n" flow;
          Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:16))
    with
    | _ -> ()
    | exception (Eio.Io _ | End_of_file) -> go (n + 1)
  in
  go 0

(* A batch of jobs, each acknowledged by the server's close: each read ends
   a new connection, an answer and no failure, so the run with no fault
   passes, and only the budget stops the check. *)
let batch () =
  Eio_net_chaos.check ~budget:30_000 @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          ignore (Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:16))));
  for _ = 1 to 10_005 do
    try
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net addr in
      Eio.Flow.copy_string "job\n" flow;
      match Eio.Flow.single_read flow (Cstruct.create 1) with
      | _ -> failwith "the job was answered"
      | exception End_of_file -> ()
    with Eio.Io _ -> ()
  done

(* A server that a fault breaks closes each later connection at once, and
   its client reconnects at once after each read that fails or ends: a spin
   no failure counts, which makes more network operations than a check plans
   from. *)
let broken_server () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  let broken = ref false in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          if not !broken then
            match
              Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:16)
            with
            | _ -> Eio.Flow.copy_string "ok\n" flow
            | exception e ->
                broken := true;
                raise e));
  (* Its own bound, well past the check's: without it, the run fails here
     instead of spinning for ever. *)
  let rec go n =
    if n > 2_000_000 then failwith "the spin was not stopped";
    match
      Eio.Switch.run (fun sw ->
          let flow = Eio.Net.connect ~sw env#net addr in
          Eio.Flow.copy_string "hi\n" flow;
          Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:16))
    with
    | _ -> ()
    | exception (Eio.Io _ | End_of_file) -> go (n + 1)
  in
  go 0

(* A scan that tries 10500 closed ports once each, as the clock stands
   still, is not a spin: the check's first run passes, and only the budget
   stops it. *)
let scan () =
  Eio_net_chaos.check ~budget:50 @@ fun env ->
  for port = 30000 to 40500 do
    match
      Eio.Switch.run (fun sw ->
          ignore
            (Eio.Net.connect ~sw env#net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))))
    with
    | () -> ()
    | exception Eio.Io _ -> ()
  done

(* A client that pings an echo peer and polls with [Fiber.yield] for the
   first pong, pinging again on each poll, giving up after [patience]
   seconds of its clock if it has any. Time passes while it polls, so a pong
   held back 50 ms arrives, though the peer takes a ping on every poll, and
   in the outage, where each is lost, a patient client gives up. *)
let poll ?patience () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let udp port = `Udp (Eio.Net.Ipaddr.V4.loopback, port) in
  let client = Eio.Net.datagram_socket ~sw env#net (udp 5000) in
  let peer = Eio.Net.datagram_socket ~sw env#net (udp 5001) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Cstruct.create 8 in
      while true do
        let from, _ = Eio.Net.recv peer buf in
        Eio.Net.send peer ~dst:from [ Cstruct.of_string "pong" ]
      done;
      `Stop_daemon);
  let got = ref false in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      ignore (Eio.Net.recv client (Cstruct.create 8));
      got := true;
      `Stop_daemon);
  let start = Eio.Time.now env#clock in
  let waited () =
    match patience with
    | Some p -> Eio.Time.now env#clock -. start >= p
    | None -> false
  in
  (* Its own bound, well past the polls a check lets through: without time
     passing, the run fails here instead of spinning for ever. *)
  let polls = ref 0 in
  while (not !got) && not (waited ()) do
    incr polls;
    if !polls > 5_000_000 then failwith "polled 5000000 times";
    Eio.Net.send client ~dst:(udp 5001) [ Cstruct.of_string "ping" ];
    Eio.Fiber.yield ()
  done;
  if (not !got) && not (Eio_net_chaos.outage env) then failwith "no reply"

(* A server that yields 200000 times while it computes its answer, and a
   client that waits 30 s for it. A real server takes a few milliseconds
   for that, so the client's timeout never fires. *)
let compute () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:16 in
          let line () = App.strict_line r in
          match Eio.Time.with_timeout_exn env#clock 5.0 line with
          | "sum" ->
              let sum = ref 0 in
              for i = 1 to 200_000 do
                sum := !sum + i;
                Eio.Fiber.yield ()
              done;
              Eio.Flow.copy_string (Fmt.str "%d\n" !sum) flow
          | _ -> ()));
  let rec ask n =
    match
      Eio.Time.with_timeout_exn env#clock 30.0 @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net addr in
      Eio.Flow.copy_string "sum\n" flow;
      App.strict_line (Eio.Buf_read.of_flow flow ~max_size:16)
    with
    | answer -> Some answer
    | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
        if n < 3 then ask (n + 1) else None
  in
  match ask 1 with
  | Some "20000100000" -> ()
  | Some answer -> Fmt.failwith "got %S" answer
  | None -> if not (Eio_net_chaos.outage env) then failwith "no answer"

(* A server that streams to each client until a write fails, and a client
   that reads 1 MiB and leaves. The server's writes wait while its socket's
   buffer is full, as on Linux, so the client runs and its close ends them. *)
let stream () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:5 env#net addr in
  let unbounded = ref false in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let chunk = Cstruct.create 65536 in
          (* Its own bound: without one, the run queues for ever. *)
          for _ = 1 to 1024 do
            Eio.Flow.write flow [ chunk ]
          done;
          unbounded := true));
  (try
     Eio.Time.with_timeout_exn env#clock 30.0 @@ fun () ->
     Eio.Switch.run @@ fun sw ->
     let flow = Eio.Net.connect ~sw env#net addr in
     let r = Eio.Buf_read.of_flow flow ~max_size:(1 lsl 20) in
     ignore (Eio.Buf_read.take (1 lsl 20) r : string)
   with Eio.Io _ | End_of_file | Eio.Time.Timeout -> ());
  if !unbounded then failwith "64 MiB queued to a client that reads 1 MiB"

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* The operations of a report between its [trace:] and [replay:] lines. *)
let trace_lines report =
  let rec drop = function
    | l :: rest when l = "  trace:" -> rest
    | _ :: rest -> drop rest
    | [] -> []
  in
  let rec take = function
    | l :: _ when String.starts_with ~prefix:"  replay:" l -> []
    | l :: rest -> l :: take rest
    | [] -> []
  in
  take (drop (String.split_on_char '\n' report))

(* A client that retries a refused connect 1200 times, then fails: each
   connect is a new socket, and the report's trace shows the first and the
   last of them. *)
let test_report_collapses_by_shape () =
  let run () =
    Eio_net_chaos.check @@ fun env ->
    for _ = 1 to 1200 do
      try Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw env#net addr))
      with Eio.Io _ -> ()
    done;
    failwith "no server"
  in
  let lines = trace_lines (Expect.report run) in
  if List.length lines > 10 then
    Alcotest.failf "the trace of 1200 connects has %d lines:@.%s"
      (List.length lines) (String.concat "\n" lines)

(* A write cut where a reset lands says so: the write before it moves what
   comes before the reset. *)
let test_report_names_the_cut () =
  let run ?replay () =
    Eio_net_chaos.check ?replay @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
            Eio.Flow.copy_string "0123456789" flow));
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    let got = Eio.Flow.read_all flow in
    if got <> "0123456789" then Fmt.failwith "got %S" got
  in
  let h = hash (replay_of (Expect.report run)) in
  let report =
    Expect.report (run ~replay:(Fmt.str "net/%s/write1s@1.reset" h))
  in
  let cut = "<- moves 1 byte(s), up to where it is reset" in
  if
    not
      (List.exists
         (fun l -> String.ends_with ~suffix:cut l)
         (trace_lines report))
  then Alcotest.failf "no line ends with %S in:@.%s" cut report

let test_malformed_replay () =
  List.iter
    (fun replay ->
      refused_with
        (Fmt.str "Eio_net_chaos.check: ~replay:%S is not a replay of eio-net"
           replay)
        (fun () -> mutant ~replay ())
        ())
    [ "net/zzzz/none"; "net/0123456/none"; "net/0123456789/none" ]

(* A client that connects and reads to the server's close for ever, with no
   failure: the report says the workload looks like a busy loop. *)
let test_loop_named () =
  let run () =
    Eio_net_chaos.check @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun _ _ -> ()));
    while true do
      try
        Eio.Switch.run @@ fun sw ->
        ignore (Eio.Flow.read_all (Eio.Net.connect ~sw env#net addr) : string)
      with Eio.Io _ -> ()
    done
  in
  let report = Expect.report run in
  let says = "a busy loop" in
  if not (contains report says) then
    Alcotest.failf "the report does not say %S: %s" says report

(* One network operation, which a fault may fail: the first run of a
   workload that probes its clock must make one. *)
let touch (env : Eio_net_chaos.env) =
  try Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw env#net addr))
  with Eio.Io _ -> ()

(* A fiber that yields 3000 times while it computes, and another that polls
   for its result with [Fiber.yield], under a 50 ms deadline: a few
   milliseconds of work on eio_posix. *)
let deadline_compute () =
  Eio_net_chaos.check @@ fun env ->
  touch env;
  let flag = ref false in
  Eio.Time.with_timeout_exn env#clock 0.05 @@ fun () ->
  Eio.Fiber.both
    (fun () ->
      for _ = 1 to 3000 do
        Eio.Fiber.yield ()
      done;
      flag := true)
    (fun () ->
      while not !flag do
        Eio.Fiber.yield ()
      done)

(* A handshake whose ends each yield 5000 times while they derive a key,
   under a 100 ms deadline per attempt. *)
let handshake () =
  let derive () =
    for _ = 1 to 5000 do
      Eio.Fiber.yield ()
    done
  in
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:16 in
          match
            Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
                App.strict_line r)
          with
          | "hello" ->
              derive ();
              Eio.Flow.copy_string "key\n" flow
          | _ -> ()));
  let rec attempt n =
    match
      Eio.Time.with_timeout_exn env#clock 0.1 @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net addr in
      Eio.Flow.copy_string "hello\n" flow;
      let key = App.strict_line (Eio.Buf_read.of_flow flow ~max_size:16) in
      derive ();
      key
    with
    | key -> Some key
    | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
        if n < 3 then attempt (n + 1) else None
  in
  match attempt 1 with
  | Some "key" -> ()
  | Some key -> Fmt.failwith "got %S" key
  | None -> if not (Eio_net_chaos.outage env) then failwith "no key"

(* Two ends that ping-pong one byte over a connection until a 10 ms timer
   says stop: a few thousand round trips on eio_posix. *)
let pingpong () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let buf = Cstruct.create 1 in
          while true do
            ignore
              (Eio.Time.with_timeout_exn env#clock 1.0 (fun () ->
                   Eio.Flow.single_read flow buf));
            Eio.Flow.copy_string "y" flow
          done));
  let stop = ref false in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep env#clock 0.01;
      stop := true);
  (* Its own bound, well past the round trips 10 ms allow: without time
     passing, the run fails here instead of running for ever. *)
  let trips = ref 0 in
  try
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    let buf = Cstruct.create 1 in
    while not !stop do
      incr trips;
      if !trips > 100_000 then failwith "100000 round trips in 10 ms";
      Eio.Flow.copy_string "x" flow;
      ignore
        (Eio.Time.with_timeout_exn env#clock 1.0 (fun () ->
             Eio.Flow.single_read flow buf))
    done
  with Eio.Io _ | End_of_file | Eio.Time.Timeout -> ()

(* The same over UDP, until a 0.5 ms timer: each datagram received is a
   scenario of the linear tier, so the default budget covers a hundred round
   trips. *)
let udp_pingpong () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let udp port = `Udp (Eio.Net.Ipaddr.V4.loopback, port) in
  let a = Eio.Net.datagram_socket ~sw env#net (udp 5010) in
  let b = Eio.Net.datagram_socket ~sw env#net (udp 5011) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Cstruct.create 8 in
      while true do
        let from, _ = Eio.Net.recv b buf in
        Eio.Net.send b ~dst:from [ Cstruct.of_string "y" ]
      done;
      `Stop_daemon);
  let stop = ref false in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep env#clock 0.0005;
      stop := true);
  let trips = ref 0 in
  let buf = Cstruct.create 8 in
  while not !stop do
    incr trips;
    if !trips > 100_000 then failwith "100000 round trips in 0.5 ms";
    Eio.Net.send a ~dst:(udp 5011) [ Cstruct.of_string "x" ];
    try
      ignore
        (Eio.Time.with_timeout_exn env#clock 1.0 (fun () -> Eio.Net.recv a buf))
    with Eio.Time.Timeout -> ()
  done

(* A loop that sleeps 1 us until a flag no one sets. *)
let tiny_sleeps () =
  Eio_net_chaos.check @@ fun env ->
  touch env;
  (* Its own bound, past the turns a run may take: without it, a run with no
     limit on its turns sleeps until its clock reaches the hour. *)
  let n = ref 0 in
  while true do
    incr n;
    if !n > 20_000_000 then failwith "slept 20000000 times";
    Eio.Time.sleep env#clock 1e-6
  done

(* A loop that yields 9,900,000 times, then sleeps 1 ms, for ever: each
   sleep ends a run of turns in a row, but its turns are nearly all of its
   clock. *)
let bursts () =
  Eio_net_chaos.check @@ fun env ->
  touch env;
  (* Its own bound, past the turns a run may take in all: without it, a run
     with no such limit yields until its clock reaches the hour. *)
  let n = ref 0 in
  while true do
    incr n;
    if !n > 3 then failwith "ran 3 bursts";
    for _ = 1 to 9_900_000 do
      Eio.Fiber.yield ()
    done;
    Eio.Time.sleep env#clock 0.001
  done

(* A poll with [Fiber.yield] for a flag no one sets. *)
let yield_poll () =
  Eio_net_chaos.check @@ fun env ->
  touch env;
  let flag = ref false in
  while not !flag do
    Eio.Fiber.yield ()
  done

(* A workload of two operations under a budget of one run: the budget is
   too small, and the report says only that. *)
let test_tiny_budget () =
  let report =
    Expect.report @@ fun () ->
    Eio_net_chaos.check ~budget:1 @@ fun env ->
    touch env;
    touch env
  in
  let says = "more than the budget of 1" in
  if not (contains report says) then
    Alcotest.failf "the report does not say %S: %s" says report;
  if contains report "loops for ever" then
    Alcotest.failf "the report calls the workload a loop: %s" report

(* A check keeps no run's operations once it has planned from them: over
   the 1500 runs of a linear tier, each of 300 datagrams, the live words grow
   by less than 100000, where the operations of every run take millions. *)
let test_linear_memory () =
  let runs = ref 0 and first = ref 0 and most = ref 0 in
  let udp port = `Udp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio_net_chaos.check (fun env ->
      incr runs;
      ( Eio.Switch.run @@ fun sw ->
        let sink = Eio.Net.datagram_socket ~sw env#net (udp 5020) in
        let src = Eio.Net.datagram_socket ~sw env#net (udp 5021) in
        let buf = Cstruct.create 16 in
        for _ = 1 to 300 do
          Eio.Net.send src ~dst:(udp 5020) [ Cstruct.of_string "x" ];
          try
            ignore
              (Eio.Time.with_timeout_exn env#clock 0.1 (fun () ->
                   Eio.Net.recv sink buf))
          with Eio.Time.Timeout -> ()
        done );
      if !runs mod 100 = 2 then begin
        Gc.full_major ();
        let live = (Gc.stat ()).live_words in
        if !runs = 2 then first := live else most := max !most live
      end);
  let grown = !most - !first in
  if grown > 100_000 then
    Alcotest.failf "the live words grew by %d over the check" grown

(* Three refused connects, then a failure: the report shows each, as
   collapsing them would hide one line behind another. *)
let test_report_shows_three () =
  let report =
    Expect.report @@ fun () ->
    Eio_net_chaos.check @@ fun env ->
    for _ = 1 to 3 do
      touch env
    done;
    failwith "no server"
  in
  let lines = trace_lines report in
  if
    List.length lines <> 3
    || List.exists (fun l -> contains l "more time") lines
  then
    Alcotest.failf "the trace of three connects is:@.%s"
      (String.concat "\n" lines)

(* A replay whose scenario is not one a check names is malformed. *)
let test_malformed_scenario () =
  let h = hash (replay_of (Expect.report (fun () -> mutant ()))) in
  List.iter
    (fun k ->
      let replay = Fmt.str "net/%s/%s" h k in
      refused_with
        (Fmt.str "Eio_net_chaos.check: ~replay:%S is not a replay of eio-net"
           replay)
        (fun () -> mutant ~replay ())
        ())
    [
      "read1c@0.reset+read1c@1.reset+read1c@2.reset";
      "read1c@x.reset";
      "read1c@0.explode";
      "read1c@0.reset+";
      "read1x@0.reset";
      "";
    ]

let first_fails error =
  refused_with ("Eio_net_chaos.check: the run with no fault failed: " ^ error)

let busy =
  "the run woke 10,000,000 times without waiting 1 ms or more: a busy loop"

let busy_cpu =
  "the run kept the CPU busy for 10 s, more than half its time: a busy loop"

let ops =
  "the run hangs in a busy loop, or is too large for a check: it made 100000 \
   network operations"

(* 200 fibers on a 20 ms heartbeat for 1200 s of the clock: twelve million
   turns, each heartbeat a wait. *)
let heartbeats () =
  Eio_net_chaos.check ~budget:2 @@ fun env ->
  touch env;
  Eio.Fiber.all
    (List.init 200 (fun _ () ->
         while Eio.Time.now env#clock < 1200. do
           Eio.Time.sleep env#clock 0.02
         done))

(* A ping-pong that never ends holds a bounded trace while it runs: the
   live words, sampled as it goes, stay under three million. *)
let test_busy_memory () =
  let most = ref 0 and first = ref None in
  let sample () =
    Gc.full_major ();
    let live = (Gc.stat ()).live_words in
    match !first with
    | None -> first := Some live
    | Some f -> most := max !most (live - f)
  in
  let report =
    Expect.report @@ fun () ->
    Eio_net_chaos.check @@ fun env ->
    first := None;
    Eio.Switch.run @@ fun sw ->
    let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
            let buf = Cstruct.create 1 in
            while true do
              ignore (Eio.Flow.single_read flow buf);
              Eio.Flow.copy_string "y" flow
            done));
    let flow = Eio.Net.connect ~sw env#net addr in
    let buf = Cstruct.create 1 in
    let n = ref 0 in
    while true do
      if !n mod 25_000 = 0 then sample ();
      incr n;
      Eio.Flow.copy_string "x" flow;
      ignore (Eio.Flow.single_read flow buf)
    done
  in
  if not (contains report "a busy loop") then
    Alcotest.failf "the report does not call it a busy loop: %s" report;
  if !most > 3_000_000 then
    Alcotest.failf "the live words grew by %d while it ran" !most

let suite =
  ( "guards",
    [
      Alcotest.test_case "a run every fiber waits in hangs" `Quick
        (error_is "the run hangs: every fiber waits and no timer is due"
           (After.test_d ~read:(App.Frame.read ~bug:`No_timeout)));
      Alcotest.test_case "a run that keeps a timer going hangs" `Quick
        (error_is
           "the run hangs: it is still running after an hour of its clock"
           ticking);
      Alcotest.test_case "a run that fails with the clock stopped hangs" `Quick
        (error_is
           "the run hangs: the same network operation failed 10000 times on \
            the same target while its clock stood still"
           spin);
      Alcotest.test_case "a client that reconnects at once spins" `Quick
        (first_fails ops reconnect);
      Alcotest.test_case "a batch of jobs ended by the server is no spin" `Quick
        (refused_with
           "Eio_net_chaos.check: the linear tier needs 190100 runs, more than \
            the budget of 30000"
           batch);
      Alcotest.test_case "a client of a server a fault broke spins" `Quick
        (error_is ops broken_server);
      Alcotest.test_case "a client that polls with a deadline" `Quick
        (Expect.passes (poll ~patience:1.0));
      Alcotest.test_case "a client that polls for ever hangs" `Quick
        (error_is busy (poll ?patience:None));
      Alcotest.test_case "fibers that yield while they compute meet a deadline"
        `Quick
        (Expect.passes deadline_compute);
      Alcotest.test_case "a handshake that computes meets its deadline" `Quick
        (Expect.passes handshake);
      Alcotest.test_case "a ping-pong until a timer ends" `Quick
        (Expect.passes pingpong);
      Alcotest.test_case "a UDP ping-pong until a timer ends" `Quick
        (Expect.passes udp_pingpong);
      Alcotest.test_case "a loop of 1 us sleeps is a busy loop" `Quick
        (first_fails busy tiny_sleeps);
      Alcotest.test_case "a poll that never succeeds is a busy loop" `Quick
        (first_fails busy yield_poll);
      Alcotest.test_case "bursts of yields between 1 ms sleeps are a busy loop"
        `Quick
        (first_fails busy_cpu bursts);
      Alcotest.test_case "a budget too small says so" `Quick test_tiny_budget;
      Alcotest.test_case "many fibers on a heartbeat are no busy loop" `Quick
        (refused_with "Eio_net_chaos.check: the linear tier needs" heartbeats);
      Alcotest.test_case "a busy loop's trace is bounded" `Quick
        test_busy_memory;
      Alcotest.test_case "a check keeps no run's operations" `Quick
        test_linear_memory;
      Alcotest.test_case "a report collapses no single line" `Quick
        test_report_shows_three;
      Alcotest.test_case "a malformed scenario is refused" `Quick
        test_malformed_scenario;
      Alcotest.test_case "a server that yields while it computes" `Quick
        (Expect.passes compute);
      Alcotest.test_case "a server that streams until its client leaves" `Quick
        (Expect.passes stream);
      Alcotest.test_case "a report collapses operations of one shape" `Quick
        test_report_collapses_by_shape;
      Alcotest.test_case "a report names the cut before a reset" `Quick
        test_report_names_the_cut;
      Alcotest.test_case "a malformed replay is refused" `Quick
        test_malformed_replay;
      Alcotest.test_case "a report names a loop" `Quick test_loop_named;
      Alcotest.test_case "a scan of closed ports is not a spin" `Quick
        (refused_with "Eio_net_chaos.check: the linear tier needs" scan);
      Alcotest.test_case "a large transfer fits the default budget" `Quick
        (Expect.passes (transfer 25 4096));
      Alcotest.test_case "a linear tier past the budget fails" `Quick
        (refused_with
           "Eio_net_chaos.check: the linear tier needs 69 runs, more than the \
            budget of 20"
           (transfer ~budget:20 25 4096));
      Alcotest.test_case "resume: correct" `Quick
        (Expect.passes (fun () -> resume ()));
      Alcotest.test_case "resume: stale offset, by the sampled tier" `Quick
        (Expect.caught ~tier:"sampled" (fun () -> resume ~bug:`Stale ()));
      Alcotest.test_case "determinism" `Quick test_determinism;
      Alcotest.test_case "a cut lands at its byte" `Quick test_cut_positions;
      Alcotest.test_case "replay" `Quick test_replay;
      Alcotest.test_case "a replay after a fix is re-matched" `Quick
        test_rematched_replay;
      Alcotest.test_case "a stale replay is refused" `Quick test_stale_replay;
      Alcotest.test_case "an unknown replay is refused" `Quick
        test_unknown_replay;
      Alcotest.test_case "a budget that is not positive is refused" `Quick
        test_budget;
      Alcotest.test_case "a restarted server needs reuse_addr" `Quick
        test_restart;
      Alcotest.test_case "a workload with no operation is refused" `Quick
        test_nothing;
      Alcotest.test_case "a nondeterministic workload is refused" `Quick
        test_nondeterministic;
      Alcotest.test_case "a first run that fails is refused" `Quick
        test_first_run_fails;
      Alcotest.test_case "a check inside another loop" `Quick test_nested;
    ] )
