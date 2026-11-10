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

let test_first_run_fails () =
  refused_with "Eio_net_chaos.check: the run with no fault failed: Eio.Io"
    (fun () ->
      Eio_net_chaos.check (fun env ->
          Eio.Switch.run @@ fun sw -> ignore (Eio.Net.connect ~sw env#net addr)))
    ()

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
  let rec go () =
    match
      Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw env#net addr))
    with
    | () -> ()
    | exception Eio.Io _ -> go ()
  in
  go ()

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
           "the run hangs: 10000 network operations failed in a row while its \
            clock stood still"
           spin);
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
