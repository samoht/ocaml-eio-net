(* What a check relies on in Quiet_mock, the silent copy of Eio_mock's
   scheduler and clock: a clock that prints nothing, starts at 0 and moves 1 us
   per turn, a run that stops at its time limit instead of moving past it, a
   deadlock, a stop and an idle hook, the turn limit and its two verdicts,
   [charge] for work that does not suspend, and effects a hook answers at
   once, as [Eio_unix.run_in_systhread]. eio-fs and eio-net carry the same
   copy, and this same file. *)

let us n = Mtime.Span.(n * us)
let ns mono = Mtime.to_uint64_ns (Eio.Time.Mono.now mono)

let run ?idle ?stop ?effects ?(limit = 3600.) main =
  Quiet_mock.run ?idle ?stop ?effects ~limit main

let check_ns what expected got = Alcotest.(check int64) what expected got

(* [silenced fn] is what [fn] wrote on stderr, the descriptor itself, where
   Eio_mock's clock writes from the scheduler. *)
let silenced fn =
  let file = Filename.temp_file "quiet_mock" ".err" in
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile file [ O_WRONLY; O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr;
      Unix.close saved)
    fn;
  let written = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  written

let test_silent () =
  let written =
    silenced (fun () ->
        run @@ fun clock mono _ ->
        Eio.Time.sleep clock 1.;
        Eio.Time.Mono.sleep mono 1.;
        Eio.Fiber.both
          (fun () -> Eio.Time.sleep clock 2.)
          (fun () -> Eio.Time.Mono.sleep mono 3.))
  in
  Alcotest.(check string) "nothing on stderr" "" written

(* The clocks start at 0, and each turn, a fiber resumed, costs 1 us. *)
let test_turns () =
  run @@ fun clock mono _ ->
  check_ns "at the start" 0L (ns mono);
  Alcotest.(check (float 0.))
    "the wall clock at the start" 0. (Eio.Time.now clock);
  for _ = 1 to 5 do
    Eio.Fiber.yield ()
  done;
  check_ns "five yields" 5_000L (ns mono);
  Eio.Fiber.both Eio.Fiber.yield Eio.Fiber.yield;
  Alcotest.(check bool) "two fibers' turns" true (ns mono > 5_000L)

(* With every fiber waiting, the clock moves to the next timer. *)
let test_timer () =
  run @@ fun _ mono _ ->
  Eio.Time.Mono.sleep_span mono Mtime.Span.(5 * s);
  Alcotest.(check bool)
    "woken at the timer, the turn after" true
    (ns mono >= 5_000_000_000L && ns mono <= 5_000_001_000L)

(* A run that would move the clock past its limit stops instead, whether a
   timer or its turns would move it. *)
let test_limit () =
  (match run ~limit:5. @@ fun clock _ _ -> Eio.Time.sleep clock 10. with
  | () -> Alcotest.fail "a sleep past the limit returned"
  | exception Quiet_mock.Time_limit -> ());
  let last = ref 0L in
  match
    run ~limit:0.001 @@ fun _ mono _ ->
    while true do
      last := ns mono;
      Eio.Fiber.yield ()
    done
  with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Time_limit ->
      Alcotest.(check bool)
        "the clock stopped at the limit" true
        (!last <= 1_000_000L && !last >= 999_000L)

let test_deadlock () =
  match
    run @@ fun _ _ _ ->
    let p, _ = Eio.Promise.create () in
    Eio.Promise.await p
  with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Deadlock_detected waiting ->
      Alcotest.(check int) "the fibers that wait" 1 (List.length waiting)

let test_stop () =
  let stop = ref false in
  match
    run ~stop:(fun () -> !stop) @@ fun _ _ _ ->
    stop := true;
    Eio.Fiber.yield ();
    Alcotest.fail "a turn ran once stop held"
  with
  | () -> Alcotest.fail "a stopped run returned"
  | exception Quiet_mock.Stopped -> ()

(* [idle] runs when every fiber waits; when it wakes one, the clock stays. *)
let test_idle () =
  let wake = ref None in
  let idle () =
    match !wake with
    | Some r ->
        wake := None;
        Eio.Promise.resolve r ();
        true
    | None -> false
  in
  run ~idle @@ fun _ mono _ ->
  let p, r = Eio.Promise.create () in
  wake := Some r;
  let woken = ref 0L in
  Eio.Fiber.both
    (fun () ->
      Eio.Promise.await p;
      woken := ns mono)
    (fun () -> Eio.Time.Mono.sleep mono 10.);
  Alcotest.(check bool) "woken before the timer" true (!woken < 10_000L);
  Alcotest.(check bool) "the timer after" true (ns mono >= 10_000_000_000L)

let busy_loop () =
  while true do
    Eio.Fiber.yield ()
  done

(* After [max_turns] turns a run stops: as a busy loop if no fiber waited
   1 ms or more for a timer in the last [max_turns], and as too long
   otherwise. *)
let test_max_turns () =
  Alcotest.(check int) "max_turns" 10_000_000 Quiet_mock.max_turns;
  (match
     run @@ fun _ mono _ ->
     Eio.Time.Mono.sleep_span mono Mtime.Span.(2 * ms);
     busy_loop ()
   with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Busy -> ());
  match
    run @@ fun _ mono _ ->
    while true do
      for _ = 1 to 1000 do
        Eio.Fiber.yield ()
      done;
      Eio.Time.Mono.sleep_span mono Mtime.Span.(2 * ms)
    done
  with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Too_long -> ()

(* [charge ()] takes a turn without suspending: the clock moves 1 us, the
   limit stops it, and its turns count to [max_turns]. *)
let test_charge () =
  run (fun _ mono charge ->
      charge ();
      charge ();
      check_ns "two charges" 2_000L (ns mono));
  (match
     run ~limit:0.001 @@ fun _ _ charge ->
     while true do
       charge ()
     done
   with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Time_limit -> ());
  match
    run @@ fun _ _ charge ->
    while true do
      charge ()
    done
  with
  | () -> Alcotest.fail "the run returned"
  | exception Quiet_mock.Busy -> ()

(* The effect of [Eio_unix.run_in_systhread], answered in the calling
   thread, as eio-net's checks answer it. *)
let systhread =
  {
    Quiet_mock.handle =
      (fun (type a) (e : a Effect.t) : (unit -> a) option ->
        match e with
        | Eio_unix.Private.Thread_pool.Run_in_systhread f ->
            Some
              (fun () ->
                let r =
                  match f () with
                  | v -> Ok v
                  | exception ex -> Error (ex, Printexc.get_raw_backtrace ())
                in
                ( r,
                  Eio_unix.Private.Thread_pool.create
                    ~sleep_q:(Eio_utils.Zzz.create ()) ))
        | _ -> None);
  }

(* An effect the hook answers runs at once, in the scheduler, and its fiber
   resumes on its next turn. *)
let test_effects () =
  run ~effects:systhread @@ fun _ mono _ ->
  let ran = ref false in
  let v =
    Eio_unix.run_in_systhread (fun () ->
        ran := true;
        42)
  in
  Alcotest.(check bool) "ran" true !ran;
  Alcotest.(check int) "its answer" 42 v;
  check_ns "one turn" 1_000L (ns mono);
  match Eio_unix.run_in_systhread (fun () -> failwith "boom") with
  | _ -> Alcotest.fail "a failure in the systhread returned"
  | exception Failure m -> Alcotest.(check string) "its failure" "boom" m

(* Turns, charges and timers in any order: the clock never moves back. *)
let test_monotonic () =
  run @@ fun _ mono charge ->
  let last = ref 0L in
  let see () =
    let now = ns mono in
    if now < !last then Alcotest.failf "the clock moved back to %Ld" now;
    last := now
  in
  Eio.Fiber.all
    [
      (fun () ->
        for _ = 1 to 50 do
          Eio.Time.Mono.sleep_span mono (us 3);
          see ()
        done);
      (fun () ->
        for _ = 1 to 200 do
          charge ();
          see ();
          Eio.Fiber.yield ();
          see ()
        done);
      (fun () ->
        for i = 1 to 20 do
          Eio.Time.Mono.sleep_span mono (us (i mod 4));
          see ()
        done);
    ]

let suite =
  ( "quiet_mock",
    [
      Alcotest.test_case "the clock prints nothing" `Quick test_silent;
      Alcotest.test_case "1 us per turn" `Quick test_turns;
      Alcotest.test_case "the clock moves to the next timer" `Quick test_timer;
      Alcotest.test_case "a run stops at its limit" `Quick test_limit;
      Alcotest.test_case "every fiber waiting is a deadlock" `Quick
        test_deadlock;
      Alcotest.test_case "stop ends the run" `Quick test_stop;
      Alcotest.test_case "idle wakes a fiber, the clock stays" `Quick test_idle;
      Alcotest.test_case "max_turns: busy or too long" `Slow test_max_turns;
      Alcotest.test_case "charge takes a turn" `Slow test_charge;
      Alcotest.test_case "a hooked effect runs at once" `Quick test_effects;
      Alcotest.test_case "the clock never moves back" `Quick test_monotonic;
    ] )
