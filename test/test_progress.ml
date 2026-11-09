(* Eio_net.Progress against curl's low-speed check: fewer than [limit] bytes a
   second over a window ends the transfer. Eio's mock backend advances its
   clock only when every fiber waits, so each time below is exact; each case
   runs at two windows an order of magnitude apart. *)

module Progress = Eio_net.Progress

let span = Alcotest.testable Mtime.Span.pp Mtime.Span.equal
let seconds n = Mtime.Span.(n * s)

let quarter w =
  Mtime.Span.of_uint64_ns (Int64.div (Mtime.Span.to_uint64_ns w) 4L)

(* [f ~mono p], with [p] at 100 bytes a second over [window], is expected to
   raise Too_slow; the time it took is returned. *)
let too_slow ~window f =
  Eio_mock.Backend.run_full @@ fun env ->
  let mono = env#mono_clock in
  let p = Progress.v ~mono ~limit:100 ~window in
  let t0 = Eio.Time.Mono.now mono in
  match f ~mono p with
  | () -> Alcotest.fail "a transfer under the limit went on"
  | exception Progress.Too_slow e ->
      Alcotest.(check int) "the limit it names" 100 e.limit;
      Alcotest.(check span) "the window it names" window e.window;
      Mtime.span t0 (Eio.Time.Mono.now mono)

(* A peer that never answers: the wait is cut at the window's end. *)
let test_silent n () =
  let window = seconds n in
  let took =
    too_slow ~window @@ fun ~mono:_ p ->
    ignore (Progress.move p (fun () -> Eio.Fiber.await_cancel ()) : int)
  in
  Alcotest.(check span) "it ends at one window" window took

(* A peer that sends one byte a quarter window, 4 a window where 100 a
   second are asked: a drip costs what silence costs. *)
let test_drip n () =
  let window = seconds n in
  let took =
    too_slow ~window @@ fun ~mono p ->
    let r =
      Progress.reader p
        (Bytesrw.Bytes.Reader.make (fun () ->
             Eio.Time.Mono.sleep_span mono (quarter window);
             Bytesrw.Bytes.Slice.of_string "x"))
    in
    while true do
      ignore (Bytesrw.Bytes.Reader.read r)
    done
  in
  Alcotest.(check span) "it ends at one window" window took

(* A peer that sends a window's worth at once and then stops: the first
   window passes, and the second ends it. *)
let test_burst_then_silence n () =
  let window = seconds n in
  let took =
    too_slow ~window @@ fun ~mono:_ p ->
    ignore (Progress.move p (fun () -> 100 * n) : int);
    ignore (Progress.move p (fun () -> Eio.Fiber.await_cancel ()) : int)
  in
  Alcotest.(check span) "it ends at two windows" Mtime.Span.(2 * window) took

(* A peer that takes a move each quarter window, 400 bytes a second where
   100 are asked: the transfer goes on for four windows and ends whole. *)
let test_steady n () =
  let window = seconds n in
  Eio_mock.Backend.run_full @@ fun env ->
  let mono = env#mono_clock in
  let p = Progress.v ~mono ~limit:100 ~window in
  let each = 100 * n in
  let moved = ref 0 in
  for _ = 1 to 16 do
    moved :=
      !moved
      + Progress.move p (fun () ->
          Eio.Time.Mono.sleep_span mono (quarter window);
          each)
  done;
  Alcotest.(check int) "every write went through" (16 * each) !moved

(* The time before the first move counts in no window: a transfer made a
   window before it starts moving goes on, at the limit. *)
let test_first_move n () =
  let window = seconds n in
  Eio_mock.Backend.run_full @@ fun env ->
  let mono = env#mono_clock in
  let p = Progress.v ~mono ~limit:100 ~window in
  Eio.Time.Mono.sleep_span mono Mtime.Span.(2 * window);
  let moved =
    Progress.move p (fun () ->
        Eio.Time.Mono.sleep_span mono (quarter window);
        100 * n)
  in
  Alcotest.(check int) "the first move went through" (100 * n) moved

(* Two fibers share one transfer: one waits for bytes that never come, the
   other moves ten times the limit every 0.6 windows. Every window moves
   enough, so neither is cut: the waiting fiber's window, which ended and
   was judged by the other, is not judged again. *)
let test_shared n () =
  let window = seconds n in
  Eio_mock.Backend.run_full @@ fun env ->
  let mono = env#mono_clock in
  let p = Progress.v ~mono ~limit:100 ~window in
  let step =
    Mtime.Span.of_uint64_ns
      (Int64.div (Mtime.Span.to_uint64_ns window) 10L |> Int64.mul 6L)
  in
  let moves = ref 0 in
  Eio.Fiber.first
    (fun () ->
      while true do
        ignore (Progress.move p (fun () -> Eio.Fiber.await_cancel ()) : int)
      done)
    (fun () ->
      while !moves < 20 do
        let (_ : int) =
          Progress.move p (fun () ->
              Eio.Time.Mono.sleep_span mono step;
              1000 * n)
        in
        incr moves
      done);
  Alcotest.(check int) "every move went through" 20 !moves

(* A move that outlasts a window another fiber keeps above the limit runs
   once: a write that sent part of its bytes is not cut and sent again. *)
let test_move_runs_once n () =
  let window = seconds n in
  Eio_mock.Backend.run_full @@ fun env ->
  let mono = env#mono_clock in
  let p = Progress.v ~mono ~limit:100 ~window in
  let step =
    Mtime.Span.of_uint64_ns
      (Int64.div (Mtime.Span.to_uint64_ns window) 10L |> Int64.mul 6L)
  in
  let sent = Buffer.create 4 and finished = ref false and moves = ref 0 in
  Eio.Fiber.both
    (fun () ->
      let moved =
        Progress.move p (fun () ->
            Buffer.add_string sent "a";
            Eio.Time.Mono.sleep_span mono Mtime.Span.(3 * window);
            Buffer.add_string sent "b";
            2)
      in
      Alcotest.(check int) "the slow move's count" 2 moved;
      finished := true)
    (fun () ->
      while (not !finished) && !moves < 20 do
        ignore
          (Progress.move p (fun () ->
               Eio.Time.Mono.sleep_span mono step;
               1000 * n)
            : int);
        incr moves
      done);
  Alcotest.(check string) "the slow move ran once" "ab" (Buffer.contents sent)

let test_words () =
  Alcotest.(check string)
    "curl's words"
    "Operation too slow. Less than 1 bytes/sec transferred the last 60 seconds"
    (Printexc.to_string (Progress.Too_slow { limit = 1; window = seconds 60 }))

let cases name f =
  List.map
    (fun n ->
      Alcotest.test_case (Fmt.str "%s (window %ds)" name n) `Quick (f n))
    [ 1; 10 ]

let suite =
  ( "progress",
    cases "a silent peer ends at the window" test_silent
    @ cases "a dripping peer ends at the window" test_drip
    @ cases "a burst then silence ends at the second window"
        test_burst_then_silence
    @ cases "a peer at the limit goes on" test_steady
    @ cases "the time before the first move counts in no window" test_first_move
    @ cases "two fibers sharing a transfer judge each window once" test_shared
    @ cases "a move that outlasts a window runs once" test_move_runs_once
    @ [ Alcotest.test_case "Too_slow in curl's words" `Quick test_words ] )
