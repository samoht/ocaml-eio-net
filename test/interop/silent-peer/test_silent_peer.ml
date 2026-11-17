(* A TCP connection of eio-net.mem gone silent must fail as traces/silent.csv
   measured on its kernel: its blocked call within the times measured in
   either case, give or take [latency], and with the same outcomes, that
   call's and those of the read, write and read after it. The model keeps
   one span per kernel, but macOS fails a blocked writer about 0.1 s before
   a reader. Regenerate, on macOS:
   REGEN=1 dune build @test/interop/silent-peer/regen-traces *)

let rows =
  In_channel.with_open_text "traces/silent.csv" In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ kernel; case; seconds; first; r1; w; r2 ] ->
          let reset = function "EPIPE" | "ECONNRESET" -> "reset" | o -> o in
          ( (kernel, case),
            (float_of_string seconds, List.map reset [ first; r1; w; r2 ]) )
      | _ -> Fmt.failwith "silent.csv: bad row %S" line)

let latency = 0.01

(* As measure.ml runs a case, on eio-net.mem: the seconds from the client's
   first write to the end of the call that blocked, and the outcomes. *)
let model os case =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let net = Eio_net_mem.v ~os ~mono:clock ~sw () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c = Eio.Net.connect ~sw net addr in
  let s, _ = Eio.Net.accept ~sw l in
  let byte = Cstruct.create 1 in
  Eio.Flow.copy_string "x" c;
  Eio.Flow.read_exact s byte;
  Eio.Flow.copy_string "x" s;
  Eio.Flow.read_exact c byte;
  Eio.Time.Mono.sleep clock 0.5;
  Eio_net_mem.Private.silence c;
  let ended ~ok f =
    match Eio.Time.Timeout.(run_exn (seconds clock 3600.)) f with
    | () -> ok
    | exception Eio.Time.Timeout -> "waits"
    | exception End_of_file -> "eof"
    | exception
        Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, _, _)), _) ->
        "ETIMEDOUT"
    | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> "reset"
  in
  let buf = Cstruct.create 16 in
  let read () = ended ~ok:"data" (fun () -> ignore (Eio.Flow.single_read c buf))
  and write () = ended ~ok:"ok" (fun () -> Eio.Flow.copy_string "x" c) in
  let t0 = Eio.Time.Mono.now clock in
  let first =
    match case with
    | "write-read" ->
        if write () <> "ok" then Alcotest.fail "the first write failed";
        read ()
    | "fill" ->
        let chunk = String.make 65536 'x' in
        ended ~ok:"ok" (fun () ->
            while true do
              Eio.Flow.copy_string chunk c
            done)
    | _ -> Alcotest.failf "silent.csv: unknown case %s" case
  in
  let seconds =
    Mtime.Span.to_float_ns (Mtime.span t0 (Eio.Time.Mono.now clock)) /. 1e9
  in
  let r1 = read () in
  let w = write () in
  let r2 = read () in
  (seconds, [ first; r1; w; r2 ])

let check os case () =
  let kernel = match os with `Linux -> "linux" | `Macos -> "macos" in
  let runs =
    List.filter_map
      (fun (k, r) -> if k = (kernel, case) then Some r else None)
      rows
  in
  if runs = [] then Alcotest.failf "silent.csv measures no %s %s" kernel case;
  let seconds, outcomes = model os case in
  List.iter
    (fun (_, measured) ->
      Alcotest.(check (list string))
        (Fmt.str "%s %s: the call, then a read, a write and a read" kernel case)
        measured outcomes)
    runs;
  let times =
    List.filter_map
      (fun ((k, _), (s, _)) -> if k = kernel then Some s else None)
      rows
  in
  let lo = List.fold_left Float.min Float.infinity times
  and hi = List.fold_left Float.max 0. times in
  if seconds < lo -. latency || seconds > hi +. latency then
    Alcotest.failf
      "eio-net.mem's %s %s fails after %g s, but the kernel's took %g to %g s"
      kernel case seconds lo hi

let suite =
  ( "silent_peer",
    List.concat_map
      (fun (os, name) ->
        List.map
          (fun case ->
            Alcotest.test_case
              (Fmt.str "%s, %s" name case)
              `Quick (check os case))
          [ "write-read"; "fill" ])
      [ (`Linux, "Linux"); (`Macos, "macOS") ] )
