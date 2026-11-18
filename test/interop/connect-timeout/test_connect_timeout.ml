(* A model of a connect whose SYNs are all dropped must fail with ETIMEDOUT
   within the times traces/connect.csv measured on its kernel, give or take
   [latency], the time a woken program takes to see the error. Regenerate, on
   macOS: REGEN=1 dune build @test/interop/connect-timeout/regen-traces *)

module Chaos = Eio_net_chaos

let rows =
  In_channel.with_open_text "traces/connect.csv" In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ kernel; client; seconds; outcome ] ->
          (kernel, client, float_of_string seconds, outcome)
      | _ -> Fmt.failwith "connect.csv: bad row %S" line)

let latency = 0.01

let check kernel ~model seconds =
  let runs = List.filter (fun (k, _, _, _) -> k = kernel) rows in
  if runs = [] then Alcotest.failf "connect.csv measures no %s connect" kernel;
  List.iter
    (fun (_, client, _, outcome) ->
      if outcome <> "ETIMEDOUT" then
        Alcotest.failf "a %s connect through %s ended with %s" kernel client
          outcome)
    runs;
  let times = List.map (fun (_, _, s, _) -> s) runs in
  let lo = List.fold_left Float.min Float.infinity times
  and hi = List.fold_left Float.max 0. times in
  if seconds < lo -. latency || seconds > hi +. latency then
    Alcotest.failf "%s fails after %g s, but %s connects took %g to %g s" model
      seconds kernel lo hi

let timed_out f =
  match f () with
  | _ -> Alcotest.fail "the connect went through"
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, _, _)), _)
    ->
      ()

let chaos os () =
  let seconds =
    Eio_mock.Backend.run_full @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let timeout (op : Chaos.Op.t) =
      if op.kind = `Connect then Some Chaos.Fault.Timeout else None
    in
    let net =
      Chaos.wrap ~os ~clock:env#clock timeout (Eio_net_mem.v ~os ~sw ())
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
    timed_out (fun () -> Eio.Net.connect ~sw net addr);
    Eio.Time.now env#clock
  in
  let kernel = match os with `Linux -> "linux" | `Macos -> "macos" in
  check kernel ~model:"a chaos Timeout" seconds

(* macOS drops the SYNs to an address of 127.0.0.0/8 other than 127.0.0.1,
   as the trace's macOS connects do. *)
let mem_macos () =
  let seconds =
    Eio_mock.Backend.run_full @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = env#mono_clock in
    let net = Eio_net_mem.v ~os:`Macos ~clock ~sw () in
    let ip = Eio.Net.Ipaddr.of_raw "\127\000\000\002" in
    let t0 = Eio.Time.Mono.now clock in
    timed_out (fun () -> Eio.Net.connect ~sw net (`Tcp (ip, 9)));
    Mtime.Span.to_float_ns (Mtime.span t0 (Eio.Time.Mono.now clock)) /. 1e9
  in
  check "macos" ~model:"eio-net.mem's connect to 127.0.0.2" seconds

let suite =
  ( "connect_timeout",
    [
      Alcotest.test_case "chaos, Linux" `Quick (chaos `Linux);
      Alcotest.test_case "chaos, macOS" `Quick (chaos `Macos);
      Alcotest.test_case "mem, macOS" `Quick mem_macos;
    ] )
