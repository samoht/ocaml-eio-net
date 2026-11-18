(* The rule of a run with a fault: an escaped Eio.Io or End_of_file is
   indeterminate, and the same program, run again on the healed network,
   decides. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

(* A server that sends "hello" and closes, and a client that lets every
   network error escape. A timeout ends a silent connection. *)
let download ?(reuse_addr = true) (env : Eio_net_chaos.env) =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~reuse_addr ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "hello" flow));
  match
    Eio.Time.with_timeout env#clock 5.0 @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    Ok (Eio.Buf_read.(parse_exn take_all) ~max_size:100 flow)
  with
  | Ok got ->
      if not (String.starts_with ~prefix:got "hello") then
        Fmt.failwith "got %S" got
  | Error `Timeout -> ()

(* The same download, through with_timeout_exn: a silent connection lets
   Eio.Time.Timeout escape. *)
let download_exn (env : Eio_net_chaos.env) =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "hello" flow));
  let got =
    Eio.Time.with_timeout_exn env#clock 5.0 @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    Eio.Buf_read.(parse_exn take_all) ~max_size:100 flow
  in
  if not (String.starts_with ~prefix:got "hello") then Fmt.failwith "got %S" got

let report f =
  match f () with
  | () -> Alcotest.fail "the check passed"
  | exception Failure m -> m

let starts prefix f () =
  let m = report f in
  if not (String.starts_with ~prefix m) then
    Alcotest.failf "the check failed with %S" m

(* A connect that is refused, or a read that is reset, raises Eio.Io out of
   the program: its restart works, so the check passes. *)
let escaped_io () = Eio_net_chaos.run download

(* An escaped Eio.Time.Timeout is a failure the program reports: the restart
   decides too. *)
let escaped_timeout () = Eio_net_chaos.run download_exn

(* A server that listens without ~reuse_addr cannot restart while its
   closed connections hold the port. *)
let no_reuse () = Eio_net_chaos.run (download ~reuse_addr:false)

let ended_field () =
  let m = report no_reuse in
  if Expect.field "ended" m = "" then
    Alcotest.failf "the report names no end of the faulty run: %s" m

(* [setup] runs before each run, on its network: a connection it closes from
   the server's end holds the port, so a listen without ~reuse_addr fails
   from the first run. *)
let held_port (env : Eio_net_chaos.env) =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 env#net addr in
  let c = Eio.Net.connect ~sw env#net addr in
  let s, _ = Eio.Net.accept ~sw l in
  Eio.Flow.close s;
  Eio.Flow.close c

let setup_holds () =
  Eio_net_chaos.run ~setup:held_port (download ~reuse_addr:false)

let setup_reused () = Eio_net_chaos.run ~setup:held_port download

(* [inspect] runs after each run with a fault, before the restart. *)
let inspected () =
  let n = ref 0 in
  Eio_net_chaos.run ~inspect:(fun _ -> incr n) download;
  if !n = 0 then Alcotest.fail "inspect never ran"

let inspect_fails () =
  Eio_net_chaos.run ~inspect:(fun _ -> failwith "no") download

(* [is_outage] holds in the outage run alone: not in the first run, the
   other runs with a fault, the restarts, inspect, or outside a check. *)
let outage_alone () =
  let seen = ref [] in
  let probe where (env : Eio_net_chaos.env) =
    seen := (where, Eio_net_chaos.is_outage env) :: !seen
  in
  Eio_net_chaos.run ~inspect:(probe "inspect") (fun env ->
      probe (if Eio_chaos.Private.restarted env then "restart" else "run") env;
      download env);
  let outage where = List.mem (where, true) !seen in
  if not (outage "run") then Alcotest.fail "no run was the outage";
  if outage "restart" then Alcotest.fail "a restart was the outage";
  if outage "inspect" then Alcotest.fail "inspect was the outage";
  match
    Eio_chaos.Private.within
      [ Eio_net_chaos.network () ]
      Eio_net_chaos.is_outage
  with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "an env outside a check is the outage"
  | Error e -> Alcotest.fail e

let suite =
  ( "restart",
    [
      Alcotest.test_case "an escaped Eio.Io passes when the restart does" `Quick
        escaped_io;
      Alcotest.test_case "an escaped Timeout passes when the restart does"
        `Quick escaped_timeout;
      Alcotest.test_case "a listen without ~reuse_addr cannot restart" `Quick
        (starts "Eio_net_chaos.run: the restart failed" no_reuse);
      Alcotest.test_case "the report says how the faulty run ended" `Quick
        ended_field;
      Alcotest.test_case "setup runs before the first run" `Quick
        (starts "Eio_net_chaos.run: the run with no fault failed" setup_holds);
      Alcotest.test_case "setup's network is the program's" `Quick setup_reused;
      Alcotest.test_case "inspect runs after a fault" `Quick inspected;
      Alcotest.test_case "inspect that raises fails the check" `Quick
        (starts "Eio_net_chaos.run: inspect rejected what the fault left"
           inspect_fails);
      Alcotest.test_case "is_outage holds in the outage run alone" `Quick
        outage_alone;
    ] )
