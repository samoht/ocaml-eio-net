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

(* A server that listens without ~reuse_addr cannot restart while its
   closed connections hold the port. *)
let no_reuse () = Eio_net_chaos.run (download ~reuse_addr:false)

let ended_field () =
  let m = report no_reuse in
  if Expect.field "ended" m = "" then
    Alcotest.failf "the report names no end of the faulty run: %s" m

let suite =
  ( "restart",
    [
      Alcotest.test_case "an escaped Eio.Io passes when the restart does" `Quick
        escaped_io;
      Alcotest.test_case "a listen without ~reuse_addr cannot restart" `Quick
        (starts "Eio_net_chaos.run: the restart failed" no_reuse);
      Alcotest.test_case "the report says how the faulty run ended" `Quick
        ended_field;
    ] )
