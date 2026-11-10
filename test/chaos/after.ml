(* The tests of the design: a framed reader, a client that retries, and a
   UDP request-response client. *)

let msgs = [ "hello"; ""; "a longer message of 32 bytes...." ]

let rec is_prefix p l =
  match (p, l) with
  | [], _ -> true
  | x :: p, y :: l -> x = y && is_prefix p l
  | _ -> false

let test_d ?budget ?replay ~read () =
  Eio_net_chaos.check ?budget ?replay @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
         let flow, _ = Eio.Net.accept ~sw l in
         List.iter (App.Frame.write flow) msgs;
         Eio.Flow.shutdown flow `Send
       with Eio.Io _ -> ());
      `Stop_daemon);
  let rec loop flow acc =
    match read ~clock:env#clock flow with
    | Some m -> loop flow (m :: acc)
    | None | (exception (Eio.Io _ | App.Frame.Truncated | Eio.Time.Timeout)) ->
        List.rev acc
  in
  let got =
    try loop (Eio.Net.connect ~sw env#net addr) [] with Eio.Io _ -> []
  in
  if not (is_prefix got msgs) then
    Fmt.failwith "got %a" Fmt.Dump.(list string) got

(* The error contract needs the exact cut, so it pins one scenario. *)
let test_cut ~read () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cut (op : Eio_net_chaos.Op.t) =
    (* The server closes inside the third frame's header, which starts at
       byte 13: its write there moves 2 bytes, and the next ends the stream. *)
    match (op.kind, op.side, op.offset) with
    | `Write, `Server, 13 -> Some (Eio_net_chaos.Fault.Short 2)
    | `Write, `Server, 15 -> Some Eio_net_chaos.Fault.Eof
    | _ -> None
  in
  let net = Eio_net_chaos.wrap ~clock:env#clock cut (Eio_net_mem.v ~sw ()) in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let flow, _ = Eio.Net.accept ~sw l in
      (try List.iter (App.Frame.write flow) msgs with Eio.Io _ -> ());
      `Stop_daemon);
  let flow = Eio.Net.connect ~sw net addr in
  assert (read ~clock:env#clock flow = Some "hello");
  assert (read ~clock:env#clock flow = Some "");
  match read ~clock:env#clock flow with
  | exception App.Frame.Truncated -> ()
  | _ -> failwith "an end mid-frame must raise Truncated"

let test_e ~fetch () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          App.Frame.write flow "pong"));
  let t0 = Eio.Time.now env#clock in
  let r = fetch ~clock:env#clock env#net addr in
  if Eio.Time.now env#clock -. t0 > 10. then failwith "slower than 10 seconds";
  match (r, Eio_net_chaos.outage env) with
  | Ok "pong", false | Error `Unreachable, true -> ()
  | Ok v, _ -> Fmt.failwith "got %S" v
  | Error `Unreachable, false -> failwith "gave up"

let test_f ~call () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr port = `Udp (Eio.Net.Ipaddr.V4.loopback, port) in
  let server = Eio.Net.datagram_socket ~sw env#net (addr 9000) in
  Eio.Fiber.fork_daemon ~sw (fun () -> App.Rpc.serve server);
  let client = Eio.Net.datagram_socket ~sw env#net (addr 9001) in
  for i = 1 to 5 do
    let id = string_of_int i and body = Fmt.str "msg%d" i in
    match
      ( call ~clock:env#clock client (addr 9000) ~id body,
        Eio_net_chaos.outage env )
    with
    | Ok r, false when r = String.uppercase_ascii body -> ()
    | Error `Timeout, true -> ()
    | Ok r, _ -> Fmt.failwith "request %d got %S" i r
    | Error `Timeout, false -> Fmt.failwith "request %d timed out" i
  done
