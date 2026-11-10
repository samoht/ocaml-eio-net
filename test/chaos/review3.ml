(* Review 3: correct programs that real stacks run, and the bugs they hide. *)

let tcp port = `Tcp (Eio.Net.Ipaddr.V4.loopback, port)
let strict_line = App.strict_line

(* Retries [f] three times, each bounded by a second. *)
let attempts (env : Eio_net_chaos.env) f =
  let rec go n =
    match Eio.Time.with_timeout_exn env#clock 1.0 f with
    | x -> Some x
    | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
        if n < 3 then go (n + 1) else None
  in
  go 1

let expect (env : Eio_net_chaos.env) pp want got =
  match (got, Eio_net_chaos.outage env) with
  | Some x, false when x = want -> ()
  | None, true -> ()
  | Some x, _ -> Fmt.failwith "got %a" pp x
  | None, false -> failwith "gave up"

(* An SSH-style banner exchange: both ends send their identification line on
   connect, then read the peer's (RFC 4253, 4.2). *)
let banner env =
  let id_s = "SSH-2.0-Server_1.0\r\n" and id_c = "SSH-2.0-Client_1.0\r\n" in
  let read_id flow =
    strict_line (Eio.Buf_read.of_flow flow ~max_size:255) ^ "\n"
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7022) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string id_s flow;
          let peer =
            Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> read_id flow)
          in
          if peer <> id_c then failwith "bad client id"));
  attempts env (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net (tcp 7022) in
      Eio.Flow.copy_string id_c flow;
      read_id flow)
  |> expect env Fmt.Dump.string id_s

(* A reconnecting client. With [`Connection_failure] it retries only the
   errors Eio classifies as connection failures, so a kernel ETIMEDOUT or
   EHOSTUNREACH, which eio_posix raises unclassified, escapes. *)
let connfail ?(bug = `None) env =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7050) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "hi\n" flow));
  let rec attempt n =
    if n > 3 then None
    else
      Eio.Switch.run @@ fun sw ->
      match Eio.Net.connect ~sw env#net (tcp 7050) with
      | exception Eio.Io (Eio.Net.E (Connection_failure _), _)
        when bug = `Connection_failure ->
          Eio.Time.sleep env#clock 1.0;
          attempt (n + 1)
      | exception Eio.Io _ when bug = `None ->
          Eio.Time.sleep env#clock 1.0;
          attempt (n + 1)
      | flow -> (
          match
            Eio.Time.with_timeout_exn env#clock 1.0 (fun () ->
                strict_line (Eio.Buf_read.of_flow flow ~max_size:16))
          with
          | s -> Some s
          | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
              attempt (n + 1))
  in
  expect env Fmt.Dump.string "hi" (attempt 1)

(* A server whose accept loop, under [Fault.Emfile], retries the accept at
   once: with no descriptor left it spins. *)
let busy_accept () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let emfile (op : Eio_net_chaos.Op.t) =
    if op.kind = `Accept && op.offset = 1 then Some Eio_net_chaos.Fault.Emfile
    else None
  in
  let net = Eio_net_chaos.wrap ~clock:env#clock emfile (Eio_net_mem.v ~sw ()) in
  let l = Eio.Net.listen ~sw ~backlog:5 net (tcp 7060) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match Eio.Net.accept ~sw l with
        | flow, _ ->
            Eio.Fiber.fork ~sw (fun () ->
                Eio.Flow.copy_string "hi\n" flow;
                Eio.Time.sleep env#clock 10.;
                Eio.Flow.close flow);
            loop ()
        | exception Eio.Io _ -> loop ()
      in
      loop ());
  let client () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw net (tcp 7060) in
    Eio.Time.with_timeout_exn env#clock 30. (fun () ->
        strict_line (Eio.Buf_read.of_flow flow ~max_size:16))
  in
  ignore (Eio.Fiber.List.map (fun _ -> client ()) [ 1; 2 ])

(* [Eio_net_accept.run] under [Fault.Emfile]: it sheds an idle connection,
   whose close frees a descriptor, and takes the queued one. *)
let accept_loop () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let emfile (op : Eio_net_chaos.Op.t) =
    if op.kind = `Accept && op.offset = 1 then Some Eio_net_chaos.Fault.Emfile
    else None
  in
  let net = Eio_net_chaos.wrap ~clock:env#clock emfile (Eio_net_mem.v ~sw ()) in
  let l = Eio.Net.listen ~sw ~backlog:5 net (tcp 7061) in
  let stop, set_stop = Eio.Promise.create () in
  let shed = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_net_accept.run ~stop ~classify:Eio_net_unix.accept_failure
        ~max_connections:4 l (fun _ flow _ ->
          match strict_line (Eio.Buf_read.of_flow flow ~max_size:16) with
          | line -> Eio.Flow.copy_string (line ^ "\n") flow
          | exception End_of_file -> shed := true))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let _idle = Eio.Net.connect ~sw net (tcp 7061) in
      let flow = Eio.Net.connect ~sw net (tcp 7061) in
      Eio.Flow.copy_string "hello\n" flow;
      let got =
        Eio.Time.with_timeout_exn env#clock 5. (fun () ->
            strict_line (Eio.Buf_read.of_flow flow ~max_size:16))
      in
      if got <> "hello" then Fmt.failwith "got %S" got;
      if not !shed then failwith "no connection was shed";
      Eio.Promise.resolve set_stop ())

(* A client that writes its whole batch, 18 bytes, before it reads a reply. *)
let pipelined env =
  let keys = [ "a"; "b"; "c" ] in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7001) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:256 in
          let rec loop () =
            match
              Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> strict_line r)
            with
            | cmd when String.starts_with ~prefix:"GET " cmd ->
                let k = String.sub cmd 4 (String.length cmd - 4) in
                Eio.Flow.copy_string ("VAL " ^ k ^ "\n") flow;
                loop ()
            | _ -> failwith "bad command"
            | exception End_of_file -> ()
          in
          loop ()));
  attempts env (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net (tcp 7001) in
      let r = Eio.Buf_read.of_flow flow ~max_size:256 in
      List.iter (fun k -> Eio.Flow.copy_string ("GET " ^ k ^ "\n") flow) keys;
      List.map (fun _ -> strict_line r) keys)
  |> expect env Fmt.Dump.(list string) [ "VAL a"; "VAL b"; "VAL c" ]
