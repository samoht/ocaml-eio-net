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

(* [n] lines of lengths from 1 to 11 bytes, echoed back. *)
let echo n env =
  let lines = List.init n (fun i -> String.make (1 + (i * 7 mod 11)) 'x') in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7040) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:256 in
          try
            while true do
              let line =
                Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
                    strict_line r)
              in
              Eio.Flow.copy_string (line ^ "\n") flow
            done
          with End_of_file -> ()));
  attempts env (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net (tcp 7040) in
      let r = Eio.Buf_read.of_flow flow ~max_size:256 in
      snd
        (Eio.Fiber.pair
           (fun () ->
             List.iter (fun l -> Eio.Flow.copy_string (l ^ "\n") flow) lines)
           (fun () -> List.map (fun _ -> strict_line r) lines)))
  |> expect env (fun ppf _ -> Fmt.string ppf "a wrong echo") lines

(* A server that sheds load: at most two requests served at once, the others
   answered BUSY; five clients retry on BUSY, each attempt bounded by 3 s.
   With [`Leak] the count of busy handlers is not decremented when a handler
   fails. *)
let shed ?(bug = `None) env =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:16 env#net (tcp 7009) in
  let active = ref 0 in
  let handle flow =
    let r = Eio.Buf_read.of_flow flow ~max_size:64 in
    let req =
      Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> strict_line r)
    in
    Eio.Time.sleep env#clock 1.0;
    let id = String.sub req 5 (String.length req - 5) in
    Eio.Flow.copy_string ("OK " ^ id ^ "\n") flow
  in
  let serve flow =
    incr active;
    if bug = `Leak then begin
      handle flow;
      decr active
    end
    else Fun.protect ~finally:(fun () -> decr active) (fun () -> handle flow)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        Eio.Net.accept_fork ~sw l ~on_error:ignore (fun flow _ ->
            if !active >= 2 then Eio.Flow.copy_string "BUSY\n" flow
            else serve flow);
        loop ()
      in
      loop ());
  let client i =
    let t0 = Eio.Time.now env#clock in
    let rec attempt () =
      if Eio.Time.now env#clock -. t0 > 20. then false
      else
        match
          Eio.Time.with_timeout_exn env#clock 3.0 (fun () ->
              Eio.Switch.run @@ fun sw ->
              let flow = Eio.Net.connect ~sw env#net (tcp 7009) in
              Eio.Flow.copy_string (Fmt.str "WORK %d\n" i) flow;
              strict_line (Eio.Buf_read.of_flow flow ~max_size:64))
        with
        | "BUSY" ->
            Eio.Time.sleep env#clock 0.5;
            attempt ()
        | s when s = Fmt.str "OK %d" i -> true
        | s -> Fmt.failwith "client %d got %S" i s
        | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) ->
            Eio.Time.sleep env#clock 0.2;
            attempt ()
    in
    attempt ()
  in
  let results = Eio.Fiber.List.map client [ 1; 2; 3; 4; 5 ] in
  if Eio_net_chaos.outage env then
    begin if List.exists Fun.id results then failwith "served during the outage"
    end
  else if not (List.for_all Fun.id results) then failwith "a client gave up"

(* Length-prefixed requests and replies on one connection. With [`Lenient]
   the client takes a truncated last frame for a whole one. *)
let rpc ?(bug = `None) env =
  let frame s =
    let b = Bytes.create (4 + String.length s) in
    Bytes.set_int32_be b 0 (Int32.of_int (String.length s));
    Bytes.blit_string s 0 b 4 (String.length s);
    Bytes.to_string b
  in
  let read_frame ~lenient r =
    let n = Int32.to_int (String.get_int32_be (Eio.Buf_read.take 4 r) 0) in
    if n < 0 || n > 1024 then failwith "bad frame";
    if lenient then
      try Eio.Buf_read.take n r with End_of_file -> Eio.Buf_read.take_all r
    else Eio.Buf_read.take n r
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7000) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:2048 in
          let rec loop () =
            match
              Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
                  read_frame ~lenient:false r)
            with
            | req ->
                Eio.Flow.copy_string (frame (String.uppercase_ascii req)) flow;
                loop ()
            | exception End_of_file -> ()
          in
          loop ()));
  attempts env (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net (tcp 7000) in
      let r = Eio.Buf_read.of_flow flow ~max_size:2048 in
      List.map
        (fun req ->
          Eio.Flow.copy_string (frame req) flow;
          read_frame ~lenient:(bug = `Lenient) r)
        [ "a"; "bb"; "ccc" ])
  |> expect env Fmt.Dump.(list string) [ "A"; "BB"; "CCC" ]

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
