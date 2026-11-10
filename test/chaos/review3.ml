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

(* Both ends write a large greeting before they read the other's: 30 KB from
   the server, 20 KB from the client. A Linux connection holds that much with
   its buffers at their minimum. *)
let greetings env =
  let server_hello = String.make 30_000 's'
  and client_hello = String.make 20_000 'c' in
  let read_exactly flow n =
    let buf = Cstruct.create n in
    Eio.Flow.read_exact flow buf;
    Cstruct.to_string buf
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net (tcp 7070) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string server_hello flow;
          let got =
            Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
                read_exactly flow 20_000)
          in
          if got <> client_hello then failwith "bad client greeting"));
  attempts env (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw env#net (tcp 7070) in
      Eio.Flow.copy_string client_hello flow;
      read_exactly flow 30_000 = server_hello)
  |> expect env Fmt.bool true

(* A ping-pong over a Unix-domain stream. A Unix-domain socket is reliable
   and ordered (unix(7)): a connect may be refused, and a peer may close or
   crash, but no IP error happens, and a small write arrives whole, so the
   client reads each reply with one read. *)
let unix_stream env =
  let path = `Unix "/tmp/pingpong.sock" in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net path in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let r = Eio.Buf_read.of_flow flow ~max_size:64 in
          match
            Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> strict_line r)
          with
          | "ping" -> Eio.Flow.copy_string "pong\n" flow
          | _ -> ()));
  let once () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net path in
    Eio.Flow.copy_string "ping\n" flow;
    let buf = Cstruct.create 64 in
    Cstruct.to_string ~len:(Eio.Flow.single_read flow buf) buf
  in
  let rec attempt n =
    match Eio.Time.with_timeout_exn env#clock 1.0 once with
    | reply -> Some reply
    | exception
        ( Eio.Io
            (Eio.Net.E (Connection_failure (Refused _) | Connection_reset _), _)
        | End_of_file | Eio.Time.Timeout ) ->
        if n < 3 then attempt (n + 1) else None
  in
  expect env Fmt.Dump.string "pong\n" (attempt 1)

(* A ping-pong over Unix-domain datagrams, with no retry: no datagram is
   lost, duplicated, reordered or garbled. *)
let unix_datagram env =
  Eio.Switch.run @@ fun sw ->
  let srv = Eio.Net.datagram_socket ~sw env#net (`Unix "/tmp/srv.sock") in
  let cli = Eio.Net.datagram_socket ~sw env#net (`Unix "/tmp/cli.sock") in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Cstruct.create 64 in
      let rec loop () =
        let src, n = Eio.Net.recv srv buf in
        let m = Cstruct.to_string ~len:n buf in
        Eio.Net.send srv ~dst:src
          [ Cstruct.of_string ("pong" ^ String.sub m 4 1) ];
        loop ()
      in
      loop ());
  let buf = Cstruct.create 64 in
  List.iter
    (fun i ->
      Eio.Net.send cli ~dst:(`Unix "/tmp/srv.sock")
        [ Cstruct.of_string ("ping" ^ i) ];
      let _, n =
        Eio.Time.with_timeout_exn env#clock 5.0 (fun () -> Eio.Net.recv cli buf)
      in
      let got = Cstruct.to_string ~len:n buf in
      if got <> "pong" ^ i then Fmt.failwith "got %S" got)
    [ "1"; "2"; "3" ]

(* A leader lease over UDP: a server grants a lease of 1 s; client A renews
   it until t = 2 s, then stops; B polls and must take over. The leaderships
   the clients believe in must never overlap. With [`Receipt] a client counts
   its lease from when the grant arrived, not from when it asked. *)
let lease ?(bug = `None) env =
  let server_addr = `Udp (Eio.Net.Ipaddr.V4.loopback, 7100) in
  let server ~sw =
    let sock = Eio.Net.datagram_socket ~sw env#net server_addr in
    let holder = ref None and expiry = ref 0. in
    let buf = Cstruct.create 64 in
    let rec loop () =
      let src, n = Eio.Net.recv sock buf in
      (match String.split_on_char ' ' (Cstruct.to_string buf ~len:n) with
      | [ "ACQ"; id; seq ]
        when (id = "A" || id = "B") && int_of_string_opt seq <> None ->
          let now = Eio.Time.now env#clock in
          let free =
            match !holder with
            | None -> true
            | Some h -> h = id || now >= !expiry
          in
          let reply =
            if free then begin
              holder := Some id;
              expiry := now +. 1.0;
              "OK " ^ seq
            end
            else "NO " ^ seq
          in
          Eio.Net.send sock ~dst:src [ Cstruct.of_string reply ]
      | _ -> ());
      loop ()
    in
    loop ()
  in
  let client ~sw ~id ~port ~start ~stop beliefs =
    let sock =
      Eio.Net.datagram_socket ~sw env#net
        (`Udp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let clock = env#clock in
    Eio.Time.sleep clock start;
    let until = ref neg_infinity and since = ref 0. and seq = ref 0 in
    let buf = Cstruct.create 64 in
    let rec await s =
      let _, n = Eio.Net.recv sock buf in
      match String.split_on_char ' ' (Cstruct.to_string buf ~len:n) with
      | [ "OK"; k ] when int_of_string_opt k = Some s -> `Ok
      | [ "NO"; k ] when int_of_string_opt k = Some s -> `No
      | _ -> await s
    in
    let rec loop () =
      let asked = Eio.Time.now clock in
      if asked < stop then begin
        incr seq;
        let s = !seq in
        Eio.Net.send sock ~dst:server_addr
          [ Cstruct.of_string (Fmt.str "ACQ %s %d" id s) ];
        let r =
          match Eio.Time.with_timeout_exn clock 0.2 (fun () -> await s) with
          | r -> r
          | exception Eio.Time.Timeout -> `Lost
        in
        let now = Eio.Time.now clock in
        (match r with
        | `Ok ->
            let base = if bug = `Receipt then now else asked in
            if !until < now then since := now;
            until := base +. 1.0
        | `No | `Lost -> ());
        let next = if !until > now then 0.3 else 0.02 in
        Eio.Time.sleep clock
          (Float.max 0. (next -. (Eio.Time.now clock -. now)));
        loop ()
      end
    in
    loop ();
    if !until > !since then beliefs := (id, !since, !until) :: !beliefs
  in
  let beliefs = ref [] in
  Eio.Switch.run (fun sw ->
      Eio.Fiber.fork_daemon ~sw (fun () -> server ~sw);
      Eio.Fiber.both
        (fun () -> client ~sw ~id:"A" ~port:7101 ~start:0. ~stop:2.0 beliefs)
        (fun () -> client ~sw ~id:"B" ~port:7102 ~start:0.1 ~stop:5.0 beliefs));
  let of_id i = List.filter (fun (j, _, _) -> i = j) !beliefs in
  List.iter
    (fun (_, sa, ea) ->
      List.iter
        (fun (_, sb, eb) ->
          if sa < eb && sb < ea then
            Fmt.failwith "overlap: A [%.3f,%.3f] B [%.3f,%.3f]" sa ea sb eb)
        (of_id "B"))
    (of_id "A");
  if of_id "B" = [] && not (Eio_net_chaos.outage env) then
    failwith "B never led"

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
