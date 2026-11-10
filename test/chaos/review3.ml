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
