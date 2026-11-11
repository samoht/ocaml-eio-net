(* Review 4: what Linux promises on AF_UNIX, and a client that reconnects at
   once. *)

let unix_lines (env : Eio_net_chaos.env) =
  let addr = `Unix "/run/lines.sock" in
  Eio.Switch.run @@ fun sw ->
  let written = ref 0 in
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:4 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "a\n" flow;
          Eio.Flow.copy_string "b\n" flow;
          written := 2;
          let r = Eio.Buf_read.of_flow flow ~max_size:16 in
          ignore
            (Eio.Time.with_timeout_exn env#clock 10. (fun () ->
                 App.strict_line r))));
  match
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    let r = Eio.Buf_read.of_flow flow ~max_size:16 in
    let got = ref 0 in
    match
      for _ = 1 to 2 do
        ignore
          (Eio.Time.with_timeout_exn env#clock 5. (fun () -> App.strict_line r));
        incr got
      done
    with
    | () -> Eio.Flow.copy_string "ok\n" flow
    | exception (Eio.Io _ | End_of_file | Failure _) when !written = 2 ->
        if !got < 2 then
          Fmt.failwith "the server wrote both lines, the client read %d" !got
    | exception (Eio.Io _ | End_of_file | Failure _ | Eio.Time.Timeout) -> ()
  with
  | () -> ()
  | exception Eio.Io _ -> ()
