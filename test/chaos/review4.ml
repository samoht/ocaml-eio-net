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

(* An echo of 4 KiB: the server reads the request to its end, then answers
   it. With [`Ignore_short] the server ignores the count [single_write]
   returns. *)
let echo ?(bug = `None) (env : Eio_net_chaos.env) =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7400) in
  let payload = String.init 4096 (fun i -> Char.chr (97 + (i mod 26))) in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:4 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let request =
            Eio.Time.with_timeout_exn env#clock 5. (fun () ->
                Eio.Buf_read.(parse_exn take_all) flow ~max_size:8192)
          in
          let data = Cstruct.of_string request in
          (match bug with
          | `Ignore_short -> ignore (Eio.Flow.single_write flow [ data ])
          | `None -> Eio.Flow.write flow [ data ]);
          Eio.Flow.shutdown flow `Send));
  let once () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    Eio.Flow.copy_string payload flow;
    Eio.Flow.shutdown flow `Send;
    Eio.Buf_read.(parse_exn take_all) flow ~max_size:8192
  in
  let rec go n =
    match Eio.Time.with_timeout_exn env#clock 5. once with
    | s -> s = payload || (n < 3 && go (n + 1))
    | exception (Eio.Io _ | End_of_file | Failure _ | Eio.Time.Timeout) ->
        n < 3 && go (n + 1)
  in
  if (not (go 1)) && not (Eio_net_chaos.outage env) then failwith "gave up"

(* An upload of 300,000 bytes on a Unix-domain connection, which the server
   counts and acknowledges. With [`Ignore_short] the client sends it with one
   [single_write] and ignores the count, as a full AF_UNIX buffer cuts such a
   write short. *)
let unix_upload ?(bug = `None) (env : Eio_net_chaos.env) =
  let addr = `Unix "/run/upload.sock" and size = 300_000 in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:4 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let got =
            Eio.Time.with_timeout_exn env#clock 10. (fun () ->
                Eio.Buf_read.(parse_exn take_all) flow ~max_size:(2 * size))
          in
          Eio.Flow.copy_string (Fmt.str "%d\n" (String.length got)) flow));
  let once () =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    let data = Cstruct.create size in
    (match bug with
    | `Ignore_short -> ignore (Eio.Flow.single_write flow [ data ])
    | `None -> Eio.Flow.write flow [ data ]);
    Eio.Flow.shutdown flow `Send;
    App.strict_line (Eio.Buf_read.of_flow flow ~max_size:16)
  in
  let rec go n =
    match Eio.Time.with_timeout_exn env#clock 20. once with
    | ack ->
        if ack <> string_of_int size then Fmt.failwith "acknowledged %s" ack
    | exception (Eio.Io _ | End_of_file | Failure _ | Eio.Time.Timeout) ->
        if n < 3 then go (n + 1)
        else if not (Eio_net_chaos.outage env) then failwith "gave up"
  in
  go 1
