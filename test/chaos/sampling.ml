(* A download whose client reads its head and its closing line under a
   timeout, as an HTTP client with a header timeout does. *)

let tcp = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7070)
let head = 4
let line tag i = Fmt.str "%c%06d" tag i

let download ?(bug = `None) ~body (env : Eio_net_chaos.env) =
  let within f = Eio.Time.with_timeout_exn env#clock 2.0 f in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net tcp in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          for i = 1 to head do
            Eio.Flow.copy_string (line 'H' i ^ "\n") flow
          done;
          for i = 1 to body do
            Eio.Flow.copy_string (line 'B' i ^ "\n") flow
          done;
          let r = Eio.Buf_read.of_flow flow ~max_size:64 in
          if within (fun () -> App.strict_line r) = "ACK" then
            Eio.Flow.copy_string "BYE\n" flow));
  let expect r bound want =
    let got = bound (fun () -> App.strict_line r) in
    if got <> want then Fmt.failwith "got %S, want %S" got want
  in
  let unbounded f = f () in
  match
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net tcp in
    let r = Eio.Buf_read.of_flow flow ~max_size:64 in
    for i = 1 to head do
      expect r within (line 'H' i)
    done;
    for i = 1 to body do
      expect r (if bug = `Body then unbounded else within) (line 'B' i)
    done;
    Eio.Flow.copy_string "ACK\n" flow;
    expect r within "BYE"
  with
  | () -> ()
  | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout) -> ()
