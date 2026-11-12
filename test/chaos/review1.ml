(* Review 1: a request-response server under many clients, and its idle
   timeout. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)
let n = 5

let test ~serve () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:n env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () -> serve ~clock:env#clock l);
  let client i =
    let req = Fmt.str "req%d" i in
    match
      Eio.Time.with_timeout_exn env#clock 10.0 (fun () ->
          Eio.Switch.run @@ fun sw ->
          let flow = Eio.Net.connect ~sw env#net addr in
          App.Frame.write flow req;
          App.Frame.read ~clock:env#clock flow)
    with
    | Some r when r = String.uppercase_ascii req -> 1
    | Some r -> Fmt.failwith "client %d got %S" i r
    | None | (exception (Eio.Io _ | Eio.Time.Timeout | App.Frame.Truncated)) ->
        0
  in
  let served =
    List.fold_left ( + ) 0 (Eio.Fiber.List.map client (List.init n Fun.id))
  in
  (* A run loses at most two things; the outage loses them all. *)
  if served < n - 2 && not (Eio_net_chaos.outage env) then
    Fmt.failwith "%d of %d clients served" served n

(* The idle timeout needs a client that stalls, so it pins one: it sends
   nothing, and sees the server's close. A hang would hide that close. *)
let test_idle ~serve () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let l = Eio.Net.listen ~sw ~backlog:n net addr in
  Eio.Fiber.fork_daemon ~sw (fun () -> serve ~clock:env#clock l);
  let flow = Eio.Net.connect ~sw net addr in
  (* The client sends nothing; the server must close within its timeout.
     The client itself waits longer than that. *)
  let read () = App.Frame.read ~bug:`No_timeout ~clock:env#clock flow in
  match Eio.Time.with_timeout_exn env#clock 30.0 read with
  | None | (exception (Eio.Io _ | App.Frame.Truncated)) -> ()
  | Some _ -> failwith "an answer to nothing"
  | exception Eio.Time.Timeout ->
      failwith "the server keeps a client that sends nothing"
