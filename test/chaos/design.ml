(* Probes of the design: a run that keeps a timer going, the budget, and a
   download that resumes. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

(* A workload whose daemon ticks every second, and whose client reads with
   no timeout: a stalled peer would spin the clock for ever. *)
let ticking () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Time.sleep env#clock 1.0
      done;
      `Stop_daemon);
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
         let flow, _ = Eio.Net.accept ~sw l in
         App.Frame.write flow "hello"
       with Eio.Io _ -> ());
      `Stop_daemon);
  match Eio.Net.connect ~sw env#net addr with
  | flow -> (
      try ignore (App.Frame.read ~bug:`No_timeout ~clock:env#clock flow)
      with Eio.Io _ | App.Frame.Truncated | End_of_file -> ())
  | exception Eio.Io _ -> ()

(* [transfer ~budget n size] downloads [n] writes of [size] bytes. *)
let transfer ?budget n size () =
  let big = String.init size (fun i -> Char.chr (65 + (i mod 26))) in
  Eio_net_chaos.check ?budget @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
         let flow, _ = Eio.Net.accept ~sw l in
         for _ = 1 to n do
           Eio.Flow.copy_string big flow
         done;
         Eio.Flow.shutdown flow `Send
       with Eio.Io _ -> ());
      `Stop_daemon);
  match Eio.Net.connect ~sw env#net addr with
  | exception Eio.Io _ -> ()
  | flow ->
      let got =
        try
          Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
              Eio.Flow.read_all flow)
        with Eio.Io _ | Eio.Time.Timeout -> ""
      in
      let expect = String.concat "" (List.init n (fun _ -> big)) in
      if
        not
          (String.length got <= String.length expect
          && String.sub expect 0 (String.length got) = got)
      then failwith "the transfer is corrupt"

let resume ?replay ?bug () =
  Eio_net_chaos.check ?replay @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Resume.serve ~clock:env#clock flow));
  match
    (Resume.fetch ?bug ~clock:env#clock env#net addr, Eio_net_chaos.outage env)
  with
  | Ok d, false when d = Resume.data -> ()
  | Error `Gave_up, true -> ()
  | Ok d, _ ->
      Fmt.failwith "downloaded %d bytes, %s" (String.length d)
        (if d = Resume.data then "right" else "wrong")
  | Error `Gave_up, false -> failwith "gave up"
