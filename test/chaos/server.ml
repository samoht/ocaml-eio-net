(* A request/response server: one frame in, the frame upper-cased out. *)
let handle ?(bug = `None) ~clock flow =
  let read () = App.Frame.read ~clock flow in
  match
    if bug = `No_idle_timeout then read ()
    else Eio.Time.with_timeout_exn clock 5.0 read
  with
  | Some req -> App.Frame.write flow (String.uppercase_ascii req)
  | None -> ()

let serve ?(bug = `None) ~clock l =
  match bug with
  | `Sequential ->
      (* One connection at a time: a stalled client blocks the rest. *)
      let rec loop () =
        Eio.Switch.run (fun sw ->
            let flow, _ = Eio.Net.accept ~sw l in
            try handle ~bug:`No_idle_timeout ~clock flow with Eio.Io _ -> ());
        loop ()
      in
      loop ()
  | _ ->
      Eio.Net.run_server l
        ~on_error:(if bug = `Raise_on_error then raise else ignore)
        (fun flow _ -> handle ~bug ~clock flow)
