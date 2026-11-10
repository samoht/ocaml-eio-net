(* A download that resumes after a reset. The server sends [data] from the
   offset the client asks for, in chunks of 100 bytes. With [`Stale] the
   client resumes right once, then reuses that first offset. *)
let data = String.init 600 (fun i -> Char.chr (65 + (i mod 26)))

let serve ~clock flow =
  match App.Frame.read ~bug:`No_timeout ~clock flow with
  | exception (Eio.Io _ | End_of_file | App.Frame.Truncated) -> ()
  | None -> ()
  | Some off -> (
      let off = int_of_string off in
      let rec go i =
        if i < String.length data then (
          Eio.Flow.copy_string
            (String.sub data i (min 100 (String.length data - i)))
            flow;
          go (i + 100))
      in
      (try go off with Eio.Io _ -> ());
      try Eio.Flow.shutdown flow `Send with Eio.Io _ -> ())

let fetch ?(bug = `None) ~clock net addr =
  let buf = Buffer.create 600 and first = ref None in
  let rec attempt n =
    if n > 4 then Error `Gave_up
    else
      let off =
        match (bug, !first) with `Stale, Some o -> o | _ -> Buffer.length buf
      in
      if n > 1 && !first = None then first := Some off;
      if bug <> `Stale then Buffer.truncate buf off;
      match
        Eio.Time.with_timeout_exn clock 2.0 (fun () ->
            Eio.Switch.run @@ fun sw ->
            let flow = Eio.Net.connect ~sw net addr in
            App.Frame.write flow (string_of_int off);
            let chunk = Cstruct.create 256 in
            let rec loop () =
              match Eio.Flow.single_read flow chunk with
              | k ->
                  Buffer.add_string buf (Cstruct.to_string ~len:k chunk);
                  loop ()
              | exception End_of_file -> ()
            in
            loop ())
      with
      | () when Buffer.length buf >= String.length data ->
          Ok (Buffer.contents buf)
      | () -> attempt (n + 1)
      | exception (Eio.Io _ | Eio.Time.Timeout) -> attempt (n + 1)
  in
  attempt 1
