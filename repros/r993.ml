let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let addr =
    Eio.Switch.run @@ fun sw ->
    Eio.Net.listening_addr
      (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
  in
  let t0 = Unix.gettimeofday () in
  match
    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
        Eio.Net.connect ~sw net addr)
  with
  | _ ->
      print_endline "connected";
      exit 1
  | exception (Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) as e) ->
      Printf.printf "%s after %.1f s\n" (Printexc.to_string e)
        (Unix.gettimeofday () -. t0)
  | exception e ->
      print_endline (Printexc.to_string e);
      exit 1
