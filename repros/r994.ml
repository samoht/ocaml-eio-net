let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let l =
    Eio.Net.listen ~sw ~backlog:2 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let connect () =
    let c = Eio.Net.connect ~sw net (Eio.Net.listening_addr l) in
    ignore (Eio.Net.accept ~sw l);
    c
  in
  let ok = ref true in
  let c = connect () in
  Eio.Flow.shutdown c `Send;
  (match Eio.Flow.copy_string "x" c with
  | () ->
      print_endline "write after shutdown `Send: ok";
      ok := false
  | exception Eio.Io (Eio.Net.E (Connection_reset _), _) ->
      print_endline "write after shutdown `Send: Connection_reset"
  | exception e ->
      Printf.printf "write after shutdown `Send: %s\n" (Printexc.to_string e);
      ok := false);
  let c = connect () in
  Eio.Flow.shutdown c `Receive;
  (match
     Eio.Time.with_timeout_exn env#clock 5. (fun () ->
         Eio.Flow.single_read c (Cstruct.create 4))
   with
  | n -> Printf.printf "read after shutdown `Receive: %d bytes\n" n
  | exception End_of_file ->
      print_endline "read after shutdown `Receive: End_of_file"
  | exception e ->
      Printf.printf "read after shutdown `Receive: %s\n" (Printexc.to_string e);
      ok := false);
  if not !ok then exit 1
