let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let listen name =
    match Eio.Net.listen ~sw ~backlog:1 net (`Unix "\000eio-repro") with
    | l ->
        Format.printf "%s: ok at %a@." name Eio.Net.Sockaddr.pp
          (Eio.Net.listening_addr l);
        true
    | exception e ->
        Printf.printf "%s: %s\n" name (Printexc.to_string e);
        false
  in
  let a = listen "first listen" in
  let b = listen "second listen" in
  if a && b then exit 1
