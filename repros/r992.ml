let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  match Eio.Net.getaddrinfo_stream net "localhost" ~service:"nosuchservice" with
  | l ->
      Printf.printf "%d addresses\n" (List.length l);
      exit 1
  | exception (Eio.Io (Eio.Net.E (Address_lookup_failed SERVICE), _) as e) ->
      print_endline (Printexc.to_string e)
  | exception e ->
      print_endline (Printexc.to_string e);
      exit 1
