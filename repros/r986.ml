let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let r = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
  let port =
    match Eio_unix.Resource.fd_opt r with
    | Some fd ->
        Eio_unix.Fd.use_exn "getsockname" fd (fun u ->
            match Unix.getsockname u with
            | Unix.ADDR_INET (_, p) -> p
            | _ -> assert false)
    | None -> assert false
  in
  let s = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
  Eio.Net.send s ~dst:(`Udp (lo, port)) [ Cstruct.of_string "abc" ];
  let buf = Cstruct.create 1 in
  match Eio.Net.recv r buf with
  | _, n ->
      Printf.printf "received %d byte %S\n" n (Cstruct.to_string buf);
      if n <> 1 || Cstruct.to_string buf <> "a" then exit 1
  | exception e ->
      print_endline (Printexc.to_string e);
      exit 1
