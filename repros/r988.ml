let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let closed =
    Eio.Switch.run @@ fun sw ->
    let s = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
    match Eio_unix.Resource.fd_opt s with
    | Some fd ->
        Eio_unix.Fd.use_exn "getsockname" fd (fun u ->
            match Unix.getsockname u with
            | Unix.ADDR_INET (_, p) -> p
            | _ -> assert false)
    | None -> assert false
  in
  let s = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
  Eio.Net.send s ~dst:(`Udp (lo, closed)) [ Cstruct.of_string "x" ];
  Eio.Time.sleep clock 0.5;
  match
    Eio.Time.with_timeout_exn clock 2. (fun () ->
        Eio.Net.recv s (Cstruct.create 4))
  with
  | _, n ->
      Printf.printf "received %d bytes\n" n;
      exit 1
  | exception Eio.Time.Timeout -> print_endline "still waiting after 2 s"
  | exception e ->
      print_endline (Printexc.to_string e);
      exit 1
