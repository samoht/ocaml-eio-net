(* NUL through the unrestricted fs: a file named NUL made by a verbatim
   path, read back by its plain path, and the device itself; then whether a
   failed read of the device reaches another fiber waiting for a socket. *)
let show name f =
  match f () with
  | s -> Printf.printf "%s: %S\n" name s
  | exception e -> Printf.printf "%s: %s\n" name (Printexc.to_string e)

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Sys.getcwd () in
  let sep = if Sys.win32 then "\\" else "/" in
  if Sys.win32 then
    show "save \\\\?\\<cwd>\\NUL" (fun () ->
        Eio.Path.save ~create:(`Or_truncate 0o644)
          Eio.Path.(fs / ("\\\\?\\" ^ dir ^ "\\NUL"))
          "y";
        "ok");
  show "load <cwd>\\NUL" (fun () ->
      Eio.Path.load Eio.Path.(fs / (dir ^ sep ^ "NUL")));
  if Sys.win32 then begin
    show "load \\\\.\\NUL" (fun () ->
        Eio.Path.load Eio.Path.(fs / "\\\\.\\NUL"));
    show "Stdlib <cwd>\\NUL" (fun () ->
        In_channel.with_open_bin (dir ^ "\\NUL") In_channel.input_all)
  end;
  try
    Eio.Switch.run @@ fun sw ->
    let sock =
      Eio.Net.datagram_socket ~sw (Eio.Stdenv.net env)
        (`Udp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let fd = Option.get (Eio_unix.Resource.fd_opt sock) in
    let waiter =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio_unix.Fd.use_exn "await" fd Eio_unix.await_readable)
    in
    Eio.Fiber.yield ();
    show "load \\\\.\\NUL beside a socket wait" (fun () ->
        Eio.Path.load
          Eio.Path.(fs / if Sys.win32 then "\\\\.\\NUL" else "/dev/null"));
    Eio.Time.sleep env#clock 0.5;
    match Eio.Promise.peek waiter with
    | None ->
        print_endline "socket wait: still waiting";
        Eio.Switch.fail sw Exit
    | Some (Ok ()) -> print_endline "socket wait: readable"
    | Some (Error e) -> Printf.printf "socket wait: %s\n" (Printexc.to_string e)
  with Exit -> ()
