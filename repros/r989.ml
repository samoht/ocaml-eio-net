let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let save n =
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(cwd / n) "x"
  in
  save "f";
  save "h";
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(cwd / "d");
  save "d/g";
  Eio.Switch.run @@ fun sw ->
  List.iter
    (fun n -> ignore (Eio.Path.open_in ~sw Eio.Path.(cwd / n)))
    [ "f"; "h"; "d/g" ];
  let ok = ref true in
  let try_ name f =
    match f () with
    | () -> Printf.printf "%s: ok\n" name
    | exception e ->
        Printf.printf "%s: %s\n" name (Printexc.to_string e);
        ok := false
  in
  try_ "rename f" (fun () ->
      Eio.Path.rename Eio.Path.(cwd / "f") Eio.Path.(cwd / "f2"));
  try_ "unlink h" (fun () -> Eio.Path.unlink Eio.Path.(cwd / "h"));
  try_ "rename d" (fun () ->
      Eio.Path.rename Eio.Path.(cwd / "d") Eio.Path.(cwd / "d2"));
  if not !ok then exit 1
