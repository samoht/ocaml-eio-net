let ok = ref true

let check name f =
  match f () with
  | () -> Printf.printf "%s: ok\n" name
  | exception (Eio.Io _ as e) ->
      Printf.printf "%s: Eio.Io %s\n" name (Printexc.to_string e)
  | exception e ->
      Printf.printf "%s: not Eio.Io: %s\n" name (Printexc.to_string e);
      ok := false

let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let net = Eio.Stdenv.net env in
  ( Eio.Switch.run @@ fun sw ->
    let p = Eio.Path.(cwd / "r987.tmp") in
    Eio.Path.save ~create:(`Or_truncate 0o644) p "hello";
    let f = Eio.Path.open_out ~sw ~create:`Never p in
    let off = Optint.Int63.of_int 1 in
    check "pwrite_all" (fun () ->
        Eio.File.pwrite_all f ~file_offset:off [ Cstruct.of_string "EL" ]);
    check "pread" (fun () ->
        let buf = Cstruct.create 3 in
        let n = Eio.File.pread f ~file_offset:off [ buf ] in
        Printf.printf "pread: %d bytes %S\n" n (Cstruct.to_string ~len:n buf));
    check "pread negative offset" (fun () ->
        ignore
          (Eio.File.pread f ~file_offset:(Optint.Int63.of_int (-1))
             [ Cstruct.create 1 ])) );
  Eio.Switch.run @@ fun sw ->
  check "datagram_socket on a Unix path" (fun () ->
      ignore (Eio.Net.datagram_socket ~sw net (`Unix "r987.sock")));
  if not !ok then exit 1
