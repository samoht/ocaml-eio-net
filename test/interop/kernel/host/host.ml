(* Eio_net_chaos.device on the host's own network, eio_posix, under the
   host's kernel. Run outside any sandbox that refuses sockets:
   dune build @test/interop/kernel/host/host-device *)

let os () =
  match In_channel.input_all (Unix.open_process_in "uname -s") with
  | "Darwin\n" -> `Macos
  | _ -> `Linux

let () =
  let os = os () in
  match
    Eio_main.run @@ fun env ->
    Eio_net_chaos.device ~os (fun ~sw:_ -> Eio.Stdenv.net env)
  with
  | () -> print_endline "the host's network passes"
  | exception Failure report ->
      print_endline report;
      exit 1
