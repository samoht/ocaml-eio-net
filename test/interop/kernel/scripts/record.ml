(* Records the kernel's outcome of every sequence through eio_posix, one CSV
   row per sequence: the operations, separated by ";", and their outcomes,
   separated by "|".

   record TRANSPORT         records to stdout, with Unix sockets in $KERNEL_DIR
                            (or the current directory)
   record merge LINUX MACOS joins two recordings of the same sequences *)

let run t =
  Option.iter Sys.chdir (Sys.getenv_opt "KERNEL_DIR");
  Eio_posix.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  print_endline "ops,outcomes";
  List.iter
    (fun ops ->
      let outcomes = Kernel_seq.run net t ops in
      Fmt.pr "%s,%s@." (String.concat ";" ops) (String.concat "|" outcomes))
    (Kernel_seq.sequences t)

let rows file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; outcomes ] -> (ops, outcomes)
      | _ -> failwith ("record merge: " ^ line))

let merge linux macos =
  print_endline "ops,linux,macos";
  List.iter2
    (fun (ops, l) (ops', m) ->
      if ops <> ops' then failwith ("record merge: " ^ ops ^ " / " ^ ops');
      Fmt.pr "%s,%s,%s@." ops l m)
    (rows linux) (rows macos)

let () =
  match Array.to_list Sys.argv with
  | [ _; "merge"; linux; macos ] -> merge linux macos
  | [ _; t ] -> run (Kernel_seq.transport_of_string t)
  | _ ->
      Fmt.epr "usage: record TRANSPORT | record merge LINUX MACOS@.";
      exit 2
