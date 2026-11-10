(* Records the kernel's outcome of every sequence through eio_posix, one CSV
   row per sequence: the operations, separated by ";", and their outcomes,
   separated by "|".

   record TRANSPORT         records to stdout, with Unix sockets in $KERNEL_DIR
                            (or the current directory)
   record scenarios         records the scenarios in the same way, one row each
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

let scenarios () =
  Option.iter Sys.chdir (Sys.getenv_opt "KERNEL_DIR");
  Eio_posix.run @@ fun env ->
  let net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  print_endline "ops,outcomes";
  (* macOS has no 127.0.0.2, and a connect there waits 75 s to time out. *)
  let clock = Eio.Stdenv.clock env in
  List.iter
    (fun name ->
      let r =
        match
          Eio.Time.with_timeout_exn clock 2. (fun () ->
              Kernel_seq.scenario net name)
        with
        | r -> r
        | exception Eio.Time.Timeout -> "timeout"
      in
      Fmt.pr "%s,%s@." name r)
    Kernel_seq.scenarios

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
  | [ _; "scenarios" ] -> scenarios ()
  | [ _; t ] -> run (Kernel_seq.transport_of_string t)
  | _ ->
      Fmt.epr
        "usage: record TRANSPORT | record scenarios | record merge LINUX \
         MACOS@.";
      exit 2
