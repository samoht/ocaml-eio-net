(* Records the kernel's outcome of every sequence through eio_posix, one CSV
   row per sequence: the operations, separated by ";", and their outcomes,
   separated by "|".

   record TRANSPORT         records to stdout, with Unix sockets in $KERNEL_DIR
                            (or the current directory)
   record scenarios         records the scenarios in the same way, one row each:
                            those off the loopback interface only, when
                            $KERNEL_LOOPBACK_ONLY is set on a host with no
                            other interface, those that run out of ephemeral
                            ports only, when $KERNEL_NARROW_PORTS is set on a
                            host whose range is narrowed to a few ports, and
                            the others otherwise, each given
                            $KERNEL_TIMEOUT seconds (2 by default)
   record overlay A B       is A, with B's outcome where A has none
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

let not_recorded = "not recorded"

let scenarios () =
  Option.iter Sys.chdir (Sys.getenv_opt "KERNEL_DIR");
  Eio_posix.run @@ fun env ->
  let net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  print_endline "ops,outcomes";
  (* macOS has no 127.0.0.2, and a connect there waits 75 s to time out. The
     timed scenarios run together, each sleeping on the clock. *)
  let clock = Eio.Stdenv.clock env in
  let loopback_only = Sys.getenv_opt "KERNEL_LOOPBACK_ONLY" <> None in
  let narrow = Sys.getenv_opt "KERNEL_NARROW_PORTS" <> None in
  let timeout =
    Option.fold ~none:2. ~some:float_of_string (Sys.getenv_opt "KERNEL_TIMEOUT")
  in
  let run name =
    if
      Kernel_seq.narrow_ports name <> narrow
      || ((not narrow) && Kernel_seq.off_loopback name <> loopback_only)
    then not_recorded
    else if Kernel_seq.timed name || narrow then
      Kernel_seq.scenario ~wait:(Eio.Time.sleep clock) net name
    else
      match
        Eio.Time.with_timeout_exn clock timeout (fun () ->
            Kernel_seq.scenario net name)
      with
      | r -> r
      | exception Eio.Time.Timeout -> "timeout"
  in
  (* Those that run out of ports run one at a time, as they share the
     range. *)
  let timed, untimed =
    List.partition
      (fun name -> Kernel_seq.timed name && not narrow)
      Kernel_seq.scenarios
  in
  let results = Hashtbl.create 16 in
  List.iter (fun name -> Hashtbl.replace results name (run name)) untimed;
  Eio.Fiber.List.iter
    (fun name -> Hashtbl.replace results name (run name))
    timed;
  List.iter
    (fun name -> Fmt.pr "%s,%s@." name (Hashtbl.find results name))
    Kernel_seq.scenarios

let rows file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; outcomes ] -> (ops, outcomes)
      | _ -> failwith ("record merge: " ^ line))

let overlay a b =
  print_endline "ops,outcomes";
  List.iter2
    (fun (ops, x) (ops', y) ->
      if ops <> ops' then failwith ("record overlay: " ^ ops ^ " / " ^ ops');
      Fmt.pr "%s,%s@." ops (if x = not_recorded then y else x))
    (rows a) (rows b)

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
  | [ _; "overlay"; a; b ] -> overlay a b
  | [ _; "scenarios" ] -> scenarios ()
  | [ _; t ] -> run (Kernel_seq.transport_of_string t)
  | _ ->
      Fmt.epr
        "usage: record TRANSPORT | record scenarios | record overlay A B | \
         record merge LINUX MACOS@.";
      exit 2
