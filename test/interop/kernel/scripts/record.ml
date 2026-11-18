(* Records the kernel's outcome of every sequence through Backend
   (eio_windows on Windows, eio_posix elsewhere), one CSV row per sequence:
   the operations, separated by ";", and their outcomes, separated by "|".

   record TRANSPORT         records to stdout, with Unix sockets in $KERNEL_DIR
                            (or the current directory), waiting
                            $KERNEL_SETTLE seconds (none by default) after
                            each operation that can send a segment, and
                            giving each operation $KERNEL_TIMEOUT seconds
                            (2 by default)
   record scenarios         records the scenarios in the same way, one row each:
                            those off the loopback interface only, when
                            $KERNEL_LOOPBACK_ONLY is set on a host with no
                            other interface, those that run out of ephemeral
                            ports only, when $KERNEL_NARROW_PORTS is set on a
                            host whose range is narrowed to a few ports, and
                            the others otherwise, each given
                            $KERNEL_TIMEOUT seconds (2 by default), or 300 s
                            more for one that lets time pass
   record overlay A B       is A, with B's outcome where A has none
   record merge KIND TRACE COLUMN=FILE...
                            is the trace TRACE of KIND (a transport or
                            scenarios) with the columns the recordings hold
                            replaced by theirs, a row per sequence or
                            scenario: an answer neither they nor TRACE hold
                            is not recorded
   record backend           names the backend *)

let timeout () =
  Option.fold ~none:2. ~some:float_of_string (Sys.getenv_opt "KERNEL_TIMEOUT")

let run t =
  Option.iter Sys.chdir (Sys.getenv_opt "KERNEL_DIR");
  Backend.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let settle =
    Option.map
      (fun s () -> Eio.Time.sleep env#clock (float_of_string s))
      (Sys.getenv_opt "KERNEL_SETTLE")
  in
  let timeout = timeout () in
  let bound f =
    match Eio.Time.with_timeout_exn env#clock timeout f with
    | r -> Some r
    | exception Eio.Time.Timeout -> None
  in
  print_endline "ops,outcomes";
  List.iter
    (fun ops ->
      let outcomes = Kernel_seq.run ?settle ~bound net t ops in
      Fmt.pr "%s,%s@." (String.concat ";" ops)
        (String.map
           (function ',' -> ';' | c -> c)
           (String.concat "|" outcomes)))
    (Kernel_seq.sequences t)

let not_recorded = "not recorded"

let scenarios () =
  Option.iter Sys.chdir (Sys.getenv_opt "KERNEL_DIR");
  Backend.run @@ fun env ->
  let net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  print_endline "ops,outcomes";
  (* macOS has no 127.0.0.2, and a connect there waits 75 s to time out. The
     timed scenarios run together, each sleeping on the clock. *)
  let clock = Eio.Stdenv.clock env in
  let loopback_only = Sys.getenv_opt "KERNEL_LOOPBACK_ONLY" <> None in
  let narrow = Sys.getenv_opt "KERNEL_NARROW_PORTS" <> None in
  let timeout = timeout () in
  let run name =
    if
      Kernel_seq.narrow_ports name <> narrow
      || ((not narrow) && Kernel_seq.off_loopback name <> loopback_only)
    then not_recorded
    else
      (* One that lets time pass waits less than 300 s. *)
      let waits = Kernel_seq.timed name || narrow in
      let wait = if waits then Some (Eio.Time.sleep clock) else None in
      let bound = if waits then timeout +. 300. else timeout in
      match
        Eio.Time.with_timeout_exn clock bound (fun () ->
            Kernel_seq.scenario ?wait net name)
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
  In_channel.with_open_bin file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; outcomes ] -> (ops, outcomes)
      | _ -> failwith ("record overlay: " ^ line))

let overlay a b =
  print_endline "ops,outcomes";
  List.iter2
    (fun (ops, x) (ops', y) ->
      if ops <> ops' then failwith ("record overlay: " ^ ops ^ " / " ^ ops');
      Fmt.pr "%s,%s@." ops (if x = not_recorded then y else x))
    (rows a) (rows b)

(* The traces' columns, in order: a host records some of them, and the
   others are kept from the trace before. *)
let columns = [ "linux"; "macos"; "windows eio_windows" ]

(* A trace's or a recording's answers, by row and column. *)
let read ~columns file =
  let answers = Hashtbl.create 8192 in
  (match In_channel.with_open_bin file In_channel.input_lines with
  | [] -> failwith ("record merge: empty " ^ file)
  | header :: lines ->
      let names =
        Option.value columns
          ~default:(List.tl (String.split_on_char ',' header))
      in
      List.iter
        (fun line ->
          match String.split_on_char ',' line with
          | ops :: cells when List.compare_lengths cells names = 0 ->
              List.iter2
                (fun c a -> Hashtbl.replace answers (ops, c) a)
                names cells
          | _ -> failwith ("record merge: " ^ file ^ ": " ^ line))
        lines);
  answers

let merge kind trace recordings =
  let keys =
    match kind with
    | "scenarios" -> Kernel_seq.scenarios
    | t ->
        List.map (String.concat ";")
          (Kernel_seq.sequences (Kernel_seq.transport_of_string t))
  in
  let recorded =
    List.map
      (fun r ->
        match String.index_opt r '=' with
        | Some i ->
            let column = String.sub r 0 i in
            if not (List.mem column columns) then
              failwith ("record merge: no column " ^ column);
            let file = String.sub r (i + 1) (String.length r - i - 1) in
            (column, read ~columns:(Some [ column ]) file)
        | None -> failwith ("record merge: " ^ r ^ " is not COLUMN=FILE"))
      recordings
  in
  let before =
    if Sys.file_exists trace then read ~columns:None trace else Hashtbl.create 1
  in
  let answer ops column =
    let from = Option.value (List.assoc_opt column recorded) ~default:before in
    Option.value (Hashtbl.find_opt from (ops, column)) ~default:not_recorded
  in
  print_endline (String.concat "," ("ops" :: columns));
  List.iter
    (fun ops ->
      print_endline (String.concat "," (ops :: List.map (answer ops) columns)))
    keys

let () =
  set_binary_mode_out stdout true;
  match Array.to_list Sys.argv with
  | _ :: "merge" :: kind :: trace :: recordings -> merge kind trace recordings
  | [ _; "backend" ] -> print_endline Backend.name
  | [ _; "overlay"; a; b ] -> overlay a b
  | [ _; "scenarios" ] -> scenarios ()
  | [ _; t ] -> run (Kernel_seq.transport_of_string t)
  | _ ->
      Fmt.epr
        "usage: record TRANSPORT | record scenarios | record overlay A B | \
         record merge TRACE_KIND TRACE COLUMN=FILE... | record backend@.";
      exit 2
