(* Prints the README given as its argument, its section on what mem models of
   each kernel made again from Scope's table. *)

let start = "<!-- Made from lib/mem/scope.ml"
let stop = "<!-- End of the section made from lib/mem/scope.ml -->"

let text = function
  | Scope.Modelled -> None
  | Approximated s | Stored_only s | Unsupported s ->
      if s = "" then None else Some s

(* Each distinct text of the table, in the order the tables first cite it. *)
let notes =
  List.fold_left
    (fun acc (f : Scope.feature) ->
      List.fold_left
        (fun acc c ->
          match text c with
          | Some s when not (List.mem s acc) -> acc @ [ s ]
          | _ -> acc)
        acc [ f.linux; f.macos ])
    []
    (Scope.behaviours @ Scope.options)

let rec index s i = function
  | [] -> raise Not_found
  | n :: _ when n = s -> i
  | _ :: rest -> index s (i + 1) rest

let cell c =
  let word =
    match c with
    | Scope.Modelled -> "modelled"
    | Approximated _ -> "approximated"
    | Stored_only _ -> "stored only"
    | Unsupported _ -> "unsupported"
  in
  match text c with
  | None -> word
  | Some s -> Fmt.str "%s (%d)" word (index s 1 notes)

let table ppf heading features =
  Fmt.pf ppf "@\n| %s | Linux | macOS |@\n| --- | --- | --- |@\n" heading;
  List.iter
    (fun (f : Scope.feature) ->
      Fmt.pf ppf "| %s | %s | %s |@\n" f.name (cell f.linux) (cell f.macos))
    features

(* [text] in lines of at most 80 columns, cut at spaces, each after the first
   indented by [indent]. *)
let wrap ~indent text =
  let b = Buffer.create 256 in
  let col = ref 0 in
  List.iter
    (fun w ->
      if !col = 0 then (
        Buffer.add_string b w;
        col := String.length w)
      else if !col + 1 + String.length w > 80 then (
        Buffer.add_char b '\n';
        Buffer.add_string b indent;
        Buffer.add_string b w;
        col := String.length indent + String.length w)
      else (
        Buffer.add_char b ' ';
        Buffer.add_string b w;
        col := !col + 1 + String.length w))
    (String.split_on_char ' ' text);
  Buffer.contents b

let intro =
  "eio-net.mem follows the chosen kernel in most of what a test can see, but \
   not in all. The tables below say, for each behaviour and socket option, how \
   it follows Linux and macOS: modelled, as the kernel behaves; approximated, \
   within the bound its note gives; stored only, set and read back with no \
   other effect; or unsupported, refused. They are made from \
   [`lib/mem/scope.ml`](lib/mem/scope.ml), and a test checks the class of each \
   option against what a connection does."

let section () =
  let b = Buffer.create 8192 in
  let ppf = Fmt.with_buffer b in
  Fmt.pf ppf "%s: dune build @@runtest --auto-promote -->@\n@\n%s@\n" start
    (wrap ~indent:"" intro);
  table ppf "Behaviour" Scope.behaviours;
  table ppf "Option" Scope.options;
  Fmt.pf ppf "@\nNotes:@\n@\n";
  List.iteri
    (fun i s ->
      let n = Fmt.str "%d. " (i + 1) in
      Fmt.pf ppf "%s@\n"
        (wrap ~indent:(String.make (String.length n) ' ') (n ^ s)))
    notes;
  Fmt.pf ppf "@\n%s@\n%!" stop;
  Buffer.contents b

let () =
  let lines = In_channel.with_open_text Sys.argv.(1) In_channel.input_lines in
  let rec copy found = function
    | [] ->
        if not found then
          failwith "README.md has no section made from lib/mem/scope.ml"
    | l :: rest when String.starts_with ~prefix:start l ->
        print_string (section ());
        skip rest
    | l :: rest ->
        print_endline l;
        copy found rest
  and skip = function
    | [] -> failwith "README.md: the section made from scope.ml has no end"
    | l :: rest when l = stop -> copy true rest
    | _ :: rest -> skip rest
  in
  copy false lines
