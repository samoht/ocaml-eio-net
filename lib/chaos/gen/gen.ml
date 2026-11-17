(* Writes the module of the kernel's traces from their CSV files, given as
   NAME=FILE: each sequence's answers as an index into the distinct ones, and
   each scenario's answers. *)

let rows file =
  match In_channel.with_open_text file In_channel.input_lines with
  | "ops,linux,macos" :: rows ->
      List.map
        (fun line ->
          match String.split_on_char ',' line with
          | [ _; linux; macos ] -> (linux, macos)
          | _ -> Fmt.failwith "%s: bad row %S" file line)
        rows
  | _ -> Fmt.failwith "%s: not a trace" file

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let traces =
    List.map
      (fun a ->
        match String.index_opt a '=' with
        | Some i ->
            ( String.sub a 0 i,
              rows (String.sub a (i + 1) (String.length a - i - 1)) )
        | None -> Fmt.failwith "%s is not NAME=FILE" a)
      args
  in
  let scenarios, sequences =
    List.partition (fun (name, _) -> name = "scenarios") traces
  in
  let index = Hashtbl.create 1024 and answers = ref [] in
  let code row =
    match Hashtbl.find_opt index row with
    | Some i -> i
    | None ->
        let i = Hashtbl.length index in
        Hashtbl.add index row i;
        answers := row :: !answers;
        i
  in
  let codes =
    List.map
      (fun (name, rows) ->
        let b = Buffer.create (2 * List.length rows) in
        List.iter (fun row -> Buffer.add_uint16_be b (code row)) rows;
        (name, Buffer.contents b))
      sequences
  in
  if Hashtbl.length index > 0xffff then failwith "too many distinct answers";
  let row ppf (l, m) = Fmt.pf ppf "(%S, %S)" l m in
  Fmt.pr "let answers =@\n  [|@\n%a|]@\n@\n"
    Fmt.(list ~sep:nop (any "    " ++ row ++ any ";@\n"))
    (List.rev !answers);
  Fmt.pr "let sequences =@\n  [@\n%a]@\n@\n"
    Fmt.(list ~sep:nop (any "    " ++ row ++ any ";@\n"))
    codes;
  Fmt.pr "let scenarios =@\n  [|@\n%a|]@\n"
    Fmt.(list ~sep:nop (any "    " ++ row ++ any ";@\n"))
    (List.concat_map snd scenarios)
