(* Writes the module of the kernel's traces from their CSV files, given as
   NAME=FILE, their linux, macos and windows columns found by name: each sequence's
   answers as an index into the distinct ones, and each scenario's answers. *)

let rows file =
  match In_channel.with_open_bin file In_channel.input_lines with
  | header :: rows ->
      let names = String.split_on_char ',' header in
      let index name =
        let rec go i = function
          | [] -> Fmt.failwith "%s: no column %S" file name
          | n :: _ when n = name -> i
          | _ :: r -> go (i + 1) r
        in
        go 0 names
      in
      let linux = index "linux"
      and macos = index "macos"
      and windows = index "windows eio_windows" in
      List.map
        (fun line ->
          let cells = String.split_on_char ',' line in
          if List.compare_lengths cells names <> 0 then
            Fmt.failwith "%s: bad row %S" file line;
          (List.nth cells linux, List.nth cells macos, List.nth cells windows))
        rows
  | [] -> Fmt.failwith "%s: not a trace" file

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
  let row ppf (l, m, w) = Fmt.pf ppf "(%S, %S, %S)" l m w in
  Fmt.pr "let answers =@\n  [|@\n%a|]@\n@\n"
    Fmt.(list ~sep:nop (any "    " ++ row ++ any ";@\n"))
    (List.rev !answers);
  let code ppf (name, codes) = Fmt.pf ppf "(%S, %S)" name codes in
  Fmt.pr "let sequences =@\n  [@\n%a]@\n@\n"
    Fmt.(list ~sep:nop (any "    " ++ code ++ any ";@\n"))
    codes;
  Fmt.pr "let scenarios =@\n  [|@\n%a|]@\n"
    Fmt.(list ~sep:nop (any "    " ++ row ++ any ";@\n"))
    (List.concat_map snd scenarios)
