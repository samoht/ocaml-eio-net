(* Each oracle directory under test/interop holds its committed traces in
   traces/, beside a traces/provenance file that test/provenance/write.sh
   fills in when they are recorded. The file is "key: value" lines, "#" lines
   being comments. It names the generator, the command that regenerates the
   traces, the revision, date, user and host they were recorded at, and pins
   with MD5s the generator files that decide the traces ("input: PATH MD5")
   and every trace ("trace: PATH MD5"), paths relative to the oracle
   directory. *)

let required =
  [ "generator"; "regenerate"; "revision"; "recorded"; "uid"; "host" ]

let fields file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.filter_map (fun line ->
      if line = "" || line.[0] = '#' then None
      else
        match String.index_opt line ':' with
        | Some i ->
            let value = String.sub line (i + 1) (String.length line - i - 1) in
            Some (String.sub line 0 i, String.trim value)
        | None -> Fmt.failwith "%s: bad line %S" file line)

let pins file fields key =
  List.filter_map
    (fun (k, v) ->
      if k <> key then None
      else
        match String.split_on_char ' ' v with
        | [ path; md5 ] -> Some (path, md5)
        | _ -> Fmt.failwith "%s: bad %s %S" file key v)
    fields

let md5 file = Digest.to_hex (Digest.file file)

let check_pins dir ~regen ~what pins =
  List.iter
    (fun (path, recorded) ->
      let file = Filename.concat dir path in
      if not (Sys.file_exists file) then
        Alcotest.failf "%s: %s %s is gone: %s" dir what path regen
      else if md5 file <> recorded then
        Alcotest.failf "%s: %s %s changed since the traces were recorded: %s"
          dir what path regen)
    pins

let check dir () =
  let traces = Filename.concat dir "traces" in
  let file = Filename.concat traces "provenance" in
  if not (Sys.file_exists file) then
    Alcotest.failf "%s has no provenance: record its traces with REGEN=1" dir;
  let fields = fields file in
  List.iter
    (fun key ->
      match List.assoc_opt key fields with
      | Some v when v <> "" -> ()
      | _ -> Alcotest.failf "%s names no %s" file key)
    required;
  let regen = "regenerate them with " ^ List.assoc "regenerate" fields in
  let inputs = pins file fields "input"
  and recorded = pins file fields "trace" in
  if inputs = [] then Alcotest.failf "%s pins no input" file;
  check_pins dir ~regen ~what:"input" inputs;
  let held =
    Sys.readdir traces |> Array.to_list
    |> List.filter (fun f -> f <> "dune" && f <> "provenance")
    |> List.map (Filename.concat "traces")
    |> List.sort String.compare
  in
  let listed = List.sort String.compare (List.map fst recorded) in
  if listed <> held then
    Alcotest.failf "%s pins the traces [%s], but %s holds [%s]" file
      (String.concat " " listed) traces (String.concat " " held);
  check_pins dir ~regen ~what:"trace" recorded

let interop = Filename.concat ".." "interop"

let dirs =
  Sys.readdir interop |> Array.to_list
  |> List.filter (fun d ->
      Sys.file_exists (Filename.concat interop (Filename.concat d "traces")))
  |> List.sort String.compare

let suite =
  ( "provenance",
    Alcotest.test_case "the oracle directories are found" `Quick (fun () ->
        Alcotest.(check bool)
          "kernel and cpython-struct" true
          (List.mem "kernel" dirs && List.mem "cpython-struct" dirs))
    :: List.map
         (fun d ->
           Alcotest.test_case d `Quick (check (Filename.concat interop d)))
         dirs )
