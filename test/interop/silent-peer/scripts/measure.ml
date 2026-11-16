(* Times how long a TCP connection over loopback whose peer has gone silent
   takes to fail once a byte it sent goes unacknowledged, on the monotonic
   clock, and prints one "CASE,SECONDS,FIRST,READ,WRITE,READ" line per
   connection, once all have ended.

   measure N

   Each connection exchanges a byte each way, waits [settle] seconds for the
   last acknowledgements, then its accepted end drops every segment it
   sends (see silence_stubs.c), and its client runs CASE: write-read writes a byte, then
   reads; fill writes until a write blocks. SECONDS runs from the client's
   first write to the end of the call that blocked, FIRST is how that call
   ended, and READ, WRITE and READ how the calls after it do. N connections
   per case overlap, started [stagger] seconds apart: a kernel fires a long
   timer at the end of a granule of its timer wheel, so how late one fails
   depends on when it started. *)

external silence : Unix.file_descr -> unit = "caml_measure_silence"

let stagger = 0.7
let settle = 0.5

let outcome = function
  | Unix.Unix_error (e, _, _) -> (
      match e with
      | ETIMEDOUT -> "ETIMEDOUT"
      | EPIPE -> "EPIPE"
      | ECONNRESET -> "ECONNRESET"
      | e -> String.map (function ',' -> ' ' | c -> c) (Unix.error_message e))
  | e ->
      String.map (function ',' | '\n' -> ' ' | c -> c) (Printexc.to_string e)

let read c =
  match Unix.read c (Bytes.create 16) 0 16 with
  | 0 -> "eof"
  | _ -> "data"
  | exception e -> outcome e

let write c =
  match Unix.single_write_substring c "x" 0 1 with
  | _ -> "ok"
  | exception e -> outcome e

let rec fill c chunk =
  match Unix.single_write c chunk 0 (Bytes.length chunk) with
  | _ -> fill c chunk
  | exception e -> outcome e

(* A connection and its accepted end, which stays open until the run ends. *)
let connection () =
  let l = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind l (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen l 1;
  let c = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.connect c (Unix.getsockname l);
  let s, _ = Unix.accept l in
  Unix.close l;
  let b = Bytes.create 1 in
  let exchange src dst =
    if Unix.write_substring src "x" 0 1 <> 1 || Unix.read dst b 0 1 <> 1 then
      failwith "measure: the exchange failed"
  in
  exchange c s;
  exchange s c;
  Thread.delay settle;
  silence s;
  (c, s)

let run case c =
  let t = Mtime_clock.counter () in
  let first =
    match case with
    | "write-read" ->
        if write c <> "ok" then failwith "measure: the first write failed";
        read c
    | "fill" -> fill c (Bytes.make 65536 'x')
    | _ -> invalid_arg case
  in
  let seconds = Mtime.Span.to_float_ns (Mtime_clock.count t) /. 1e9 in
  let r1 = read c in
  let w = write c in
  let r2 = read c in
  Printf.sprintf "%s,%.3f,%s,%s,%s,%s" case seconds first r1 w r2

let () =
  match Sys.argv with
  | [| _; n |] ->
      Sys.set_signal Sys.sigpipe Signal_ignore;
      let cases =
        List.concat_map
          (fun case -> List.init (int_of_string n) (fun i -> (case, i)))
          [ "write-read"; "fill" ]
      in
      let results = Array.make (List.length cases) "" in
      let kept = Array.make (List.length cases) None in
      let go j (case, i) =
        Thread.delay (stagger *. float i);
        let c, s = connection () in
        kept.(j) <- Some s;
        results.(j) <- run case c;
        Unix.close c
      in
      List.mapi (fun j x -> Thread.create (go j) x) cases
      |> List.iter Thread.join;
      Array.iter print_endline results
  | _ ->
      prerr_endline "usage: measure N";
      exit 2
