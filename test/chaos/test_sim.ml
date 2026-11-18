(* The environment of a run: Eio's standard one, each device simulated. *)

(* A program written against Eio.Stdenv's accessors: it draws its port from
   the entropy, says so, serves one client and checks the answer. With
   [fail], it then fails, so a report shows what it printed. *)
let main ?(fail = false) env =
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  let random = Cstruct.create 2 in
  Eio.Flow.read_exact (Eio.Stdenv.secure_random env) random;
  let port = 1024 + (Cstruct.BE.get_uint16 random 0 mod 50_000) in
  let out = Eio.Stdenv.stdout env in
  Eio.Flow.copy_string (Fmt.str "port %d\n" port) out;
  Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.001;
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      Eio.Switch.run @@ fun sw ->
      let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:5 net addr in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
              Eio.Flow.copy_string "hello" flow));
      match
        Eio.Time.with_timeout_exn clock 5.0 @@ fun () ->
        Eio.Switch.run @@ fun sw ->
        let flow = Eio.Net.connect ~sw net addr in
        Eio.Buf_read.(parse_exn take_all) ~max_size:100 flow
      with
      | got when String.starts_with ~prefix:got "hello" -> ()
      | got -> Fmt.failwith "got %S" got
      | exception (Eio.Io _ | Eio.Time.Timeout) -> ());
  Eio.traceln "served on %d" port;
  if fail then failwith "done"

let runs () = Eio_net_chaos.run main

(* What a report says the program printed, a line each. *)
let section name report =
  match Expect.field name report with
  | "" -> []
  | first ->
      let rec rest = function
        | l :: more when String.starts_with ~prefix:"            " l ->
            String.trim l :: rest more
        | _ -> []
      in
      let rec go = function
        | [] -> []
        | l :: more when String.starts_with ~prefix:("  " ^ name ^ ":") l ->
            first :: rest more
        | _ :: more -> go more
      in
      go (String.split_on_char '\n' report)

let printed seed =
  section "output"
    (Expect.report (fun () -> Eio_net_chaos.run ~seed (main ~fail:true)))

(* The entropy does not depend on the seed: two seeds draw the same port. *)
let same_seed () =
  let a = printed 1 and b = printed 1 and c = printed 2 in
  Alcotest.(check (list string)) "same seed" a b;
  match a with
  | [ port; trace ]
    when String.starts_with ~prefix:"port " port
         && String.starts_with ~prefix:"served on " trace ->
      Alcotest.(check (list string)) "another seed" a c
  | _ -> Alcotest.failf "the run printed %a" Fmt.(Dump.list string) a

(* The program, which fails once a fault met it. *)
let faulted env =
  main env;
  if Eio_chaos.faulted env#chaos then failwith "a fault met it"

(* A failure's replay names no seed, and runs that case again under any. *)
let replay_seed () =
  let report = Expect.report (fun () -> Eio_net_chaos.run ~seed:3 faulted) in
  let replay =
    Scanf.sscanf
      (Expect.field "replay" report)
      "Eio_net_chaos.run ~replay:%S app" Fun.id
  in
  let again = Expect.report (fun () -> Eio_net_chaos.run ~replay faulted) in
  Alcotest.(check string) "replayed" report again

(* A program may not start a process. *)
let spawn () =
  let report =
    Expect.report (fun () ->
        Eio_net_chaos.run (fun env ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr env) [ "ls" ])))
  in
  if
    not
      (String.starts_with
         ~prefix:
           "Eio_net_chaos.run: the run with no fault failed: Eio.Io \
            Not_available env#process_mgr: spawn ls"
         report)
  then Alcotest.failf "the spawn failed with %S" report

let suite =
  ( "sim",
    [
      Alcotest.test_case "a program written against Eio.Stdenv runs" `Quick runs;
      Alcotest.test_case "every seed prints the same" `Quick same_seed;
      Alcotest.test_case "a replay names no seed" `Quick replay_seed;
      Alcotest.test_case "a process is refused" `Quick spawn;
    ] )
