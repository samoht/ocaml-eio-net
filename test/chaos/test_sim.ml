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

(* The lines of a report's section [name], each without its indent. *)
let section name report =
  let rec body = function
    | l :: rest when String.starts_with ~prefix:"    " l ->
        String.sub l 4 (String.length l - 4) :: body rest
    | _ -> []
  in
  let rec go = function
    | [] -> []
    | l :: rest when l = "  " ^ name ^ ":" -> body rest
    | _ :: rest -> go rest
  in
  go (String.split_on_char '\n' report)

let printed seed =
  section "output"
    (Expect.report (fun () -> Eio_net_chaos.run ~seed (main ~fail:true)))

(* The same seed draws the same port, and another seed another one. *)
let same_seed () =
  let a = printed 1 and b = printed 1 and c = printed 2 in
  Alcotest.(check (list string)) "same seed" a b;
  match (a, c) with
  | [ port; trace ], [ other; _ ]
    when String.starts_with ~prefix:"port " port
         && String.starts_with ~prefix:"+served on " trace ->
      if port = other then Alcotest.failf "seeds 1 and 2 both print %S" port
  | _ -> Alcotest.failf "the run printed %a" Fmt.(Dump.list string) a

(* A failure names the seed in its replay, which runs that case again. *)
let replay_seed () =
  let report =
    Expect.report (fun () -> Eio_net_chaos.run ~seed:3 (main ~fail:true))
  in
  let replay =
    Scanf.sscanf
      (Expect.field "replay" report)
      "Eio_net_chaos.run ~seed:3 ~replay:%S app" Fun.id
  in
  let again =
    Expect.report (fun () ->
        Eio_net_chaos.run ~seed:3 ~replay (main ~fail:true))
  in
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
           "Eio_net_chaos.run: the run with no fault failed: Eio.Io Unix_error \
            (Operation not permitted, \"spawn\", \"ls\")"
         report)
  then Alcotest.failf "the spawn failed with %S" report

let suite =
  ( "sim",
    [
      Alcotest.test_case "a program written against Eio.Stdenv runs" `Quick runs;
      Alcotest.test_case "the same seed prints the same" `Quick same_seed;
      Alcotest.test_case "a replay names its seed" `Quick replay_seed;
      Alcotest.test_case "a process is refused" `Quick spawn;
    ] )
