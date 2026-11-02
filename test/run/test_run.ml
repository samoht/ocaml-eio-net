let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 5000)
let aborted () = Eio_unix.Err.v Unix.ECONNABORTED "accept" ""
let no_descriptor () = Eio_unix.Err.v Unix.EMFILE "accept" ""

(* A listener over [inner] whose accept fails with each scripted exception in
   turn, once its cue resolves, and otherwise accepts from [inner]. A cue
   resolved already fails the accept before it races [inner]'s, which could
   take a peer the failure would then drop. *)
module Scripted = struct
  type t = {
    inner : [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
    mutable failures : (unit Eio.Promise.t * exn) list;
  }

  type tag = [ `Generic ]

  let accept t ~sw =
    match t.failures with
    | [] -> Eio.Net.accept ~sw t.inner
    | (cue, exn) :: rest when Eio.Promise.is_resolved cue ->
        t.failures <- rest;
        raise exn
    | (cue, exn) :: rest ->
        Eio.Fiber.first
          (fun () ->
            Eio.Promise.await cue;
            t.failures <- rest;
            raise exn)
          (fun () -> Eio.Net.accept ~sw t.inner)

  let close t = Eio.Resource.close t.inner
  let listening_addr t = Eio.Net.listening_addr t.inner
  let setsockopt t opt v = Eio.Net.setsockopt t.inner opt v
  let getsockopt t opt = Eio.Net.getsockopt t.inner opt
end

let scripted ?(addr = addr) ~sw net failures =
  let inner = Eio.Net.listen ~sw ~backlog:16 net addr in
  Eio.Resource.T
    ({ Scripted.inner; failures }, Eio.Net.Pi.listening_socket (module Scripted))

(* A protocol whose head is one line: it answers each line with [ok LINE] until
   its peer ends, saying before each line that the head is incomplete and its
   progress the bytes buffered, and once the line is in that it is complete.
   [served] runs between the line and its answer. *)
let line_server ?(served = ignore) head flow _addr =
  let reader = Eio.Buf_read.of_flow ~max_size:4096 flow in
  let rec next () =
    Eio_net_accept.reading head (fun () -> Eio.Buf_read.buffered_bytes reader);
    match Eio.Buf_read.line reader with
    | exception End_of_file -> ()
    | line ->
        Eio_net_accept.complete head;
        served line;
        Eio.Flow.copy_string ("ok " ^ line ^ "\n") flow;
        next ()
  in
  next ()

(* A peer that sends [data], ends its side and reads the answer to its end. *)
let exchange ?(addr = addr) net data =
  Eio.Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string data flow;
  Eio.Flow.shutdown flow `Send;
  Eio.Buf_read.(parse_exn take_all) flow ~max_size:4096

(* A peer that sends [data] and then nothing, and reads until its connection
   ends; [on_end] runs once it has. Any answer fails the case. *)
let stall ?(addr = addr) ~sw net data ~on_end =
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun csw ->
      let flow = Eio.Net.connect ~sw:csw net addr in
      Eio.Flow.copy_string data flow;
      match Eio.Flow.single_read flow (Cstruct.create 1) with
      | _ -> Alcotest.fail "a stalled head was answered"
      | exception End_of_file -> on_end ())

(* Under the mock backend the fibers run in a fixed order, so a fixed number of
   yields lets every accepted connection read what its peer sent. *)
let settle n =
  for _ = 1 to 10 * n do
    Eio.Fiber.yield ()
  done

let errors () =
  let seen = ref [] in
  (seen, fun e -> seen := Printexc.to_string e :: !seen)

let check_no_error seen =
  Alcotest.(check (list string)) "no connection reported an error" [] !seen

(* A peer that resets its connection before the accept makes the accept raise
   [Connection_reset] (ECONNABORTED); the loop accepts the next peer. *)
let test_aborted_accept_is_followed_by_the_next () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  Eio.Switch.run @@ fun scope ->
  let listening =
    scripted ~sw:scope net
      [
        (Eio.Promise.create_resolved (), aborted ());
        (Eio.Promise.create_resolved (), aborted ());
      ]
  in
  Eio.Fiber.fork_daemon ~sw:scope (fun () ->
      Eio_net_accept.run ~max_connections:4 ~on_error listening
        (line_server ?served:None));
  Alcotest.(check string)
    "the fresh peer is answered" "ok fresh\n" (exchange net "fresh\n");
  check_no_error seen

(* [n] peers each send part of a head and stop, peer [i] [n - i] bytes of it,
   so the last has made the least progress. Then the loop comes under
   pressure: every one of its [max_connections] is held and another peer
   connects, or an accept fails for lack of a descriptor (EMFILE). It sheds the
   last peer and no other, reports no error for it, and answers the fresh
   peer. *)
let pressure_sheds_the_least_progressed pressure n () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let max_connections =
    match pressure with `Held -> n | `Descriptors -> 10 * n
  in
  let pressed, press = Eio.Promise.create () in
  let ended = Array.make n false in
  let last_ended, last_ended_r = Eio.Promise.create () in
  Eio.Switch.run (fun clients ->
      Eio.Switch.run (fun scope ->
          let listening =
            scripted ~sw:scope net
              (match pressure with
              | `Held -> []
              | `Descriptors -> [ (pressed, no_descriptor ()) ])
          in
          Eio.Fiber.fork_daemon ~sw:scope (fun () ->
              Eio_net_accept.run ~max_connections ~on_error listening
                (line_server ?served:None));
          for i = 0 to n - 1 do
            stall ~sw:clients net
              (String.make (n - i) 'x')
              ~on_end:(fun () ->
                ended.(i) <- true;
                if i = n - 1 then Eio.Promise.resolve last_ended_r ())
          done;
          settle n;
          (match pressure with
          | `Held -> ()
          | `Descriptors ->
              Eio.Promise.resolve press ();
              Eio.Promise.await last_ended);
          Alcotest.(check string)
            "the fresh peer is answered" "ok fresh\n" (exchange net "fresh\n");
          Alcotest.(check (list int))
            "the least progressed head, and no other, was shed"
            [ n - 1 ]
            (List.filter (fun i -> ended.(i)) (List.init n Fun.id));
          check_no_error seen))

(* Loops handed one pool share their descriptors' pressure: [n] peers stall
   on one loop, peer [i] with [n - i] bytes of its head, and an accept of
   another loop of the pool, which holds none, fails for lack of a descriptor
   (EMFILE). The second loop sheds the first's least progressed head and no
   other, and answers a fresh peer of its own. *)
let descriptors_are_shared_across_loops n () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let other = `Tcp (Eio.Net.Ipaddr.V4.loopback, 5001) in
  let pressed, press = Eio.Promise.create () in
  let ended = Array.make n false in
  let last_ended, last_ended_r = Eio.Promise.create () in
  let pool = Eio_net_accept.pool () in
  Eio.Switch.run (fun clients ->
      Eio.Switch.run (fun scope ->
          let first = scripted ~sw:scope net [] in
          let second =
            scripted ~addr:other ~sw:scope net [ (pressed, no_descriptor ()) ]
          in
          Eio.Fiber.fork_daemon ~sw:scope (fun () ->
              Eio_net_accept.run ~pool ~max_connections:(10 * n) ~on_error first
                (line_server ?served:None));
          Eio.Fiber.fork_daemon ~sw:scope (fun () ->
              Eio_net_accept.run ~pool ~max_connections:(10 * n) ~on_error
                second (line_server ?served:None));
          for i = 0 to n - 1 do
            stall ~sw:clients net
              (String.make (n - i) 'x')
              ~on_end:(fun () ->
                ended.(i) <- true;
                if i = n - 1 then Eio.Promise.resolve last_ended_r ())
          done;
          settle n;
          Eio.Promise.resolve press ();
          Eio.Promise.await last_ended;
          Alcotest.(check string)
            "the second loop's fresh peer is answered" "ok fresh\n"
            (exchange ~addr:other net "fresh\n");
          Alcotest.(check string)
            "the first loop serves on" "ok more\n" (exchange net "more\n");
          Alcotest.(check (list int))
            "the first loop's least progressed head, and no other, was shed"
            [ n - 1 ]
            (List.filter (fun i -> ended.(i)) (List.init n Fun.id));
          check_no_error seen))

(* A loop sheds only the connections of the loops it shares a pool with. [n]
   peers stall on one loop, and an accept of another loop, with a pool of its
   own, fails for lack of a descriptor (EMFILE), as any listener can claim of
   its accept. That loop holds no connection to shed or wait for, so it raises
   the accept's error, and the first loop's peers are all still held. *)
let descriptors_are_not_shared_without_a_pool n () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let other = `Tcp (Eio.Net.Ipaddr.V4.loopback, 5001) in
  let ended = ref 0 in
  Eio.Switch.run (fun clients ->
      Eio.Switch.run (fun scope ->
          let first = scripted ~sw:scope net [] in
          let second =
            scripted ~addr:other ~sw:scope net
              [ (Eio.Promise.create_resolved (), no_descriptor ()) ]
          in
          Eio.Fiber.fork_daemon ~sw:scope (fun () ->
              Eio_net_accept.run ~max_connections:(10 * n) ~on_error first
                (line_server ?served:None));
          for i = 0 to n - 1 do
            stall ~sw:clients net
              (String.make (n - i) 'x')
              ~on_end:(fun () -> incr ended)
          done;
          settle n;
          let second =
            Eio.Fiber.fork_promise ~sw:scope (fun () ->
                Eio_net_accept.run ~max_connections:(10 * n) ~on_error second
                  (line_server ?served:None))
          in
          settle n;
          (match Eio.Promise.peek second with
          | Some (Error (Eio.Io _ as e)) ->
              Alcotest.(check bool)
                "the accept's error is raised" true
                (Eio_net_accept.classify e = Some `Descriptors)
          | Some (Error e) -> raise e
          | Some (Ok ()) -> Alcotest.fail "the second loop returned"
          | None -> Alcotest.fail "the second loop waits on the first's peers");
          Alcotest.(check int) "no peer of the first loop is shed" 0 !ended;
          Alcotest.(check string)
            "the first loop serves on" "ok more\n" (exchange net "more\n");
          check_no_error seen))

(* With no descriptor left and no connection held there is nothing to shed or
   wait for: the loop raises the accept's error. *)
let test_no_descriptor_and_none_held_raises () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let listening =
    scripted ~sw net [ (Eio.Promise.create_resolved (), no_descriptor ()) ]
  in
  match
    Eio_net_accept.run ~max_connections:4 ~on_error:ignore listening
      (line_server ?served:None)
  with
  | (_ : unit) -> Alcotest.fail "the loop returned"
  | exception (Eio.Io _ as e) ->
      Alcotest.(check bool)
        "the accept's error is raised" true
        (Eio_net_accept.classify e = Some `Descriptors)

(* A connection whose head is complete is being served and is never shed: a
   peer past [max_connections] waits for one to end. *)
let complete_heads_are_not_shed n () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let release, release_r = Eio.Promise.create () in
  let served _ = Eio.Promise.await release in
  let answered = ref 0 in
  Eio.Switch.run @@ fun scope ->
  let listening = scripted ~sw:scope net [] in
  Eio.Fiber.fork_daemon ~sw:scope (fun () ->
      Eio_net_accept.run ~max_connections:n ~on_error listening
        (line_server ~served));
  Eio.Switch.run (fun clients ->
      for _ = 1 to n + 1 do
        Eio.Fiber.fork ~sw:clients (fun () ->
            if String.equal (exchange net "held\n") "ok held\n" then
              incr answered)
      done;
      settle n;
      Alcotest.(check int) "no peer is answered or shed" 0 !answered;
      Eio.Promise.resolve release_r ());
  Alcotest.(check int) "every peer is answered" (n + 1) !answered;
  check_no_error seen

(* A connection that fails on its socket reports it to [on_error] and ends
   alone. *)
let test_a_failed_connection_leaves_the_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let served = function
    | "boom" -> raise (Eio_unix.Err.v Unix.ECONNRESET "write" "")
    | _ -> ()
  in
  Eio.Switch.run @@ fun scope ->
  let listening = scripted ~sw:scope net [] in
  Eio.Fiber.fork_daemon ~sw:scope (fun () ->
      Eio_net_accept.run ~max_connections:4 ~on_error listening
        (line_server ~served));
  Alcotest.(check string)
    "the failed peer is not answered" "" (exchange net "boom\n");
  Alcotest.(check string)
    "the next peer is answered" "ok fresh\n" (exchange net "fresh\n");
  Alcotest.(check int) "the failure is reported" 1 (List.length !seen)

(* Without [on_error], a failed connection is logged as a warning on the
   [eio-net.accept] source, and the loop serves on. *)
let test_a_failed_connection_is_logged_by_default () =
  let warnings = ref [] in
  let report src level ~over k msgf =
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Fmt.kstr
      (fun _ ->
        if level = Logs.Warning then warnings := Logs.Src.name src :: !warnings;
        over ();
        k ())
      fmt
  in
  let previous = Logs.reporter () in
  Logs.set_reporter { Logs.report };
  Fun.protect ~finally:(fun () -> Logs.set_reporter previous) @@ fun () ->
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let served = function
    | "boom" -> raise (Eio_unix.Err.v Unix.ECONNRESET "write" "")
    | _ -> ()
  in
  Eio.Switch.run @@ fun scope ->
  let listening = scripted ~sw:scope net [] in
  Eio.Fiber.fork_daemon ~sw:scope (fun () ->
      Eio_net_accept.run ~max_connections:4 listening (line_server ~served));
  Alcotest.(check string)
    "the failed peer is not answered" "" (exchange net "boom\n");
  Alcotest.(check string)
    "the next peer is answered" "ok fresh\n" (exchange net "fresh\n");
  Alcotest.(check (list string))
    "the failure is a warning of eio-net.accept" [ "eio-net.accept" ] !warnings

(* A peer that sends a length prefix past the bound makes its connection's
   handler raise [Eio_net_framing.Error]: the error is reported to [on_error]
   with the others, and the loop serves the next peer. *)
let test_a_malformed_peer_leaves_the_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen = ref [] in
  let on_error e = seen := e :: !seen in
  let handler head flow _ =
    Eio_net_accept.complete head;
    let channel = Eio_net.Frame.of_length_prefixed_flow flow in
    let frame = Eio_net.Frame.recv channel in
    Eio_net.Frame.send channel [ Cstruct.of_string ("ok " ^ frame) ]
  in
  Eio.Switch.run @@ fun scope ->
  let listening = scripted ~sw:scope net [] in
  Eio.Fiber.fork_daemon ~sw:scope (fun () ->
      Eio_net_accept.run ~max_connections:4 ~on_error listening handler);
  Alcotest.(check string)
    "the malformed peer is not answered" ""
    (exchange net "\255\255\255\255");
  Alcotest.(check string)
    "the next peer is answered" "\000\000\000\008ok fresh"
    (exchange net "\000\000\000\005fresh");
  match !seen with
  | [ Eio_net_framing.Error (Invalid_length _) ] -> ()
  | errors ->
      Alcotest.failf "expected one framing error, got [%a]"
        Fmt.(list ~sep:semi exn)
        errors

(* [on_error] may raise to stop: the exception ends the loop, which raises
   it. *)
let test_on_error_raising_ends_the_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let served = function "boom" -> failwith "boom" | _ -> () in
  match
    Eio.Switch.run @@ fun scope ->
    let listening = scripted ~sw:scope net [] in
    Eio.Fiber.fork_daemon ~sw:scope (fun () ->
        Eio_net_accept.run ~max_connections:4 ~on_error:raise listening
          (line_server ~served));
    ignore (exchange net "boom\n" : string)
  with
  | () -> Alcotest.fail "the loop went on"
  | exception Failure msg -> Alcotest.(check string) "the defect" "boom" msg

(* A connection served when [stop] resolves is shed as it next reads a head,
   and the failure of its shut socket is not reported. *)
let test_a_connection_shed_after_stop_is_not_reported () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let stop, stop_r = Eio.Promise.create () in
  let release, release_r = Eio.Promise.create () in
  Eio.Switch.run (fun scope ->
      let listening = scripted ~sw:scope net [] in
      Eio.Fiber.fork ~sw:scope (fun () ->
          Eio_net_accept.run ~stop ~max_connections:4 ~on_error listening
            (fun head flow _ ->
              Eio_net_accept.complete head;
              Eio.Promise.await release;
              Eio_net_accept.reading head (fun () -> 0);
              Eio.Flow.copy_string "late\n" flow));
      let (_ : _ Eio.Net.stream_socket) = Eio.Net.connect ~sw:scope net addr in
      settle 1;
      Eio.Promise.resolve stop_r ();
      settle 1;
      Eio.Promise.resolve release_r ());
  check_no_error seen

(* Once [stop] resolves the loop accepts no more, sheds each connection whose
   head is incomplete, at once or when it next reads a head, and returns
   [stop]'s value once the connections it is serving end. *)
let test_stop_sheds_incomplete_heads () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let seen, on_error = errors () in
  let stop, stop_r = Eio.Promise.create () in
  let release, release_r = Eio.Promise.create () in
  let served _ = Eio.Promise.await release in
  let stalled = ref 0 in
  let returned = ref None in
  let answer = ref "" in
  Eio.Switch.run (fun scope ->
      let listening = scripted ~sw:scope net [] in
      Eio.Fiber.fork ~sw:scope (fun () ->
          returned :=
            Some
              (Eio_net_accept.run ~stop ~max_connections:4 ~on_error listening
                 (line_server ~served)));
      for _ = 1 to 2 do
        stall ~sw:scope net "xx" ~on_end:(fun () -> incr stalled)
      done;
      Eio.Fiber.fork ~sw:scope (fun () ->
          Eio.Switch.run @@ fun csw ->
          let flow = Eio.Net.connect ~sw:csw net addr in
          Eio.Flow.copy_string "kept\n" flow;
          answer := Eio.Buf_read.(parse_exn take_all) flow ~max_size:4096);
      settle 3;
      Eio.Promise.resolve stop_r 42;
      settle 3;
      Alcotest.(check int) "both incomplete heads are shed" 2 !stalled;
      Alcotest.(check (option int))
        "the loop waits for the served" None !returned;
      Eio.Promise.resolve release_r ());
  Alcotest.(check (option int))
    "the loop returns stop's value" (Some 42) !returned;
  Alcotest.(check string)
    "the served peer is answered, then shed as it reads its next head"
    "ok kept\n" !answer;
  check_no_error seen

let suite =
  ( "run",
    [
      Alcotest.test_case "an aborted accept is followed by the next" `Quick
        test_aborted_accept_is_followed_by_the_next;
      Alcotest.test_case "10 held connections shed the least progressed" `Quick
        (pressure_sheds_the_least_progressed `Held 10);
      Alcotest.test_case "100 held connections shed the least progressed" `Quick
        (pressure_sheds_the_least_progressed `Held 100);
      Alcotest.test_case "no descriptor sheds the least progressed of 10" `Quick
        (pressure_sheds_the_least_progressed `Descriptors 10);
      Alcotest.test_case "no descriptor sheds the least progressed of 100"
        `Quick
        (pressure_sheds_the_least_progressed `Descriptors 100);
      Alcotest.test_case "no descriptor on a second loop sheds of 10" `Quick
        (descriptors_are_shared_across_loops 10);
      Alcotest.test_case "no descriptor on a second loop sheds of 100" `Quick
        (descriptors_are_shared_across_loops 100);
      Alcotest.test_case "no descriptor sheds no other pool's of 10" `Quick
        (descriptors_are_not_shared_without_a_pool 10);
      Alcotest.test_case "no descriptor sheds no other pool's of 100" `Quick
        (descriptors_are_not_shared_without_a_pool 100);
      Alcotest.test_case "no descriptor and none held raises" `Quick
        test_no_descriptor_and_none_held_raises;
      Alcotest.test_case "10 complete heads are not shed" `Quick
        (complete_heads_are_not_shed 10);
      Alcotest.test_case "100 complete heads are not shed" `Quick
        (complete_heads_are_not_shed 100);
      Alcotest.test_case "a failed connection leaves the loop" `Quick
        test_a_failed_connection_leaves_the_loop;
      Alcotest.test_case "a failed connection is logged by default" `Quick
        test_a_failed_connection_is_logged_by_default;
      Alcotest.test_case "a malformed peer leaves the loop" `Quick
        test_a_malformed_peer_leaves_the_loop;
      Alcotest.test_case "on_error raising ends the loop" `Quick
        test_on_error_raising_ends_the_loop;
      Alcotest.test_case "a connection shed after stop is not reported" `Quick
        test_a_connection_shed_after_stop_is_not_reported;
      Alcotest.test_case "stop sheds the incomplete heads" `Quick
        test_stop_sheds_incomplete_heads;
    ] )
