(* A fault-free wrapper against its bare network, on generated sequences of
   operations: closes and double closes, uses after a close, the end of the
   switch with sockets and operations open, cancels of a blocked read, accept,
   connect or receive, operations pending as another fiber closes their socket,
   and shutdowns of each direction. Each step's outcome and time must match,
   and through a spy so must the sockets left open after each step and once
   the sequence closed every socket it holds. Under random faults, that last
   check alone holds. *)

module O = Eio.Net.Sockopt

type stream = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
type datagram = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t
type listener = [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t
type end_ = C | S | C2 | S2
type dgram = Rx | Tx

(* An operation that waits in a fiber of its own while the next steps run,
   until one of them, or the end of its switch, ends it. *)
type pending = Read_pending of end_ | Accept_pending | Recv_pending

type step =
  | Write of end_
  | Copy of end_
  | Read of end_
  | Shutdown of end_ * Eio.Flow.shutdown_command
  | Sockopt of end_
  | Close of end_
  | Connect
  | Accept
  | Listening_addr
  | Close_listener
  | Send
  | Recv
  | Close_datagram of dgram
  | Pending of pending
  | Release

let end_name = function C -> "c" | S -> "s" | C2 -> "c2" | S2 -> "s2"
let dgram_name = function Rx -> "rx" | Tx -> "tx"

let command_name = function
  | `Send -> "send"
  | `Receive -> "receive"
  | `All -> "all"

let step_name = function
  | Write e -> end_name e ^ " write"
  | Copy e -> end_name e ^ " copy"
  | Read e -> end_name e ^ " read"
  | Shutdown (e, c) -> Fmt.str "%s shutdown %s" (end_name e) (command_name c)
  | Sockopt e -> end_name e ^ " sockopt"
  | Close e -> end_name e ^ " close"
  | Connect -> "connect"
  | Accept -> "accept"
  | Listening_addr -> "listening addr"
  | Close_listener -> "listener close"
  | Send -> "send"
  | Recv -> "recv"
  | Close_datagram d -> dgram_name d ^ " close"
  | Pending (Read_pending e) -> end_name e ^ " read pending"
  | Pending Accept_pending -> "accept pending"
  | Pending Recv_pending -> "recv pending"
  | Release -> "release"

let gen rng =
  let pick l = List.nth l (Random.State.int rng (List.length l)) in
  let end_ () = pick [ C; S; C; S; C; S; C2; S2 ] in
  match Random.State.int rng 23 with
  | 0 | 1 | 2 -> Write (end_ ())
  | 3 -> Copy (end_ ())
  | 4 | 5 | 6 -> Read (end_ ())
  | 7 | 8 -> Shutdown (end_ (), pick [ `Send; `Receive; `All ])
  | 9 -> Sockopt (end_ ())
  | 10 | 11 -> Close (end_ ())
  | 12 -> Connect
  | 13 -> Accept
  | 14 -> Listening_addr
  | 15 -> Close_listener
  | 16 -> Send
  | 17 -> Recv
  | 18 -> Close_datagram (pick [ Rx; Tx ])
  | 19 -> Pending (Read_pending (end_ ()))
  | 20 -> Pending Accept_pending
  | 21 -> Pending Recv_pending
  | _ -> Release

let sequence seed =
  let rng = Random.State.make [| seed |] in
  List.init (6 + Random.State.int rng 20) (fun _ -> gen rng)

let outcome f =
  match f () with
  | s -> s
  | exception Exit -> "-"
  | exception End_of_file -> "end of file"
  | exception Invalid_argument _ -> "Invalid_argument"
  | exception Eio.Time.Timeout -> "blocks"
  | exception Eio.Cancel.Cancelled _ -> "cancelled"
  | exception Eio.Io (e, _) -> Fmt.str "%a" Eio.Exn.pp_err e

let stream_addr family : Eio.Net.Sockaddr.stream =
  match family with
  | `Inet -> `Tcp (Eio.Net.Ipaddr.V4.loopback, 8000)
  | `Unix -> `Unix "/seq.sock"

let dgram_addr family n : Eio.Net.Sockaddr.datagram =
  match family with
  | `Inet -> `Udp (Eio.Net.Ipaddr.V4.loopback, 9000 + n)
  | `Unix -> `Unix (Fmt.str "/seq-dgram-%d.sock" n)

(* The sockets a sequence holds: [lost] are the ends a later connect or
   accept took the name of. *)
type state = {
  mutable l : listener option;
  mutable ends : (end_ * stream) list;
  mutable lost : stream list;
  mutable dgrams : (dgram * datagram) list;
}

let name st e flow =
  Option.iter (fun old -> st.lost <- old :: st.lost) (List.assoc_opt e st.ends);
  st.ends <- (e, flow) :: List.remove_assoc e st.ends

let need = function Some x -> x | None -> raise Exit
let stream st e = need (List.assoc_opt e st.ends)
let datagram st d = need (List.assoc_opt d st.dgrams)

(* A blocked operation is cancelled after this long, which no stall or
   timeout of a fault matches. *)
let patience = 2.5

let read flow () =
  let buf = Cstruct.create 4 in
  let n = Eio.Flow.single_read flow buf in
  Cstruct.to_string ~len:n buf

let recv rx () =
  let buf = Cstruct.create 4 in
  let _, n = Eio.Net.recv rx buf in
  Cstruct.to_string ~len:n buf

let accept ~sw st () =
  let s, _ = Eio.Net.accept ~sw (need st.l) in
  name st S2 s;
  "ok"

(* A pending operation's fiber gives up after this long, unless something
   ends it first. *)
let pending_patience = 30.

let exec ~clock ~sw ~family ~note net st step () =
  let wait f = Eio.Time.with_timeout_exn clock patience f in
  let pending name f =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        note (name ^ " ends")
          (outcome (fun () ->
               Eio.Time.with_timeout_exn clock pending_patience f));
        `Stop_daemon);
    "pending"
  in
  match step with
  | Write e ->
      string_of_int
        (Eio.Flow.single_write (stream st e) [ Cstruct.of_string "ab" ])
  | Copy e ->
      Eio.Flow.copy_string "xyz" (stream st e);
      "ok"
  | Read e -> wait (read (stream st e))
  | Shutdown (e, cmd) ->
      Eio.Flow.shutdown (stream st e) cmd;
      "ok"
  | Sockopt e ->
      let flow = stream st e in
      Eio.Net.setsockopt flow O.SO_KEEPALIVE true;
      string_of_bool (Eio.Net.getsockopt flow O.SO_KEEPALIVE)
  | Close e ->
      Eio.Resource.close (stream st e);
      "ok"
  | Connect ->
      let c = wait (fun () -> Eio.Net.connect ~sw net (stream_addr family)) in
      name st C2 c;
      "ok"
  | Accept ->
      ignore (need st.l);
      wait (accept ~sw st)
  | Listening_addr ->
      Fmt.str "%a" Eio.Net.Sockaddr.pp (Eio.Net.listening_addr (need st.l))
  | Close_listener ->
      Eio.Resource.close (need st.l);
      "ok"
  | Send ->
      Eio.Net.send (datagram st Tx) ~dst:(dgram_addr family 0)
        [ Cstruct.of_string "d" ];
      "ok"
  | Recv -> wait (recv (datagram st Rx))
  | Close_datagram d ->
      Eio.Resource.close (datagram st d);
      "ok"
  | Pending (Read_pending e) as step ->
      pending (step_name step) (read (stream st e))
  | Pending Accept_pending as step ->
      ignore (need st.l);
      pending (step_name step) (accept ~sw st)
  | Pending Recv_pending as step ->
      pending (step_name step) (recv (datagram st Rx))
  | Release -> "ok"

(* What [steps] do on [net]: each step's outcome and time, and with [spy] the
   sockets left open after it, then those left open once every socket held is
   closed. The sockets live in a switch of their own, which [Release] ends;
   the steps after it run on the closed sockets. *)
let transcript ~clock ~family ?spy net steps =
  let log = ref [] in
  let st = { l = None; ends = []; lost = []; dgrams = [] } in
  let note name r =
    let opened =
      match spy with
      | None -> ""
      | Some spy ->
          Fmt.str " [%a]" Fmt.(list ~sep:comma string) (Spy.opened spy)
    in
    log := Fmt.str "%s: %s at %g%s" name r (Eio.Time.now clock) opened :: !log
  in
  let run ~sw step =
    note (step_name step) (outcome (exec ~clock ~sw ~family ~note net st step))
  in
  let released = ref false in
  Eio.Switch.run (fun sw ->
      let rest =
        Eio.Switch.run @@ fun csw ->
        let l = Eio.Net.listen ~sw:csw ~backlog:1 net (stream_addr family) in
        st.l <- Some l;
        let c = Eio.Net.connect ~sw:csw net (stream_addr family) in
        let s, _ = Eio.Net.accept ~sw:csw l in
        st.ends <- [ (C, c); (S, s) ];
        let dgram n =
          Eio.Net.datagram_socket ~sw:csw net (dgram_addr family n)
        in
        st.dgrams <- [ (Rx, dgram 0); (Tx, dgram 1) ];
        let rec go = function
          | [] -> []
          | Release :: rest ->
              released := true;
              rest
          | step :: rest ->
              run ~sw:csw step;
              go rest
        in
        go steps
      in
      if !released then note "release" "ok";
      List.iter (run ~sw) rest;
      let close = Eio.Resource.close in
      List.iter (fun (_, s) -> close s) st.ends;
      List.iter close st.lost;
      List.iter (fun (_, d) -> close d) st.dgrams;
      Option.iter close st.l;
      note "close all" "ok");
  List.rev !log

let pp_step ppf s = Fmt.string ppf (step_name s)

let configs =
  [
    (`Linux, `Inet, "Linux TCP");
    (`Linux, `Unix, "Linux AF_UNIX");
    (`Macos, `Inet, "macOS TCP");
    (`Macos, `Unix, "macOS AF_UNIX");
  ]

(* The transcript of [steps] on eio-net.mem under [os], through a spy if
   [spy], and wrapped with [f] if there is one. *)
let run ~os ~family ~spy ?f steps =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  Eio.Switch.run @@ fun sw ->
  let mem = Eio_net_mem.v ~os ~mono:env#mono_clock ~sw () in
  let spy, base =
    if spy then
      let s, n = Spy.v mem in
      (Some s, n)
    else (None, mem)
  in
  let net =
    match f with
    | Some f ->
        Wrapper.v ~os ~clock ~outage:false
          (fun op -> Option.map (fun f -> (f, 0)) (f op))
          base
    | None -> base
  in
  transcript ~clock ~family ?spy net steps

let seeds = List.init 400 Fun.id

(* Edges a sequence must reach for the comparison to cover them. *)
let edges =
  let line s l =
    let n = String.length s in
    let rec at x i =
      i + n <= String.length x && (String.sub x i n = s || at x (i + 1))
    in
    List.exists (fun x -> at x 0) l
  in
  let twice l =
    List.exists
      (fun e ->
        let p = end_name e ^ " close: ok" in
        List.length (List.filter (String.starts_with ~prefix:p) l) >= 2)
      [ C; S ]
  in
  (* A read pending on an end, then that end's close, then the read's end. *)
  let closed_under_read l =
    List.exists
      (fun e ->
        let e = end_name e in
        let rec go state = function
          | [] -> false
          | x :: rest ->
              let starts p = String.starts_with ~prefix:(e ^ p) x in
              if state = 0 && starts " read pending: pending" then go 1 rest
              else if state = 1 && starts " close: ok" then go 2 rest
              else if state = 2 && starts " read pending ends" then true
              else go state rest
        in
        go 0 l)
      [ C; S ]
  in
  [
    ("a blocked read cancelled", line "read: blocks");
    ("a blocked accept cancelled", line "accept: blocks");
    ("a blocked connect cancelled", line "connect: blocks");
    ("a blocked receive cancelled", line "recv: blocks");
    ("an operation pending as its switch ends", line "ends: cancelled");
    ("a read pending as its flow closes", closed_under_read);
    ("a read after a close", line "read: Invalid_argument");
    ("a write after a close", line "write: Invalid_argument");
    ("a double close", twice);
    ("a switch ended with sockets open", line "release: ok");
    ("a send shutdown", line "shutdown send: ok");
    ("a receive shutdown", line "shutdown receive: ok");
    ("a full shutdown", line "shutdown all: ok");
  ]

let pp_failure ppf (name, seed, steps, bare, wrapped) =
  Fmt.pf ppf "@[<v>%s, seed %d: %a@,@,bare:@,%a@,@,wrapped:@,%a@]" name seed
    Fmt.(list ~sep:(any ", ") pp_step)
    steps
    Fmt.(list ~sep:cut string)
    bare
    Fmt.(list ~sep:cut string)
    wrapped

let test_sequences () =
  let seen = ref [] in
  List.iter
    (fun (os, family, name) ->
      List.iter
        (fun spy ->
          List.iter
            (fun seed ->
              let steps = sequence seed in
              let bare = run ~os ~family ~spy steps in
              let wrapped = run ~os ~family ~spy ~f:(fun _ -> None) steps in
              seen := bare :: !seen;
              if bare <> wrapped then
                Alcotest.failf "%a" pp_failure
                  ( (if spy then name ^ " through a spy" else name),
                    seed,
                    steps,
                    bare,
                    wrapped ))
            seeds)
        [ false; true ])
    configs;
  List.iter
    (fun (edge, reached) ->
      if not (List.exists reached !seen) then
        Alcotest.failf "no sequence reaches %s" edge)
    edges

(* A fault drawn for about one operation in three, after the connect and the
   accept that start a sequence. *)
let faulty seed =
  let rng = Random.State.make [| seed; 1 |] in
  fun (op : Op.t) ->
    if op.index > 2 && Random.State.int rng 3 = 0 then
      Some (List.nth Faults.all (Random.State.int rng (List.length Faults.all)))
    else None

let test_faults_release () =
  List.iter
    (fun (os, family, name) ->
      List.iter
        (fun seed ->
          let steps = sequence seed in
          let t = run ~os ~family ~spy:true ~f:(faulty seed) steps in
          let last = List.nth t (List.length t - 1) in
          if not (String.ends_with ~suffix:"[]" last) then
            Alcotest.failf "@[<v>%s, seed %d: %a@,@,%a@]" name seed
              Fmt.(list ~sep:(any ", ") pp_step)
              steps
              Fmt.(list ~sep:cut string)
              t)
        seeds)
    configs

let suite =
  ( "wrapper",
    [
      Alcotest.test_case "a fault-free wrapper does what its network does"
        `Quick test_sequences;
      Alcotest.test_case "under faults, a close of every socket closes its base"
        `Quick test_faults_release;
    ] )
