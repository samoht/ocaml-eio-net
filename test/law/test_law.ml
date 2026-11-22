(* Each row of the law, checked against what the operation does through a
   wrapper over a spy over eio-net.mem: a forwarded operation calls its base
   once and asks about no fault, an intercepted one calls its base once and
   asks once, a synthesized one never calls its base's function of that name,
   and an unsupported one is refused. The faults an intercepted row lists, and
   only those, change what a run of it does, and closing every socket a run
   holds closes every socket the wrapper made under it. *)

module O = Eio.Net.Sockopt

type stream = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
type datagram = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t
type listener = [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t

let family_name = function `Inet -> "TCP" | `Unix -> "AF_UNIX"

let stream_addr family n : Eio.Net.Sockaddr.stream =
  match family with
  | `Inet -> `Tcp (Eio.Net.Ipaddr.V4.loopback, 8000 + n)
  | `Unix -> `Unix (Fmt.str "/law-%d.sock" n)

let dgram_addr family n : Eio.Net.Sockaddr.datagram =
  match family with
  | `Inet -> `Udp (Eio.Net.Ipaddr.V4.loopback, 9000 + n)
  | `Unix -> `Unix (Fmt.str "/law-dgram-%d.sock" n)

let constructor : Fault.t -> int = function
  | Short _ -> 0
  | Stall _ -> 1
  | Hang -> 2
  | Reset -> 3
  | Eof -> 4
  | Refuse -> 5
  | Timeout -> 6
  | Unreachable -> 7
  | No_descriptor -> 8
  | Drop -> 9
  | Dup -> 10
  | Reorder -> 11
  | Garble _ -> 12
  | Cancel _ -> 13

let outcome f =
  match f () with
  | s -> s
  | exception Exit -> "-"
  | exception End_of_file -> "end of file"
  | exception Invalid_argument _ -> "Invalid_argument"
  | exception Eio.Time.Timeout -> "blocks"
  | exception Eio.Io (e, _) -> Fmt.str "%a" Eio.Exn.pp_err e

let ok f () =
  f ();
  "ok"

let read flow () =
  let buf = Cstruct.create 16 in
  let n = Eio.Flow.single_read flow buf in
  Cstruct.to_string ~len:n buf

let write flow s () =
  string_of_int (Eio.Flow.single_write flow [ Cstruct.of_string s ])

let recv sock () =
  let buf = Cstruct.create 16 in
  let _, n = Eio.Net.recv sock buf in
  Cstruct.to_string ~len:n buf

(* A datagram socket's shutdown, which Eio.Flow.shutdown's type does not
   take. *)
let shutdown_of (Eio.Resource.T (t, ops) : datagram) cmd =
  let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
  X.shutdown t cmd

let refused b () = if b () then "refused" else "offered"

(* The wrapper with a fault at the start of each operation [f] fails. *)
let wrapped ?os ~clock ~outage f net =
  Wrapper.v ?os ~clock ~outage
    (fun op -> Option.map (fun f -> (f, 0)) (f op))
    net

(* --- Classes ------------------------------------------------------------- *)

(* A connection with "hello" queued to its server, and a datagram queued to
   [rx], on [net]. *)
type fixture = {
  sw : Eio.Switch.t;
  family : [ `Inet | `Unix ];
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  listener : listener;
  client : stream;
  server : stream;
  rx : datagram;
  tx : datagram;
}

let fixture ~sw ~family net =
  let listener = Eio.Net.listen ~sw ~backlog:5 net (stream_addr family 0) in
  let client = Eio.Net.connect ~sw net (stream_addr family 0) in
  let server, _ = Eio.Net.accept ~sw listener in
  Eio.Flow.copy_string "hello" client;
  let rx = Eio.Net.datagram_socket ~sw net (dgram_addr family 0) in
  let tx = Eio.Net.datagram_socket ~sw net (dgram_addr family 1) in
  Eio.Net.send tx ~dst:(dgram_addr family 0) [ Cstruct.of_string "x" ];
  { sw; family; net; listener; client; server; rx; tx }

(* [drive op fx] readies [fx] for [op] and is a run of [op] alone. *)
let drive : type c. c Law.t -> fixture -> unit -> string =
 fun op fx ->
  let sw = fx.sw in
  match op with
  | Law.Connect ->
      ok (fun () ->
          ignore (Eio.Net.connect ~sw fx.net (stream_addr fx.family 0)))
  | Listen ->
      ok (fun () ->
          ignore
            (Eio.Net.listen ~sw ~backlog:1 fx.net (stream_addr fx.family 1)))
  | Datagram_socket ->
      ok (fun () ->
          ignore (Eio.Net.datagram_socket ~sw fx.net (dgram_addr fx.family 2)))
  | Getaddrinfo ->
      fun () ->
        Fmt.str "%d addresses"
          (List.length (Eio.Net.getaddrinfo fx.net "127.0.0.1"))
  | Getnameinfo ->
      fun () ->
        fst (Eio.Net.getnameinfo fx.net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 80)))
  | Outage -> fun () -> string_of_bool (Wrapper.is_outage fx.net)
  | Accept ->
      ignore (Eio.Net.connect ~sw fx.net (stream_addr fx.family 0));
      ok (fun () -> ignore (Eio.Net.accept ~sw fx.listener))
  | Listening_addr ->
      fun () ->
        Fmt.str "%a" Eio.Net.Sockaddr.pp (Eio.Net.listening_addr fx.listener)
  | Listener_setsockopt ->
      ok (fun () -> Eio.Net.setsockopt fx.listener O.SO_REUSEADDR true)
  | Listener_getsockopt ->
      fun () -> string_of_bool (Eio.Net.getsockopt fx.listener O.SO_REUSEADDR)
  | Listener_close -> ok (fun () -> Eio.Resource.close fx.listener)
  | Listener_fd ->
      refused (fun () -> Option.is_none (Eio_unix.Resource.fd_opt fx.listener))
  | Read_methods ->
      fun () ->
        let (Eio.Resource.T (_, ops)) = fx.client in
        let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
        Fmt.str "%d read methods" (List.length X.read_methods)
  | Single_read -> read fx.server
  | Single_write -> write fx.client "ab"
  | Copy -> ok (fun () -> Eio.Flow.copy_string "xyz" fx.client)
  | Shutdown -> ok (fun () -> Eio.Flow.shutdown fx.client `Send)
  | Setsockopt ->
      ok (fun () -> Eio.Net.setsockopt fx.client O.SO_KEEPALIVE true)
  | Getsockopt ->
      fun () -> string_of_bool (Eio.Net.getsockopt fx.client O.SO_KEEPALIVE)
  | Close -> ok (fun () -> Eio.Resource.close fx.client)
  | Fd ->
      refused (fun () -> Option.is_none (Eio_unix.Resource.fd_opt fx.client))
  | Connection ->
      refused (fun () ->
          Option.is_none (Eio_net_mem.Private.connection_id fx.client))
  | Silence ->
      refused (fun () ->
          match Eio_net_mem.Private.silence fx.client with
          | () -> false
          | exception Invalid_argument _ -> true)
  | Send ->
      ok (fun () ->
          Eio.Net.send fx.tx ~dst:(dgram_addr fx.family 0)
            [ Cstruct.of_string "y" ])
  | Recv -> recv fx.rx
  | Datagram_shutdown -> ok (fun () -> shutdown_of fx.rx `Receive)
  | Datagram_setsockopt ->
      ok (fun () -> Eio.Net.setsockopt fx.rx O.SO_BROADCAST true)
  | Datagram_getsockopt ->
      fun () -> string_of_bool (Eio.Net.getsockopt fx.rx O.SO_BROADCAST)
  | Datagram_close -> ok (fun () -> Eio.Resource.close fx.rx)
  | Datagram_fd ->
      refused (fun () -> Option.is_none (Eio_unix.Resource.fd_opt fx.rx))

(* What [op] answers, the calls it makes of the spy and the kinds of the
   operations it asks about, with or without a wrapper. *)
let observe ~wrap ~family op =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let spy, base = Spy.v (Eio_net_mem.v ~clock:env#mono_clock ~sw ()) in
  let asked = ref [] in
  let net =
    if wrap then
      wrapped ~clock:env#clock ~outage:false
        (fun (o : Op.t) ->
          asked := o.kind :: !asked;
          None)
        base
    else base
  in
  let run = drive op (fixture ~sw ~family net) in
  Spy.clear spy;
  asked := [];
  let answer = outcome run in
  (answer, Spy.calls spy, List.rev !asked)

let pp_call ppf ((i : Law.interface), name) =
  Fmt.pf ppf "%s of %s" name
    (match i with
    | `Network -> "a network"
    | `Listening_socket -> "a listener"
    | `Stream_socket -> "a stream"
    | `Datagram_socket -> "a datagram socket")

let pp_calls = Fmt.(Dump.list pp_call)
let pp_kinds = Fmt.(Dump.list (using Op.kind_name string))

(* What is wrong with [op]'s row under [family], if anything. *)
let wrong ~family (Law.Op op) =
  let me = (Law.interface op, Law.name op) in
  let answer, calls, asked = observe ~wrap:true ~family op in
  let bare () =
    let a, _, _ = observe ~wrap:false ~family op in
    if a = answer then None
    else Fmt.kstr Option.some "answers %S where its base answers %S" answer a
  in
  let calls_once kinds =
    if not (List.equal Spy.equal_call calls [ me ]) then
      Fmt.kstr Option.some "calls %a of its base" pp_calls calls
    else if not (List.equal Op.equal_kind asked kinds) then
      Fmt.kstr Option.some "asks about %a" pp_kinds asked
    else bare ()
  in
  match Law.class_ op with
  | Forwarded -> calls_once []
  | Intercepted i -> calls_once [ i.kind ]
  | Synthesized ->
      if List.mem me calls then Some "calls its base's own" else None
  | Unsupported -> if answer = "refused" then None else Some ("is " ^ answer)

let test_classes () =
  let errors =
    List.concat_map
      (fun family ->
        List.filter_map
          (fun (Law.Op o as op) ->
            Option.map
              (Fmt.str "%a (%s) %s" Law.pp o (family_name family))
              (wrong ~family op))
          Law.all)
      [ `Inet; `Unix ]
  in
  if errors <> [] then
    Alcotest.failf "@[<v>%a@]" Fmt.(list ~sep:cut string) errors

(* --- Faults -------------------------------------------------------------- *)

(* [nth kind fault] injects [fault] on the first operation of [kind]. *)
let first kind fault =
  let seen = ref false in
  fun (op : Op.t) ->
    if Op.equal_kind op.kind kind && not !seen then begin
      seen := true;
      fault
    end
    else None

let need r = match !r with Some x -> x | None -> raise Exit

let set r f () =
  r := Some (f ());
  "ok"

(* A run of [op] on [net], each step with its outcome and the time it ended
   at, and the sockets it holds. *)
let steps : Law.intercepted Law.t -> _ =
 fun op ~sw ~family net ->
  let l = ref None and c = ref None and s = ref None in
  let rx = ref None and tx = ref None in
  let addr = stream_addr family 0 in
  let listen () = Eio.Net.listen ~sw ~backlog:5 net addr in
  let connect () = Eio.Net.connect ~sw net addr in
  let accept () = fst (Eio.Net.accept ~sw (need l)) in
  let r x () = read (need x) () and w x m () = write (need x) m () in
  let steps =
    match op with
    | Law.Connect ->
        [
          ("listen", set l listen);
          ("connect", set c connect);
          ("accept", set s accept);
          ("c write", w c "hi");
          ("s read", r s);
          ("c read", r c);
        ]
    | Accept ->
        [
          ("listen", set l listen);
          ("connect", set c connect);
          ("c write", w c "hi");
          ("accept", set s accept);
          ("s read", r s);
          ("s write", w s "yo");
          ("c read", r c);
          ("s read", r s);
        ]
    | Single_read ->
        [
          ("listen", set l listen);
          ("connect", set c connect);
          ("accept", set s accept);
          ("c write", w c "hello");
          ("s read", r s);
          ("s read", r s);
          ("s write", w s "xy");
          ("c read", r c);
          ("c read", r c);
        ]
    | Single_write ->
        [
          ("listen", set l listen);
          ("connect", set c connect);
          ("accept", set s accept);
          ("c write", w c "hello");
          ("s read", r s);
          ("s read", r s);
          ("c write", w c "z");
          ("s read", r s);
        ]
    | Recv ->
        let send m () =
          Eio.Net.send (need tx) ~dst:(dgram_addr family 0)
            [ Cstruct.of_string m ];
          "ok"
        in
        let recv () = recv (need rx) () in
        [
          ( "rx",
            set rx (fun () ->
                Eio.Net.datagram_socket ~sw net (dgram_addr family 0)) );
          ( "tx",
            set tx (fun () ->
                Eio.Net.datagram_socket ~sw net (dgram_addr family 1)) );
          ("send a", send "a");
          ("send b", send "b");
          ("send c", send "c");
          ("recv", recv);
          ("recv", recv);
          ("recv", recv);
          ("recv", recv);
        ]
  in
  let close r = Option.iter Eio.Resource.close !r in
  let close_all () =
    close c;
    close s;
    close l;
    close rx;
    close tx
  in
  (steps, close_all)

let timed clock (name, f) =
  let r = outcome (fun () -> Eio.Time.with_timeout_exn clock 300. f) in
  Fmt.str "%s: %s at %g" name r (Eio.Time.now clock)

(* What a run of [op] does with [fault] on its first operation, on
   eio-net.mem. *)
let run op ~family fault =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  Eio.Switch.run @@ fun sw ->
  let net =
    wrapped ~clock ~outage:false
      (first (Law.kind op) fault)
      (Eio_net_mem.v ~clock:env#mono_clock ~sw ())
  in
  let steps, _ = steps op ~sw ~family net in
  List.map (timed clock) steps

(* The sockets the wrapper made under a run of [op] with [fault] and leaves
   open once the run closed every socket it holds. *)
let left_open op ~family fault =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  Eio.Switch.run @@ fun sw ->
  let spy, base = Spy.v (Eio_net_mem.v ~clock:env#mono_clock ~sw ()) in
  let net = wrapped ~clock ~outage:false (first (Law.kind op) fault) base in
  let steps, close_all = steps op ~sw ~family net in
  List.iter (fun s -> ignore (timed clock s)) steps;
  close_all ();
  Spy.opened spy

let intercepted : Law.intercepted Law.t list =
  [ Connect; Accept; Single_read; Single_write; Recv ]

let listed op family =
  let (Law.Intercepted i) = Law.class_ op in
  match family with `Inet -> i.inet | `Unix -> i.unix

let test_faults () =
  let errors =
    List.concat_map
      (fun family ->
        List.concat_map
          (fun op ->
            let listed = listed op family in
            let base = run op ~family None in
            let where = Fmt.str "%a (%s)" Law.pp op (family_name family) in
            List.filter_map
              (fun fault ->
                let got = run op ~family (Some fault) in
                let changes = got <> base
                and expected = List.mem fault listed in
                if changes = expected then None
                else
                  Fmt.kstr Option.some "@[<v>%s: %a %s:@,%a@]" where Fault.pp
                    fault
                    (if changes then "changes it, not listed"
                     else "changes nothing")
                    Fmt.(list ~sep:cut string)
                    got)
              Faults.all)
          intercepted)
      [ `Inet; `Unix ]
  in
  if errors <> [] then
    Alcotest.failf "@[<v>%a@]" Fmt.(list ~sep:(any "@,@,") string) errors

let test_released () =
  let errors =
    List.concat_map
      (fun family ->
        List.concat_map
          (fun op ->
            List.filter_map
              (fun fault ->
                match left_open op ~family fault with
                | [] -> None
                | l ->
                    Fmt.kstr Option.some "%a (%s) under %a leaves %a open"
                      Law.pp op (family_name family)
                      Fmt.(option ~none:(any "no fault") Fault.pp)
                      fault
                      Fmt.(list ~sep:comma string)
                      l)
              (None :: List.map Option.some (listed op family)))
          intercepted)
      [ `Inet; `Unix ]
  in
  if errors <> [] then
    Alcotest.failf "@[<v>%a@]" Fmt.(list ~sep:cut string) errors

(* --- Coverage ------------------------------------------------------------ *)

let test_complete () =
  let rows =
    List.map (fun (Law.Op o) -> (Law.interface o, Law.name o)) Law.all
  in
  let uniq = List.sort_uniq compare rows in
  if List.length uniq <> List.length rows then
    Alcotest.fail "two rows name the same operation";
  List.iter
    (fun call ->
      if not (List.mem call rows) then
        Alcotest.failf "no row for %a" pp_call call)
    Spy.operations;
  List.iter
    (fun ((_, name) as call) ->
      let eio = String.starts_with ~prefix:"Eio" name in
      let own =
        not
          (String.starts_with ~prefix:"Eio." name
          || String.starts_with ~prefix:"Eio_unix." name)
      in
      if eio && (not own) && not (List.mem call Spy.operations) then
        Alcotest.failf "%a is no operation of Eio" pp_call call)
    rows;
  let ctors = List.sort_uniq compare (List.map constructor Faults.all) in
  Alcotest.(check int) "every fault" 14 (List.length ctors);
  let rows =
    List.filter
      (fun (Law.Op o) ->
        match Law.class_ o with Law.Intercepted _ -> true | _ -> false)
      Law.all
  in
  Alcotest.(check int)
    "every intercepted operation is run" (List.length rows)
    (List.length intercepted);
  List.iter
    (fun op ->
      List.iter
        (fun f ->
          if not (List.mem f Faults.all) then
            Alcotest.failf "%a lists %a, which the test does not try" Law.pp op
              Fault.pp f)
        (listed op `Inet @ listed op `Unix))
    intercepted

let suite =
  ( "law",
    [
      Alcotest.test_case "every operation has a row" `Quick test_complete;
      Alcotest.test_case "each row's class holds" `Quick test_classes;
      Alcotest.test_case "the listed faults, and only those, change it" `Quick
        test_faults;
      Alcotest.test_case "a fault leaves no socket open" `Quick test_released;
    ] )
