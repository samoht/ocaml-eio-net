(* Each option the table classifies, checked under each kernel against what a
   TCP connection does on the network. An unsupported option is refused by a
   connect; a stored-only one is taken, read back, and leaves the connection's
   trace as it was; a modelled or approximated one changes that trace.
   Behaviours are checked by the kernel's traces and the network's own
   tests. *)

module O = Eio.Net.Sockopt

(* IPV6_ONLY needs an IPv6 socket, and a bound one refuses it with EINVAL, as
   on Linux: it is set on the client only, over ::1. *)
let v6_only (Scope.Sample (o, _)) =
  match o with O.IPV6_ONLY -> true | _ -> false

let addr l =
  let ip =
    if List.exists v6_only l then Eio.Net.Ipaddr.V6.loopback
    else Eio.Net.Ipaddr.V4.loopback
  in
  `Tcp (ip, 8200)

(* Every run turns keepalive on, so that the options tuning it have an effect
   to show; its own sample turns it off. *)
let base = [ Scope.Sample (O.SO_KEEPALIVE, true) ]

let settings l =
  List.fold_right (fun (Scope.Sample (o, v)) acc -> O.((o, v) :: acc)) l O.[]

let secs clock =
  Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) /. 1e9

(* [f ()]'s result or exception, and the time it came at, or [waits] if it
   takes [limit] seconds. *)
let outcome ?(limit = 10_800.) clock f =
  match Eio.Time.Timeout.(run_exn (seconds clock limit)) f with
  | v -> Fmt.str "%s at %.3f s" v (secs clock)
  | exception Eio.Time.Timeout -> Fmt.str "waits %.0f s" limit
  | exception End_of_file -> Fmt.str "end of file at %.3f s" (secs clock)
  | exception (Eio.Io _ as e) -> Fmt.str "%a at %.3f s" Fmt.exn e (secs clock)

let read flow () =
  let buf = Cstruct.create 1_048_576 in
  Fmt.str "reads %d" (Eio.Flow.single_read flow buf)

let write flow n () =
  Eio.Flow.write flow [ Cstruct.create n ];
  Fmt.str "writes %d" n

(* How many bytes the client queues, in writes of 4 KiB, before one waits. *)
let fill clock flow =
  let chunk = Cstruct.create 4096 in
  let rec go n =
    match
      Eio.Time.Timeout.(run_exn (seconds clock 1.)) @@ fun () ->
      Eio.Flow.single_write flow [ chunk ]
    with
    | k -> go (n + k)
    | exception Eio.Time.Timeout -> n
  in
  Fmt.str "queues %d" (go 0)

(* The earliest whole second a listener can bind the address again. *)
let free clock net addr =
  let rec go () =
    match
      Eio.Switch.run @@ fun sw -> Eio.Net.listen ~sw ~backlog:1 net addr
    with
    | _ -> Fmt.str "free at %.0f s" (secs clock)
    | exception
        Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _) ->
        if secs clock >= 300. then "held"
        else (
          Eio.Time.Mono.sleep clock 1.;
          go ())
  in
  go ()

(* A connection made with [l] set on the client and on the listener, and the
   lines [probe] observes of it. *)
let session ~os l probe =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#mono_clock in
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~os ~clock ~sw () in
  let addr = addr l in
  let listener = Eio.Net.listen ~sw ~backlog:1 net addr in
  List.iter
    (fun (Scope.Sample (o, v) as s) ->
      if not (v6_only s) then Eio.Net.setsockopt listener o v)
    l;
  let client = Eio.Net.connect ~sw ~options:(settings l) net addr in
  match
    Eio.Time.Timeout.(run_exn (seconds clock 60.)) @@ fun () ->
    Eio.Net.accept ~sw listener
  with
  | exception Eio.Time.Timeout -> [ "no accept in 60 s" ]
  | server, _ ->
      let accepted = Fmt.str "accepted at %.3f s" (secs clock) in
      accepted :: probe ~clock ~net ~addr ~listener ~client ~server

let exchange ~clock ~net:_ ~addr:_ ~listener:_ ~client ~server =
  let w1 = outcome clock (write server 5) in
  let r1 = outcome ~limit:10. clock (read client) in
  let w2 = outcome clock (write client 3000) in
  let r2 = outcome ~limit:10. clock (read server) in
  [ w1; r1; w2; r2 ]

let queue ~clock ~net:_ ~addr:_ ~listener:_ ~client ~server =
  let q = fill clock client in
  [ q; outcome ~limit:10. clock (read server) ]

let idle_silence ~clock ~net:_ ~addr:_ ~listener:_ ~client ~server:_ =
  Eio_net_mem.Private.silence client;
  [ outcome clock (read client) ]

let busy_silence ~clock ~net:_ ~addr:_ ~listener:_ ~client ~server:_ =
  Eio_net_mem.Private.silence client;
  let w = outcome clock (write client 1) in
  [ w; outcome clock (read client) ]

let close_first ~clock ~net ~addr ~listener ~client ~server =
  Eio.Resource.close listener;
  let w = outcome clock (write server 5) in
  Eio.Resource.close server;
  let closed = Fmt.str "closed at %.3f s" (secs clock) in
  let r1 = outcome ~limit:10. clock (read client) in
  let r2 = outcome ~limit:10. clock (read client) in
  [ w; closed; r1; r2; free clock net addr ]

let probes =
  [
    ("exchange", exchange);
    ("queue", queue);
    ("idle silence", idle_silence);
    ("busy silence", busy_silence);
    ("close first", close_first);
  ]

let trace ~os l =
  List.concat_map
    (fun (name, probe) ->
      List.map (fun line -> name ^ ": " ^ line) (session ~os l probe))
    probes

let refused ~os (Scope.Sample (o, v) as sample) =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~os ~sw () in
  let addr = addr [ sample ] in
  let _listener = Eio.Net.listen ~sw ~backlog:1 net addr in
  match Eio.Net.connect ~sw ~options:O.[ (o, v) ] net addr with
  | flow ->
      ignore (Eio.Net.getsockopt flow o);
      false
  | exception Eio.Io _ -> true
  | exception Invalid_argument _ -> true

let pp_trace = Fmt.(list ~sep:cut string)

(* The lines of [a] not in [b], marked [-], then those of [b] not in [a],
   marked [+]. *)
let pp_diff ppf (a, b) =
  let only x y = List.filter (fun l -> not (List.mem l y)) x in
  Fmt.pf ppf "%a"
    Fmt.(list ~sep:cut string)
    (List.map (( ^ ) "- ") (only a b) @ List.map (( ^ ) "+ ") (only b a))

(* What is wrong with the claim that [sample] is of class [cls], if
   anything. *)
let wrong ~os cls sample =
  match cls with
  | Scope.Unsupported _ -> if refused ~os sample then None else Some "is taken"
  | Stored_only _ | Modelled | Approximated _ -> (
      if refused ~os sample then Some "is refused"
      else
        let without = trace ~os base
        and with_ = trace ~os (base @ [ sample ]) in
        match cls with
        | Stored_only _ when without <> with_ ->
            Fmt.kstr Option.some "@[<v>changes what a socket does:@,%a@]"
              pp_diff (without, with_)
        | (Modelled | Approximated _) when without = with_ ->
            Fmt.kstr Option.some "@[<v>changes nothing:@,%a@]" pp_trace without
        | _ -> None)

let test_options () =
  let errors =
    List.concat_map
      (fun (f : Scope.feature) ->
        List.concat_map
          (fun sample ->
            List.filter_map
              (fun (os, kernel, cls) ->
                Option.map
                  (Fmt.str "%s (%s) %s" f.name kernel)
                  (wrong ~os cls sample))
              [ (`Linux, "Linux", f.linux); (`Macos, "macOS", f.macos) ])
          f.samples)
      Scope.options
  in
  if errors <> [] then
    Alcotest.failf "@[<v>%a@]" Fmt.(list ~sep:(any "@,@,") string) errors

(* The options of Eio.Net.Sockopt and Eio_unix.Net a socket takes, each of
   which needs a sample. *)
let settable =
  [
    "SO_DEBUG";
    "SO_BROADCAST";
    "SO_REUSEADDR";
    "SO_KEEPALIVE";
    "SO_DONTROUTE";
    "SO_OOBINLINE";
    "TCP_NODELAY";
    "IPV6_ONLY";
    "SO_REUSEPORT";
    "SO_SNDBUF";
    "SO_RCVBUF";
    "SO_RCVLOWAT";
    "SO_SNDLOWAT";
    "SO_LINGER";
    "SO_RCVTIMEO";
    "SO_SNDTIMEO";
    "TCP_KEEPIDLE";
    "TCP_KEEPINTVL";
    "TCP_KEEPCNT";
    "TCP_CONGESTION";
    "TCP_CORK";
    "TCP_USER_TIMEOUT";
    "TCP_MAXSEG";
    "TCP_LINGER2";
    "TCP_DEFER_ACCEPT";
    "TCP_SYNCNT";
    "TCP_WINDOW_CLAMP";
    "TCP_QUICKACK";
    "TCP_FASTOPEN";
    "IP_FREEBIND";
    "IP_BIND_ADDRESS_NO_PORT";
    "IP_LOCAL_PORT_RANGE";
    "IP_TTL";
    "IP_MTU_DISCOVER";
  ]

(* The options a socket only reports. *)
let read_only = [ "IP_MTU"; "SO_ACCEPTCONN"; "SO_ERROR"; "SO_TYPE" ]

(* The arguments of listen and datagram_socket that stand for an option. *)
let arguments = [ "reuse_addr"; "reuse_port" ]

let mentions name o =
  let o = "`" ^ o ^ "`" in
  let n = String.length o in
  let rec at i =
    i + n <= String.length name && (String.sub name i n = o || at (i + 1))
  in
  at 0

let test_complete () =
  List.iter
    (fun o ->
      if
        not
          (List.exists
             (fun (f : Scope.feature) -> mentions f.name o)
             Scope.options)
      then Alcotest.failf "no row names %s" o)
    (settable @ read_only @ arguments);
  List.iter
    (fun (f : Scope.feature) ->
      let n = List.length (List.filter (mentions f.name) settable) in
      if List.length f.samples <> n then
        Alcotest.failf "%s names %d settable options and has %d samples" f.name
          n (List.length f.samples))
    Scope.options

let suite =
  ( "scope",
    [
      Alcotest.test_case "every option has a row and samples" `Quick
        test_complete;
      Alcotest.test_case "each option's class holds" `Quick test_options;
    ] )
