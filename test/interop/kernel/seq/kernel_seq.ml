(* The operations and their notation follow the exhaustive sequences of the
   review that found eio-net.mem's divergences from Linux. *)

type transport = Tcp4 | Tcp6 | Unix_stream | Udp4 | Udp6 | Unix_dgram

let transports = [ Tcp4; Tcp6; Unix_stream; Udp4; Udp6; Unix_dgram ]

let transport_name = function
  | Tcp4 -> "tcp4"
  | Tcp6 -> "tcp6"
  | Unix_stream -> "unix-stream"
  | Udp4 -> "udp4"
  | Udp6 -> "udp6"
  | Unix_dgram -> "unix-dgram"

let transport_of_string s =
  match List.find_opt (fun t -> transport_name t = s) transports with
  | Some t -> t
  | None -> invalid_arg ("Kernel_seq.transport_of_string: " ^ s)

let is_stream = function
  | Tcp4 | Tcp6 | Unix_stream -> true
  | Udp4 | Udp6 | Unix_dgram -> false

let sides = [ "C"; "S" ]

let stream_ops =
  [ "connect"; "accept"; "close_listener" ]
  @ List.concat_map
      (fun s ->
        [
          "write " ^ s ^ " 0";
          "write " ^ s ^ " 2";
          "read " ^ s ^ " 1";
          "read " ^ s ^ " 4";
          "shutdown " ^ s ^ " send";
          "shutdown " ^ s ^ " recv";
          "shutdown " ^ s ^ " all";
          "close " ^ s;
        ])
      sides

let dgram_ops =
  List.concat_map
    (fun s ->
      [
        "send " ^ s ^ " 0";
        "send " ^ s ^ " 3";
        "send_nodst " ^ s;
        "read " ^ s ^ " 1";
        "read " ^ s ^ " 4";
        "shutdown " ^ s ^ " send";
        "shutdown " ^ s ^ " recv";
        "shutdown " ^ s ^ " all";
        "close " ^ s;
      ])
    sides

let words op = String.split_on_char ' ' op

(* A stream operation on an endpoint needs it; connect and accept happen
   once; the listener closes once. *)
let stream_valid ops =
  let rec go ~c ~s ~l = function
    | [] -> true
    | "connect" :: r -> (not c) && go ~c:true ~s ~l r
    | "accept" :: r -> (not s) && go ~c ~s:true ~l r
    | "close_listener" :: r -> l && go ~c ~s ~l:false r
    | op :: r -> (
        match words op with
        | _ :: "C" :: _ -> c && go ~c ~s ~l r
        | _ :: "S" :: _ -> s && go ~c ~s ~l r
        | _ -> false)
  in
  go ~c:false ~s:false ~l:true ops

let extend alphabet valid prefix depth =
  let acc = ref [] in
  let rec go rev_prefix k =
    acc := List.rev rev_prefix :: !acc;
    if k > 0 then
      List.iter
        (fun op ->
          let p = op :: rev_prefix in
          if valid (List.rev p) then go p (k - 1))
        alphabet
  in
  go (List.rev prefix) depth;
  !acc

(* Streams: every sequence of up to four operations, and up to three more
   after a connect and an accept, which the shutdown and close cases need.
   Datagrams: every sequence of up to three operations. *)
let sequences t =
  let all =
    if is_stream t then
      extend stream_ops stream_valid [] 4
      @ extend stream_ops stream_valid [ "connect"; "accept" ] 3
    else extend dgram_ops (fun _ -> true) [] 3
  in
  List.sort_uniq compare all

(* Outcomes *)

let errno : Unix.error -> string = function
  | EADDRINUSE -> "EADDRINUSE"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | ENAMETOOLONG -> "ENAMETOOLONG"
  | EDESTADDRREQ -> "EDESTADDRREQ"
  | EMSGSIZE -> "EMSGSIZE"
  | EINVAL -> "EINVAL"
  | ENETUNREACH -> "ENETUNREACH"
  | EAFNOSUPPORT -> "EAFNOSUPPORT"
  | EPROTOTYPE -> "EPROTOTYPE"
  | ENOTCONN -> "ENOTCONN"
  | EPIPE -> "EPIPE"
  | ECONNRESET -> "ECONNRESET"
  | ECONNREFUSED -> "ECONNREFUSED"
  | ENOENT -> "ENOENT"
  | EBADF -> "EBADF"
  | EISCONN -> "EISCONN"
  | EOPNOTSUPP -> "EOPNOTSUPP"
  | ENOBUFS -> "ENOBUFS"
  | e -> Unix.error_message e

(* eio-net.mem's codes, named by the system error each stands for. *)
let mem_errno : Eio.Exn.Backend.t -> string option = function
  | Eio_net_mem.Address_in_use -> Some "EADDRINUSE"
  | Eio_net_mem.No_destination -> Some "EDESTADDRREQ"
  | Eio_net_mem.Not_connected -> Some "ENOTCONN"
  | Eio_net_mem.Message_too_long -> Some "EMSGSIZE"
  | Eio_net_mem.Invalid_port -> Some "EINVAL"
  | Eio_net_mem.Network_unreachable -> Some "ENETUNREACH"
  | Eio_net_mem.Address_family -> Some "EAFNOSUPPORT"
  | Eio_net_mem.Wrong_socket_type -> Some "EPROTOTYPE"
  | Eio_net_mem.Address_not_available -> Some "EADDRNOTAVAIL"
  | Eio_net_mem.Name_too_long -> Some "ENAMETOOLONG"
  | Eio_net_mem.Invalid_address -> Some "EINVAL"
  | _ -> None

let classify = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> "Reset"
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> "Refused"
  | Eio.Io (Eio.Net.E _, _) -> "Net"
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> "Not_found"
  | Eio.Io (Eio.Fs.E _, _) -> "Fs"
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (e, _, _)), _) -> errno e
  | Eio.Io (Eio.Exn.X b, _) -> (
      match mem_errno b with
      | Some e -> e
      | None -> Fmt.str "%a" Eio.Exn.Backend.pp b)
  | Unix.Unix_error (e, _, _) -> errno e
  | End_of_file -> "EOF"
  | Invalid_argument _ -> "Invalid_argument"
  | e -> Printexc.to_string e

exception Blocked

(* One attempt that does not wait: a real socket's backend tries the system
   call before it waits for readiness, and mem completes or waits at once. *)
let now f =
  match
    Eio.Fiber.first
      (fun () -> `Done (f ()))
      (fun () ->
        for _ = 1 to 64 do
          Eio.Fiber.yield ()
        done;
        `Blocked)
  with
  | `Done v -> v
  | `Blocked -> raise Blocked

let outcome f =
  try f () with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Blocked -> "blk"
  | e -> classify e

let shape (a : Eio.Net.Sockaddr.t) =
  match a with
  | `Tcp (ip, _) -> Fmt.str "tcp:%a" Eio.Net.Ipaddr.pp ip
  | `Udp (ip, _) -> Fmt.str "udp:%a" Eio.Net.Ipaddr.pp ip
  | `Unix p -> "unix:" ^ Filename.basename p

let unlink path = try Unix.unlink path with Unix.Unix_error _ -> ()

let int_arg op =
  match List.rev (words op) with n :: _ -> int_of_string n | [] -> 0

let side op = match words op with _ :: s :: _ -> s | _ -> ""

let shutdown_cmd op =
  match List.rev (words op) with
  | "send" :: _ -> `Send
  | "recv" :: _ -> `Receive
  | _ -> `All

let run_stream net t ops =
  let path = "s.sock" in
  unlink path;
  let addr : Eio.Net.Sockaddr.stream =
    match t with
    | Tcp4 -> `Tcp (Eio.Net.Ipaddr.V4.loopback, 0)
    | Tcp6 -> `Tcp (Eio.Net.Ipaddr.V6.loopback, 0)
    | _ -> `Unix path
  in
  let out = ref [] in
  (try
     Eio.Switch.run ~name:"sequence" @@ fun sw' ->
     let l = Eio.Net.listen ~sw:sw' ~backlog:8 ~reuse_addr:true net addr in
     let caddr = Eio.Net.listening_addr l in
     let c = ref None and s = ref None in
     let flow op = Option.get (if side op = "C" then !c else !s) in
     List.iter
       (fun op ->
         let r =
           outcome @@ fun () ->
           match words op with
           | [ "connect" ] ->
               c := Some (Eio.Net.connect ~sw:sw' net caddr);
               "ok"
           | [ "accept" ] ->
               let f, a = now (fun () -> Eio.Net.accept ~sw:sw' l) in
               s := Some f;
               "ok:" ^ shape (a :> Eio.Net.Sockaddr.t)
           | [ "close_listener" ] ->
               Eio.Resource.close l;
               "ok"
           | "write" :: _ ->
               string_of_int
                 (Eio.Flow.single_write (flow op)
                    [ Cstruct.create (int_arg op) ])
           | "read" :: _ ->
               now (fun () ->
                   let n =
                     Eio.Flow.single_read (flow op)
                       (Cstruct.create (int_arg op))
                   in
                   "r" ^ string_of_int n)
           | "shutdown" :: _ ->
               Eio.Flow.shutdown (flow op) (shutdown_cmd op);
               "ok"
           | "close" :: _ ->
               Eio.Flow.close (flow op);
               "ok"
           | _ -> invalid_arg op
         in
         out := r :: !out)
       ops
   with e -> out := ("abort " ^ classify e) :: !out);
  unlink path;
  List.rev !out

(* Eio has no shutdown for a datagram socket's type, though each backend
   implements one. *)
let dgram_shutdown (sock : _ Eio.Net.datagram_socket) cmd =
  let (Eio.Resource.T (t, ops)) = sock in
  let module S = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
  S.shutdown t cmd

let run_dgram net t ops =
  let pa = "a.sock" and pb = "b.sock" in
  unlink pa;
  unlink pb;
  let out = ref [] in
  (try
     Eio.Switch.run ~name:"sequence" @@ fun sw' ->
     let ip =
       match t with
       | Udp6 -> Eio.Net.Ipaddr.V6.loopback
       | _ -> Eio.Net.Ipaddr.V4.loopback
     in
     let bind p : Eio.Net.Sockaddr.datagram =
       match t with Udp4 | Udp6 -> `Udp (ip, 0) | _ -> `Unix p
     in
     let a = Eio.Net.datagram_socket ~sw:sw' net (bind pa) in
     let b = Eio.Net.datagram_socket ~sw:sw' net (bind pb) in
     (* A real socket is asked for its port; mem binds the lowest free ones. *)
     let addr_of sock p fallback : Eio.Net.Sockaddr.datagram =
       match (t, Eio_unix.Resource.fd_opt sock) with
       | (Udp4 | Udp6), Some fd ->
           Eio_unix.Fd.use_exn "getsockname" fd (fun u ->
               match Unix.getsockname u with
               | Unix.ADDR_INET (_, port) -> `Udp (ip, port)
               | Unix.ADDR_UNIX _ -> `Unix p)
       | (Udp4 | Udp6), None -> `Udp (ip, fallback)
       | _ -> `Unix p
     in
     let aa = addr_of a pa 49152 and ba = addr_of b pb 49153 in
     let sock op = if side op = "C" then (a, ba) else (b, aa) in
     List.iter
       (fun op ->
         let r =
           outcome @@ fun () ->
           let s, dst = sock op in
           match words op with
           | "send" :: _ ->
               Eio.Net.send s ~dst [ Cstruct.create (int_arg op) ];
               "ok"
           | "send_nodst" :: _ ->
               Eio.Net.send s [ Cstruct.create 1 ];
               "ok"
           | "read" :: _ ->
               now (fun () ->
                   let src, n = Eio.Net.recv s (Cstruct.create (int_arg op)) in
                   Fmt.str "r%d:%s" n (shape (src :> Eio.Net.Sockaddr.t)))
           | "shutdown" :: _ ->
               dgram_shutdown s (shutdown_cmd op);
               "ok"
           | "close" :: _ ->
               Eio.Resource.close s;
               "ok"
           | _ -> invalid_arg op
         in
         out := r :: !out)
       ops
   with e -> out := ("abort " ^ classify e) :: !out);
  unlink pa;
  unlink pb;
  List.rev !out

let run net t ops =
  if is_stream t then run_stream net t ops else run_dgram net t ops

(* Scenarios: the address, backlog, reuse and resolution cases the
   sequences above do not reach, each run on a fresh network and printed as
   one line. *)

type net = [ `Generic ] Eio.Net.ty Eio.Resource.t

let ip s = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string s)
let ip_s a = Fmt.str "%a" Eio.Net.Ipaddr.pp a

let port_of l =
  match Eio.Net.listening_addr l with `Tcp (_, p) -> p | `Unix _ -> 0

let read_s flow n =
  now (fun () ->
      let buf = Cstruct.create n in
      let got = Eio.Flow.single_read flow buf in
      Cstruct.to_string ~len:got buf)

let lookup f =
  match f () with
  | v -> v
  | exception (Eio.Io (Eio.Net.E (Address_lookup_failed _), _) as e) ->
      (* "Net Address_lookup_failed NONAME (...)": its tag. *)
      let words = String.split_on_char ' ' (Printexc.to_string e) in
      let rec tag = function
        | "Address_lookup_failed" :: t :: _ -> t
        | _ :: r -> tag r
        | [] -> "lookup failed"
      in
      tag words

(* Three clients of one listener: accepts in connect order, one connection's
   close leaves the others alone, and the listener's close resets the
   connection it queued and refuses later connects. *)
let three net =
  Eio.Switch.run @@ fun sw ->
  let l =
    Eio.Net.listen ~sw ~backlog:8 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let addr = Eio.Net.listening_addr l in
  let connect () = Eio.Net.connect ~sw net addr in
  let c1 = connect () and c2 = connect () and c3 = connect () in
  Eio.Flow.copy_string "1" c1;
  Eio.Flow.copy_string "2" c2;
  Eio.Flow.copy_string "3" c3;
  Eio.Flow.close c2;
  let accept () = fst (now (fun () -> Eio.Net.accept ~sw l)) in
  let s1 = accept () in
  let s2 = accept () in
  let s3 = accept () in
  let o f = outcome f in
  let order = o (fun () -> read_s s1 16 ^ read_s s2 16 ^ read_s s3 16) in
  let s2_eof = o (fun () -> read_s s2 16) in
  let s1_blk = o (fun () -> read_s s1 16) in
  Eio.Flow.close s1;
  let c1_eof = o (fun () -> read_s c1 16) in
  Eio.Flow.copy_string "x" c3;
  let s3_x = o (fun () -> read_s s3 16) in
  let c4 = connect () in
  Eio.Flow.copy_string "4" c4;
  Eio.Resource.close l;
  let w () = string_of_int (Eio.Flow.single_write c4 [ Cstruct.create 1 ]) in
  let c4s =
    List.map o [ (fun () -> read_s c4 16); w; w; (fun () -> read_s c4 16) ]
  in
  let refused =
    o (fun () ->
        ignore (connect ());
        "ok")
  in
  let s3_w =
    o (fun () -> string_of_int (Eio.Flow.single_write s3 [ Cstruct.create 1 ]))
  in
  let c3_r = o (fun () -> "r" ^ string_of_int (String.length (read_s c3 16))) in
  String.concat " "
    ([ order; s2_eof; s1_blk; c1_eof; s3_x ] @ c4s @ [ refused; s3_w; c3_r ])

(* Connects that no accept takes: Linux queues one more than the backlog. *)
let backlog n net =
  Eio.Switch.run @@ fun sw ->
  let l =
    Eio.Net.listen ~sw ~backlog:n net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let addr = Eio.Net.listening_addr l in
  let connect () =
    outcome (fun () ->
        ignore (now (fun () -> Eio.Net.connect ~sw net addr));
        "ok")
  in
  let connects = List.init (n + 3) (fun _ -> connect ()) in
  let rec drain k =
    match now (fun () -> Eio.Net.accept ~sw l) with
    | _ -> drain (k + 1)
    | exception Blocked -> k
  in
  let accepted = drain 0 in
  Fmt.str "%s accepted=%d then %s"
    (String.concat " " connects)
    accepted (connect ())

let listen_at ?(reuse_addr = false) net addr =
  outcome (fun () ->
      Eio.Switch.run @@ fun sw ->
      ignore (Eio.Net.listen ~sw ~reuse_addr ~backlog:1 net addr);
      "ok")

(* A connect to [dst] bound at [bind], from a listener at [at]: its outcome
   and the peer address the accept sees. *)
let connect_case at dst bind net =
  Eio.Switch.run @@ fun sw ->
  match Eio.Net.listen ~sw ~backlog:8 net (`Tcp (ip at, 0)) with
  | exception e -> "listen " ^ classify e
  | l ->
      let bind_to = Option.map (fun b -> `Tcp (ip b, 0)) bind in
      outcome (fun () ->
          ignore (Eio.Net.connect ~sw ?bind_to net (`Tcp (ip dst, port_of l)));
          let _, peer = now (fun () -> Eio.Net.accept ~sw l) in
          "ok " ^ shape (peer :> Eio.Net.Sockaddr.t))

let udp_port sock fallback =
  match Eio_unix.Resource.fd_opt sock with
  | Some fd ->
      Eio_unix.Fd.use_exn "getsockname" fd (fun u ->
          match Unix.getsockname u with
          | Unix.ADDR_INET (_, port) -> port
          | Unix.ADDR_UNIX _ -> fallback)
  | None -> fallback

(* A datagram from a socket at [src] to [dst], at the port of a socket at
   [at]: the send's outcome, and what that socket receives. mem binds the
   lowest free ports. *)
let dgram_case at src dst net =
  Eio.Switch.run @@ fun sw ->
  let bind a : [ Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6 ] =
    match a with "UdpV4" -> `UdpV4 | "UdpV6" -> `UdpV6 | a -> `Udp (ip a, 0)
  in
  match Eio.Net.datagram_socket ~sw net (bind at) with
  | exception e -> "recv " ^ classify e
  | r -> (
      let port = udp_port r 49152 in
      match Eio.Net.datagram_socket ~sw net (bind src) with
      | exception e -> "send " ^ classify e
      | s ->
          let sent =
            outcome (fun () ->
                Eio.Net.send s ~dst:(`Udp (ip dst, port)) [ Cstruct.create 2 ];
                "sent")
          in
          let got =
            outcome (fun () ->
                let from, n =
                  now (fun () -> Eio.Net.recv r (Cstruct.create 4))
                in
                Fmt.str "%s n=%d" (shape (from :> Eio.Net.Sockaddr.t)) n)
          in
          sent ^ " " ^ got)

let gai name service net =
  lookup (fun () ->
      Eio.Net.getaddrinfo ~service net name
      |> List.map (fun (a : Eio.Net.Sockaddr.t) ->
          match a with
          | `Tcp (i, p) -> Fmt.str "tcp:%s:%d" (ip_s i) p
          | `Udp (i, p) -> Fmt.str "udp:%s:%d" (ip_s i) p
          | `Unix p -> "unix:" ^ p)
      |> String.concat " ")

let gni addr net =
  let host, service = Eio.Net.getnameinfo net addr in
  host ^ "/" ^ service

(* A port held after a connection closes, by the end that closed first. *)
let reuse which net =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let free () =
    Eio.Switch.run @@ fun sw ->
    port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0)))
  in
  let addr = `Tcp (lo, free ()) in
  let relisten a = listen_at net a ^ " " ^ listen_at ~reuse_addr:true net a in
  match which with
  | `Server_first | `Client_first | `Open | `None ->
      Eio.Switch.run @@ fun sw ->
      let l = Eio.Net.listen ~sw ~backlog:8 net addr in
      let conn =
        if which = `None then None
        else
          let c = Eio.Net.connect ~sw net addr in
          Some (c, fst (Eio.Net.accept ~sw l))
      in
      (match (which, conn) with
      | `Server_first, Some (c, s) ->
          Eio.Flow.close s;
          Eio.Flow.close c
      | `Client_first, Some (c, s) ->
          Eio.Flow.close c;
          Eio.Flow.close s
      | _ -> ());
      Eio.Resource.close l;
      relisten addr
  | (`Bound_client_first | `Bound_server_first) as which ->
      Eio.Switch.run @@ fun sw ->
      let l = Eio.Net.listen ~sw ~backlog:8 net addr in
      let bound = `Tcp (lo, free ()) in
      let c = Eio.Net.connect ~sw ~bind_to:bound net addr in
      let s, _ = Eio.Net.accept ~sw l in
      if which = `Bound_client_first then (
        Eio.Flow.close c;
        Eio.Flow.close s)
      else (
        Eio.Flow.close s;
        Eio.Flow.close c);
      let again =
        outcome (fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Net.connect ~sw ~bind_to:bound net addr);
            let s, _ = Eio.Net.accept ~sw l in
            Eio.Flow.close s;
            "ok")
      in
      again ^ " " ^ relisten bound

(* Each in its own switch, whose finish is not part of the outcome:
   eio_posix's listen then unlinks a path it never bound. *)
let unix_paths net =
  let long = String.make 120 'x' in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let o f =
    let r = ref "none" in
    (try
       Eio.Switch.run (fun sw ->
           r :=
             outcome (fun () ->
                 f sw;
                 "ok"))
     with _ -> ());
    !r
  in
  String.concat " "
    [
      o (fun sw -> ignore (Eio.Net.listen ~sw ~backlog:1 net (`Unix long)));
      o (fun sw -> ignore (Eio.Net.connect ~sw net (`Unix long)));
      o (fun sw -> ignore (Eio.Net.datagram_socket ~sw net (`Unix long)));
      o (fun sw ->
          ignore
            (Eio.Net.connect ~sw ~bind_to:(`Tcp (lo, 0)) net (`Unix "p.sock")));
    ]

let stream_ips =
  [
    "127.0.0.1";
    "0.0.0.0";
    "::1";
    "::";
    "::ffff:127.0.0.1";
    "::ffff:0.0.0.0";
    "127.0.0.2";
  ]

let non_local = [ "10.255.255.1"; "192.0.2.1"; "::2"; "::ffff:10.255.255.1" ]

let names =
  [
    "localhost";
    "127.0.0.1";
    "::1";
    "LOCALHOST";
    "localhost.";
    "0.0.0.0";
    "::";
    "127.1";
    "::ffff:127.0.0.1";
    "";
  ]

let services =
  [
    "80";
    "0";
    "65535";
    "65536";
    "http";
    "";
    "+80";
    "080";
    "0x50";
    "1_000";
    " 80";
    "-1";
    "-0";
    "0o17";
    "4294967296";
    "4294967295";
    "2147483648";
    "99999999999999999999";
    "\t7";
    "8 ";
    "+";
    "-";
  ]

let scenario_list : (string * (net -> string)) list =
  [ ("three", three) ]
  @ List.map (fun n -> (Fmt.str "backlog %d" n, backlog n)) [ 0; 1; 2; 5 ]
  @ List.map
      (fun a -> ("listen " ^ a, fun net -> listen_at net (`Tcp (ip a, 0))))
      (stream_ips @ non_local)
  @ List.concat_map
      (fun x ->
        List.concat_map
          (fun y ->
            List.map
              (fun (rx, ry) ->
                ( Fmt.str "listen %s reuse=%b then %s reuse=%b" x rx y ry,
                  fun net ->
                    Eio.Switch.run @@ fun sw ->
                    match
                      Eio.Net.listen ~sw ~reuse_addr:rx ~backlog:1 net
                        (`Tcp (ip x, 0))
                    with
                    | exception e -> classify e
                    | l -> listen_at ~reuse_addr:ry net (`Tcp (ip y, port_of l))
                ))
              [ (false, false); (true, true) ])
          [ "127.0.0.1"; "0.0.0.0"; "::1"; "::"; "127.0.0.2" ])
      [ "127.0.0.1"; "0.0.0.0"; "::1"; "::"; "127.0.0.2" ]
  @ List.concat_map
      (fun at ->
        List.concat_map
          (fun dst ->
            List.map
              (fun bind ->
                ( Fmt.str "connect %s from %s to %s" dst
                    (Option.value bind ~default:"-")
                    at,
                  connect_case at dst bind ))
              (None :: List.map Option.some (stream_ips @ [ "10.255.255.1" ])))
          stream_ips)
      stream_ips
  @ List.concat_map
      (fun at ->
        List.concat_map
          (fun src ->
            List.map
              (fun dst ->
                ( Fmt.str "dgram %s from %s to %s" dst src at,
                  dgram_case at src dst ))
              (List.filter (fun a -> a <> "::ffff:0.0.0.0") stream_ips))
          (stream_ips @ [ "UdpV4"; "UdpV6" ]))
      stream_ips
  @ List.concat_map
      (fun n ->
        List.map (fun s -> (Fmt.str "getaddrinfo %S %S" n s, gai n s)) services)
      names
  @ List.map
      (fun (n, a) -> ("getnameinfo " ^ n, gni a))
      [
        ("tcp 127.0.0.1:80", `Tcp (ip "127.0.0.1", 80));
        ("tcp ::1:22", `Tcp (ip "::1", 22));
        ("udp 127.0.0.1:53", `Udp (ip "127.0.0.1", 53));
        ("tcp 127.0.0.1:49999", `Tcp (ip "127.0.0.1", 49999));
        ("tcp 10.1.2.3:80", `Tcp (ip "10.1.2.3", 80));
        ("tcp 127.0.0.2:8080", `Tcp (ip "127.0.0.2", 8080));
      ]
  @ List.map
      (fun (n, w) -> ("reuse " ^ n, reuse w))
      [
        ("server closes first", `Server_first);
        ("client closes first", `Client_first);
        ("connection open", `Open);
        ("no connection", `None);
        ("bound client closes first", `Bound_client_first);
        ("bound server closes first", `Bound_server_first);
      ]
  @ [ ("unix paths", unix_paths) ]

let scenarios = List.map fst scenario_list

let scenario net name =
  let f = List.assoc name scenario_list in
  let r = try f net with e -> "abort " ^ classify e in
  String.map (function ',' | '\n' -> ';' | c -> c) r
