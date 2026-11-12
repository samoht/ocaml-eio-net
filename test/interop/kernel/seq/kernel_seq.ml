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
  | Eio_net_mem.Host_unreachable -> Some (Unix.error_message Unix.EHOSTUNREACH)
  | Eio_net_mem.Message_too_long -> Some "EMSGSIZE"
  | Eio_net_mem.Invalid_port -> Some "EINVAL"
  | Eio_net_mem.Network_unreachable -> Some "ENETUNREACH"
  | Eio_net_mem.Address_family -> Some "EAFNOSUPPORT"
  | Eio_net_mem.Wrong_socket_type -> Some "EPROTOTYPE"
  | Eio_net_mem.Address_not_available -> Some "EADDRNOTAVAIL"
  | Eio_net_mem.Name_too_long -> Some "ENAMETOOLONG"
  | Eio_net_mem.Invalid_address -> Some "EINVAL"
  | _ -> None

let rec classify = function
  | Eio.Exn.Multiple l ->
      Fmt.str "multiple(%s)"
        (String.concat "+" (List.map (fun (e, _) -> classify e) l))
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

(* A path with a NUL that is not its first byte: a socket bound there, a
   connect and a datagram sent there. *)
let nul_path net =
  let path = `Unix "nul\000x.sock" in
  let o f =
    outcome (fun () ->
        Eio.Switch.run @@ fun sw ->
        f sw;
        "ok")
  in
  String.concat " "
    [
      o (fun sw -> ignore (Eio.Net.listen ~sw ~backlog:1 net path));
      o (fun sw -> ignore (Eio.Net.connect ~sw net path));
      o (fun sw -> ignore (Eio.Net.datagram_socket ~sw net path));
      o (fun sw ->
          let d = Eio.Net.datagram_socket ~sw net (`Unix "nul-from.sock") in
          Eio.Net.send d ~dst:path [ Cstruct.of_string "x" ]);
      o (fun sw ->
          let l = Eio.Net.listen ~sw ~backlog:1 net (`Unix "nul-to.sock") in
          ignore l;
          ignore (Eio.Net.connect ~sw ~bind_to:path net (`Unix "nul-to.sock")));
    ]

(* A TCP hold on port [p]: a listener at [a], or a client bound at [a] of a
   listener at [to4], an IPv4 one. *)
let hold_at ~sw net ~to4 what p =
  match what with
  | `Listen a -> ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip a, p)))
  | `Client a ->
      ignore
        (Eio.Net.connect ~sw
           ~bind_to:(`Tcp (ip a, p))
           net
           (`Tcp (ip "127.0.0.1", port_of to4)))

(* A connect bound at the IPv6 wildcard address and a port [p], and a hold on
   [p] made before it, [`Hold_first], or after: the outcome of each. The
   listeners they connect to bind their ports first, so that [p] is none of
   them. *)
let wildcard_bind what order net =
  Eio.Switch.run @@ fun sw ->
  let to6 = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (ip "::1", 0)) in
  let to4 = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (ip "127.0.0.1", 0)) in
  let p =
    Eio.Switch.run @@ fun sw ->
    port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip "127.0.0.1", 0)))
  in
  let bound () =
    outcome (fun () ->
        ignore
          (Eio.Net.connect ~sw
             ~bind_to:(`Tcp (ip "::", p))
             net
             (`Tcp (ip "::1", port_of to6)));
        "ok")
  in
  let held () =
    outcome (fun () ->
        hold_at ~sw net ~to4 what p;
        "ok")
  in
  match order with
  | `Hold_first ->
      let h = held () in
      h ^ "/" ^ bound ()
  | `Bound_first ->
      let b = bound () in
      b ^ "/" ^ held ()

(* A connect from a listener at the IPv6 wildcard address to an address off
   the loopback interface, at the listener's port. *)
let off_loopback_connect dst net =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (ip "::", 0)) in
  outcome (fun () ->
      ignore (Eio.Net.connect ~sw net (`Tcp (ip dst, port_of l)));
      "ok")

(* The error a bind raises, with its shape: raw from eio_posix's listen and
   datagram_socket, or an Eio.Io error. *)
let shaped f =
  match f () with
  | () -> "ok"
  | exception (Unix.Unix_error _ as e) -> "raw:" ^ classify e
  | exception (Eio.Io _ as e) -> "io:" ^ classify e
  | exception e -> classify e

let bind_shapes net =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0)) in
  let held = `Tcp (lo, port_of l) in
  let d = Eio.Net.datagram_socket ~sw net (`Udp (lo, 0)) in
  let dport = udp_port d 49152 in
  let in_switch f () = Eio.Switch.run (fun sw -> f sw) in
  String.concat " "
    (List.map shaped
       [
         in_switch (fun sw -> ignore (Eio.Net.listen ~sw ~backlog:1 net held));
         in_switch (fun sw ->
             ignore (Eio.Net.datagram_socket ~sw net (`Udp (lo, dport))));
         in_switch (fun sw ->
             ignore
               (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip "10.255.255.1", 0))));
         in_switch (fun sw ->
             ignore (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip "ff02::1", 0))));
         in_switch (fun sw ->
             ignore
               (Eio.Net.datagram_socket ~sw net (`Unix (String.make 120 'z'))));
         in_switch (fun sw ->
             ignore (Eio.Net.connect ~sw ~bind_to:held net (`Tcp (lo, 1))));
       ])

(* Two datagram sockets at the empty path send to a named one: each autobinds
   an abstract address, a NUL and five hexadecimal digits, which a reply
   reaches. *)
let autobind net =
  Eio.Switch.run @@ fun sw ->
  if Sys.file_exists "ab.sock" then Unix.unlink "ab.sock";
  let r = Eio.Net.datagram_socket ~sw net (`Unix "ab.sock") in
  let a = Eio.Net.datagram_socket ~sw net (`Unix "") in
  let b = Eio.Net.datagram_socket ~sw net (`Unix "") in
  let buf = Cstruct.create 8 in
  let from s =
    Eio.Net.send s ~dst:(`Unix "ab.sock") [ Cstruct.of_string "x" ];
    fst (now (fun () -> Eio.Net.recv r buf))
  in
  let fa = from a and fb = from b in
  let form = function
    | `Unix n when String.length n > 0 && n.[0] = '\000' ->
        Fmt.str "abstract%d" (String.length n)
    | (`Unix _ | `Udp _) as addr -> shape (addr :> Eio.Net.Sockaddr.t)
  in
  let reply =
    outcome (fun () ->
        Eio.Net.send r ~dst:fa [ Cstruct.of_string "ok" ];
        let _, n = now (fun () -> Eio.Net.recv a buf) in
        "r" ^ string_of_int n)
  in
  let r =
    String.concat " "
      [ form fa; form fb; (if fa = fb then "same" else "distinct"); reply ]
  in
  if Sys.file_exists "ab.sock" then Unix.unlink "ab.sock";
  r

(* UDP sockets that both have SO_REUSEADDR at one port: which receives a
   datagram sent to 127.0.0.1. *)
let udp_reuse binds net =
  Eio.Switch.run @@ fun sw ->
  let socks =
    List.fold_left
      (fun (port, acc) a ->
        let addr = match port with None -> 0 | Some p -> p in
        let s =
          Eio.Net.datagram_socket ~sw ~reuse_addr:true net (`Udp (ip a, addr))
        in
        let port =
          match port with
          | Some p -> p
          | None -> udp_port s (49152 + List.length acc)
        in
        (Some port, acc @ [ s ]))
      (None, []) binds
  in
  match socks with
  | None, _ | _, [] -> "none"
  | Some port, socks ->
      let sender = Eio.Net.datagram_socket ~sw net `UdpV4 in
      Eio.Net.send sender
        ~dst:(`Udp (Eio.Net.Ipaddr.V4.loopback, port))
        [ Cstruct.of_string "x" ];
      String.concat " "
        (List.mapi
           (fun i s ->
             Fmt.str "s%d=%s" i
               (outcome (fun () ->
                    ignore (now (fun () -> Eio.Net.recv s (Cstruct.create 4)));
                    "got")))
           socks)

(* A datagram of [len] bytes from a socket at [src] to [dst]: the error
   Linux checks first. "short" and "long" are Unix paths whose address is
   shorter and longer than an IPv4 one. *)
let dgram_error src dst len net =
  Eio.Switch.run @@ fun sw ->
  if Sys.file_exists "u1.sock" then Unix.unlink "u1.sock";
  let addr = function
    | "unix" -> `Unix "u1.sock"
    | "short" -> `Unix "nowhere"
    | "long" -> `Unix "abcdefghijklmnop"
    | "port0" -> `Udp (Eio.Net.Ipaddr.V4.loopback, 0)
    | a -> `Udp (ip a, 9)
  in
  let src = match addr src with `Udp (a, _) -> `Udp (a, 0) | a -> a in
  let s = Eio.Net.datagram_socket ~sw net src in
  let r =
    outcome (fun () ->
        Eio.Net.send s ~dst:(addr dst) [ Cstruct.create len ];
        "sent")
  in
  if Sys.file_exists "u1.sock" then Unix.unlink "u1.sock";
  r

let edge_bind a net =
  let l = listen_at net (`Tcp (ip a, 0)) in
  let d =
    outcome (fun () ->
        Eio.Switch.run @@ fun sw ->
        ignore (Eio.Net.datagram_socket ~sw net (`Udp (ip a, 0)));
        "ok")
  in
  l ^ " " ^ d

let edge_connect ?bind d net =
  outcome (fun () ->
      Eio.Switch.run @@ fun sw ->
      let bind_to = Option.map (fun b -> `Tcp (ip b, 0)) bind in
      ignore (Eio.Net.connect ~sw ?bind_to net (`Tcp (ip d, 9)));
      "ok")

(* A connect bound at its own destination opens to itself. *)
let self_connect net =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let free =
    Eio.Switch.run @@ fun sw ->
    port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0)))
  in
  let addr = `Tcp (lo, free) in
  Eio.Switch.run @@ fun sw ->
  outcome (fun () ->
      let c = Eio.Net.connect ~sw ~bind_to:addr net addr in
      Eio.Flow.copy_string "hi" c;
      let got = read_s c 4 in
      Eio.Flow.shutdown c `Send;
      let eof = outcome (fun () -> read_s c 4) in
      let held = listen_at net addr in
      String.concat " " [ "ok"; got; eof; held ])

(* A listener closed with a connection queued, which no accept took: then a
   listen at its port, with or without reuse_addr, or a connect bound there,
   and what the queued client reads. *)
let queued_close how net =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  Eio.Switch.run @@ fun sw ->
  let other = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  let l = Eio.Net.listen ~sw ~backlog:4 net (`Tcp (lo, 0)) in
  let addr = Eio.Net.listening_addr l in
  let c = Eio.Net.connect ~sw net addr in
  Eio.Resource.close l;
  let again =
    match how with
    | `Relisten -> listen_at net addr
    | `Relisten_reuse -> listen_at ~reuse_addr:true net addr
    | `Bind_to ->
        outcome (fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore
              (Eio.Net.connect ~sw ~bind_to:addr net
                 (Eio.Net.listening_addr other));
            "ok")
  in
  let read =
    outcome (fun () -> "r" ^ string_of_int (String.length (read_s c 4)))
  in
  again ^ " " ^ read

(* Linux's abstract namespace: a Unix address that starts with a NUL, which
   has no file. *)
let abstract name = `Unix ("\000eio-net-" ^ name)

(* Each in its own switch, whose finish is not part of the outcome. *)
let attempt_s f =
  let r = ref "none" in
  (try Eio.Switch.run (fun sw -> r := outcome (fun () -> f sw)) with _ -> ());
  !r

let attempt f =
  attempt_s (fun sw ->
      f sw;
      "ok")

let abstract_cases =
  let dgram sw net a = ignore (Eio.Net.datagram_socket ~sw net a) in
  let listen ?reuse_addr sw net a =
    ignore (Eio.Net.listen ?reuse_addr ~sw ~backlog:1 net a)
  in
  let connect sw net a = ignore (Eio.Net.connect ~sw net a) in
  (* From a named path, so that the sender binds no abstract address. *)
  let send sw net a =
    if Sys.file_exists "as.sock" then Unix.unlink "as.sock";
    let s = Eio.Net.datagram_socket ~sw net (`Unix "as.sock") in
    Fun.protect
      ~finally:(fun () ->
        if Sys.file_exists "as.sock" then Unix.unlink "as.sock")
      (fun () -> Eio.Net.send s ~dst:a [ Cstruct.of_string "x" ])
  in
  [
    ( "connect unbound",
      fun net -> attempt (fun sw -> connect sw net (abstract "a")) );
    ("send unbound", fun net -> attempt (fun sw -> send sw net (abstract "b")));
    ( "send closed",
      fun net ->
        let a = abstract "c" in
        let bound = attempt (fun sw -> dgram sw net a) in
        bound ^ " " ^ attempt (fun sw -> send sw net a) );
    ( "connect closed",
      fun net ->
        let a = abstract "d" in
        let bound = attempt (fun sw -> listen sw net a) in
        bound ^ " " ^ attempt (fun sw -> connect sw net a) );
    ( "connect at datagram",
      fun net ->
        attempt (fun sw ->
            let a = abstract "e" in
            dgram sw net a;
            connect sw net a) );
    ( "send at listener",
      fun net ->
        attempt (fun sw ->
            let a = abstract "f" in
            listen sw net a;
            send sw net a) );
    ( "listen in use",
      fun net ->
        attempt_s (fun sw ->
            let a = abstract "g" in
            listen sw net a;
            let again = attempt (fun sw -> listen sw net a) in
            let reuse = attempt (fun sw -> listen ~reuse_addr:true sw net a) in
            again ^ " " ^ reuse) );
    ( "listen at datagram's",
      fun net ->
        attempt (fun sw ->
            let a = abstract "h" in
            dgram sw net a;
            listen sw net a) );
    ( "datagram at listener's",
      fun net ->
        attempt (fun sw ->
            let a = abstract "i" in
            listen sw net a;
            dgram sw net a) );
    ( "listen after close",
      fun net ->
        let a = abstract "j" in
        let first = attempt (fun sw -> listen sw net a) in
        first ^ " " ^ attempt (fun sw -> listen sw net a) );
  ]

(* The form of a Unix address: an abstract one by its length. *)
let unix_form (a : Eio.Net.Sockaddr.t) =
  match a with
  | `Unix n when String.length n > 0 && n.[0] = '\000' ->
      Fmt.str "abstract%d" (String.length n)
  | a -> shape a

(* The empty Unix path: a listen there and a connect bound there autobind an
   abstract address, as a datagram socket does; a connect to it is
   invalid. *)
let empty_path_cases =
  [
    ( "listen twice",
      fun net ->
        attempt_s (fun sw ->
            let l1 = Eio.Net.listen ~sw ~backlog:1 net (`Unix "") in
            let l2 = Eio.Net.listen ~sw ~backlog:1 net (`Unix "") in
            let a1 = Eio.Net.listening_addr l1
            and a2 = Eio.Net.listening_addr l2 in
            let c = Eio.Net.connect ~sw net a1 in
            Eio.Flow.copy_string "x" c;
            let s, _ = now (fun () -> Eio.Net.accept ~sw l1) in
            let got = read_s s 4 in
            let same = if a1 = a2 then "same" else "distinct" in
            String.concat " "
              [
                unix_form (a1 :> Eio.Net.Sockaddr.t);
                unix_form (a2 :> Eio.Net.Sockaddr.t);
                same;
                got;
              ]) );
    ( "bind_to",
      fun net ->
        attempt_s (fun sw ->
            if Sys.file_exists "eb.sock" then Unix.unlink "eb.sock";
            let l = Eio.Net.listen ~sw ~backlog:1 net (`Unix "eb.sock") in
            ignore
              (Eio.Net.connect ~sw ~bind_to:(`Unix "") net (`Unix "eb.sock"));
            let _, peer = now (fun () -> Eio.Net.accept ~sw l) in
            unix_form (peer :> Eio.Net.Sockaddr.t)) );
    ( "connect",
      fun net -> attempt (fun sw -> ignore (Eio.Net.connect ~sw net (`Unix "")))
    );
    ( "send",
      fun net ->
        attempt (fun sw ->
            let s = Eio.Net.datagram_socket ~sw net (`Unix "") in
            Eio.Net.send s ~dst:(`Unix "") [ Cstruct.of_string "x" ]) );
  ]

let listening_addr_closed net =
  attempt (fun sw ->
      let l =
        Eio.Net.listen ~sw ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      Eio.Resource.close l;
      ignore (Eio.Net.listening_addr l))

(* A connect that fails keeps its socket on its switch, bound: twice in one
   switch, then a third probe of the bound address in that switch, then a
   listen there once it ends. The third probe is a listen, except at a path:
   eio_posix's listen unlinks a path it never bound when its switch ends. *)
let failed_connect ~bind ~dst net =
  let file =
    match bind with
    | `Unix p when p <> "" && p.[0] <> '\000' -> Some p
    | _ -> None
  in
  Option.iter unlink file;
  let connect sw =
    outcome (fun () ->
        ignore (Eio.Net.connect ~sw ~bind_to:bind net dst);
        "ok")
  in
  let during =
    attempt_s (fun sw ->
        let first = connect sw in
        let second = connect sw in
        let third =
          match file with Some _ -> connect sw | None -> listen_at net bind
        in
        String.concat " " [ first; second; third ])
  in
  let after = listen_at net bind in
  Option.iter unlink file;
  during ^ " then " ^ after

let failed_connect_cases =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  (* Two free ports. *)
  let free net =
    Eio.Switch.run @@ fun sw ->
    let port () = port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0))) in
    let a = port () in
    (a, port ())
  in
  [
    ( "tcp",
      fun net ->
        let a, b = free net in
        failed_connect ~bind:(`Tcp (lo, a)) ~dst:(`Tcp (lo, b)) net );
    ("path", failed_connect ~bind:(`Unix "fc.sock") ~dst:(`Unix "fc-none.sock"));
    ("abstract", failed_connect ~bind:(abstract "k") ~dst:(abstract "kk"));
    ( "abstract to the empty path",
      failed_connect ~bind:(abstract "m") ~dst:(`Unix "") );
  ]

(* A Unix address's host is the host's name. *)
let gni_host addr net =
  let host, service = Eio.Net.getnameinfo net addr in
  let host = if host = Unix.gethostname () then "HOSTNAME" else host in
  host ^ "/" ^ service

(* Timed: TIME_WAIT and FIN_WAIT2. [wait d] lets [d] seconds pass. Each case
   opens a connection to a listener at a free port, closes it in an order,
   closes the listener, and listens at the port again now and after 65
   seconds, past Linux's 60-second timers. *)
let timed_close order ~wait net =
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let reuse = order = `Both_reuse in
  let addr, bound =
    Eio.Switch.run @@ fun sw ->
    let free () = port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (lo, 0))) in
    let a = free () in
    (`Tcp (lo, a), `Tcp (lo, free ()))
  in
  let probe () =
    match order with
    | `Bound_client_first ->
        outcome (fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Net.connect ~sw ~bind_to:bound net addr);
            "ok")
        ^ "/" ^ listen_at net bound
    | `Relisten_reuse | `Both_reuse -> listen_at ~reuse_addr:true net addr
    | _ -> listen_at net addr
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~reuse_addr:reuse ~backlog:4 net addr in
  let bind_to = if order = `Bound_client_first then Some bound else None in
  let c = Eio.Net.connect ~sw ?bind_to net addr in
  let s, _ = Eio.Net.accept ~sw l in
  let close f = Eio.Flow.close f and shut f cmd = Eio.Flow.shutdown f cmd in
  let late = ref [] in
  (match order with
  | `Server_first | `Relisten_reuse | `Both_reuse ->
      close s;
      close c
  | `Client_first | `Bound_client_first ->
      close c;
      close s
  | `Client_stays -> close s
  | `Reset ->
      Eio.Flow.copy_string "unread" c;
      close s;
      close c
  | `Shut_both ->
      shut s `Send;
      shut c `Send;
      close c;
      close s
  | `Server_shut_client_close ->
      shut s `Send;
      close c;
      close s
  | `Client_shut cmd ->
      shut c cmd;
      close s;
      close c
  | `Client_closes_later ->
      close s;
      late := [ c ]);
  if order <> `Bound_client_first then Eio.Resource.close l;
  let first = probe () in
  let rest =
    match !late with
    | [ c ] ->
        wait 30.;
        close c;
        wait 35.;
        let mid = probe () in
        wait 30.;
        [ mid; probe () ]
    | _ ->
        wait 65.;
        [ probe () ]
  in
  String.concat " " (first :: rest)

let timed_cases =
  [
    ("server closes first", `Server_first);
    ("server closes first and relisten with reuse_addr", `Relisten_reuse);
    ("server closes first with reuse_addr on both", `Both_reuse);
    ("client closes first", `Client_first);
    ("server closes and client stays", `Client_stays);
    ("reset by unread bytes", `Reset);
    ("bound client closes first", `Bound_client_first);
    ("both shut send and server first", `Shut_both);
    ("server shuts send then client closes", `Server_shut_client_close);
    ("client shuts send then server closes", `Client_shut `Send);
    ("client shuts down then server closes", `Client_shut `All);
    ("client shuts receive then server closes", `Client_shut `Receive);
    ("server closes and client closes at 30 s", `Client_closes_later);
  ]

(* Addresses that loopback does not reach: a host with loopback only has no
   route to them. *)
let off_loopback_ips =
  [ "10.1.2.3"; "2001:db8::1"; "::2"; "::ffff:10.1.2.3"; "::127.0.0.1" ]

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

(* Names that are numbers only in part, each with service "80". A host with
   no IPv6 address but loopback answers ADDRFAMILY for any IPv6 one, so only
   the scopes glibc takes are here. *)
let odd_names =
  [
    "LocalHost.";
    "[::1]";
    "0x";
    "0X";
    "0x.1";
    "1.0x";
    "fe80::1%lo";
    "fe80::1%1";
    "fe80::1%2";
    "fe80::1%999";
    "::1%1";
    "127.0.0.1%lo";
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

let dgram_error_cases =
  [
    ("v4->v6", "127.0.0.1", "::1");
    ("v6lo->v4", "::1", "127.0.0.1");
    ("v6lo->mapped", "::1", "::ffff:127.0.0.1");
    ("mapped->v6", "::ffff:127.0.0.1", "::1");
    ("v4->port0", "127.0.0.1", "port0");
    ("v4->short path", "127.0.0.1", "short");
    ("v4->long path", "127.0.0.1", "long");
    ("v6->long path", "::1", "long");
    ("unix->v4", "unix", "127.0.0.1");
    ("unix->v6", "unix", "::1");
    ("unix->missing", "unix", "short");
    ("v4->10.1.2.3", "127.0.0.1", "10.1.2.3");
    ("v4 127.0.0.2->10.1.2.3", "127.0.0.2", "10.1.2.3");
    ("v4->224.0.0.1", "127.0.0.1", "224.0.0.1");
  ]

(* A writer blocked on a full queue, and what a shutdown [how] by [who]
   does: the reader's reads until one waits or ends, whether the write still
   waits, and how it ends once the reader closes. The bytes read are only
   said to be some, as the kernels queue different amounts. *)
let blocked_writer ~unix ~who how net =
  Eio.Switch.run @@ fun sw ->
  let addr =
    if unix then begin
      if Sys.file_exists "bw.sock" then Unix.unlink "bw.sock";
      `Unix "bw.sock"
    end
    else `Tcp (ip "127.0.0.1", 0)
  in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let c =
    Eio.Net.connect ~sw net
      (match addr with `Unix _ -> addr | `Tcp _ -> Eio.Net.listening_addr l)
  in
  let r, _ = Eio.Net.accept ~sw l in
  let chunk = Cstruct.create 65536 in
  let w =
    Eio.Fiber.fork_promise ~sw (fun () ->
        outcome (fun () ->
            while true do
              Eio.Flow.write c [ chunk ]
            done;
            "ok"))
  in
  let waits () =
    for _ = 1 to 64 do
      Eio.Fiber.yield ()
    done;
    match Eio.Promise.peek w with
    | None -> "blk"
    | Some (Ok s) -> s
    | Some (Error e) -> classify e
  in
  let before = waits () in
  (match who with
  | `Reader -> Eio.Flow.shutdown r how
  | `Writer -> Eio.Flow.shutdown c how);
  let buf = Cstruct.create 65536 in
  (* Its own bound, past what any queue holds: reads that free room for
     the writer would otherwise go on for ever. *)
  let rec reads total =
    match now (fun () -> Eio.Flow.single_read r buf) with
    | _ when total > 64 * 1024 * 1024 -> "data more"
    | n -> reads (total + n)
    | exception e ->
        let e = match e with Blocked -> "blk" | e -> classify e in
        (if total > 0 then "data " else "") ^ e
  in
  let read = reads 0 in
  let after = waits () in
  Eio.Resource.close r;
  (* A reader that also shut its sending lingers in FIN-WAIT once closed,
     and Linux resets the writer only when its next probe comes. *)
  let closed = if how = `All then [] else [ waits () ] in
  String.concat " " ([ before; read; after ] @ closed)

(* A datagram sent from [src] to [dst], which only loopback could carry. *)
let send_only src dst net =
  Eio.Switch.run @@ fun sw ->
  let s = Eio.Net.datagram_socket ~sw net (`Udp (ip src, 0)) in
  outcome (fun () ->
      Eio.Net.send s ~dst:(`Udp (ip dst, 9)) [ Cstruct.create 10 ];
      "sent")

let off_loopback_routes =
  List.map
    (fun d ->
      ( Fmt.str "off loopback: connect %s from ::ffff:127.0.0.1" d,
        fun net -> edge_connect ~bind:"::ffff:127.0.0.1" d net ))
    [
      "::127.0.0.1";
      "2001:db8::1";
      "fc00::1";
      "::2";
      "64:ff9b::a00:1";
      "fe80::1";
      "::ffff:10.0.0.1";
      "ff02::1";
    ]
  @ List.concat_map
      (fun d ->
        List.map
          (fun bind ->
            ( Fmt.str "off loopback: connect %s from %s" d
                (Option.value bind ~default:"-"),
              fun net -> edge_connect ?bind d net ))
          [ None; Some "127.0.0.1" ])
      [
        "127.255.255.255";
        "255.255.255.255";
        "10.0.0.1";
        "169.254.1.1";
        "240.0.0.1";
        "0.1.2.3";
      ]
  @ List.concat_map
      (fun d ->
        List.map
          (fun src ->
            (Fmt.str "off loopback: send %s from %s" d src, send_only src d))
          [ "0.0.0.0"; "::"; "127.0.0.1"; "::1"; "::ffff:127.0.0.1" ])
      [
        "127.255.255.255";
        "255.255.255.255";
        "224.0.0.1";
        "239.1.1.1";
        "::ffff:224.0.0.1";
        "::ffff:255.255.255.255";
        "ff02::1";
        "ff0e::1";
        "ff01::1";
        "10.0.0.1";
        "169.254.1.1";
        "240.0.0.1";
        "0.1.2.3";
        "::ffff:10.0.0.1";
      ]

let blocked_writers =
  List.concat_map
    (fun (unix, family) ->
      List.map
        (fun (who, how, name) ->
          ( Fmt.str "blocked %s writer: %s" family name,
            blocked_writer ~unix ~who how ))
        [
          (`Reader, `Receive, "reader shuts receive");
          (`Reader, `All, "reader shuts down");
          (`Writer, `Send, "writer shuts send");
        ])
    [ (false, "tcp"); (true, "unix") ]

(* One sender sends datagrams of [size] bytes to 60 receivers in turn until
   a send waits: how many its send buffer took. *)
let unix_sndbuf ?sndbuf ?rcvbuf size net =
  Eio.Switch.run @@ fun sw ->
  let path i = Fmt.str "sb%d.%d" size i in
  let receivers = 60 in
  for i = 0 to receivers - 1 do
    unlink (path i);
    let r = Eio.Net.datagram_socket ~sw net (`Unix (path i)) in
    Option.iter (Eio.Net.setsockopt r Eio.Net.Sockopt.SO_RCVBUF) rcvbuf
  done;
  unlink (path receivers);
  let s = Eio.Net.datagram_socket ~sw net (`Unix (path receivers)) in
  Option.iter (Eio.Net.setsockopt s Eio.Net.Sockopt.SO_SNDBUF) sndbuf;
  let data = Cstruct.create size in
  (* Its own bound, past what a send buffer holds. *)
  let rec go n =
    if n >= 2000 then "sent more"
    else
      match
        now (fun () ->
            Eio.Net.send s ~dst:(`Unix (path (n mod receivers))) [ data ])
      with
      | () -> go (n + 1)
      | exception Blocked -> Fmt.str "sent=%d" n
      | exception e -> Fmt.str "sent=%d then %s" n (classify e)
  in
  let r = go 0 in
  for i = 0 to receivers do
    unlink (path i)
  done;
  r

(* A send that waits for room on a full receiver, and what ends it: the
   receiver shuts its receiving, or the sender its sending. A send still
   waiting is cancelled at the end. *)
let unix_blocked_send who net =
  Eio.Switch.run @@ fun sw ->
  unlink "bs.r";
  unlink "bs.s";
  let r = Eio.Net.datagram_socket ~sw net (`Unix "bs.r") in
  let s = Eio.Net.datagram_socket ~sw net (`Unix "bs.s") in
  let send () = Eio.Net.send s ~dst:(`Unix "bs.r") [ Cstruct.of_string "x" ] in
  let rec fill n =
    if n >= 1000 then n
    else match now send with () -> fill (n + 1) | exception Blocked -> n
  in
  let queued = fill 0 in
  let result = ref "" in
  (try
     Eio.Switch.run @@ fun sw ->
     let w =
       Eio.Fiber.fork_promise ~sw (fun () ->
           outcome (fun () ->
               send ();
               "ok"))
     in
     let waits () =
       for _ = 1 to 64 do
         Eio.Fiber.yield ()
       done;
       match Eio.Promise.peek w with
       | None -> "blk"
       | Some (Ok s) -> s
       | Some (Error e) -> classify e
     in
     let before = waits () in
     (match who with
     | `Receiver -> dgram_shutdown r `Receive
     | `Sender -> dgram_shutdown s `Send);
     let after = waits () in
     result := Fmt.str "queued=%d %s %s" queued before after;
     Eio.Switch.fail sw Exit
   with Exit -> ());
  unlink "bs.r";
  unlink "bs.s";
  !result

(* UDP datagrams of 1000 bytes sent and received in [steps] over loopback:
   how many the receiver then holds, as Linux frees the memory of those
   read only in batches. *)
let udp_rmem ?rcvbuf steps net =
  Eio.Switch.run @@ fun sw ->
  let addr = `Udp (ip "127.0.0.1", 41999) in
  let r = Eio.Net.datagram_socket ~sw net addr in
  Option.iter (Eio.Net.setsockopt r Eio.Net.Sockopt.SO_RCVBUF) rcvbuf;
  let s = Eio.Net.datagram_socket ~sw net (`Udp (ip "127.0.0.1", 0)) in
  let buf = Cstruct.create 2000 in
  List.iter
    (function
      | `Send n ->
          for _ = 1 to n do
            Eio.Net.send s ~dst:addr [ Cstruct.create 1000 ]
          done
      | `Recv n ->
          for _ = 1 to n do
            ignore (now (fun () -> Eio.Net.recv r buf))
          done)
    steps;
  let rec drain n =
    match now (fun () -> Eio.Net.recv r buf) with
    | _ -> drain (n + 1)
    | exception Blocked -> n
  in
  Fmt.str "kept=%d" (drain 0)

(* A connection to a listener at [lst] from [cli], left open or closed by
   one end first, once the listener closed: what a listen without
   [SO_REUSEADDR] at the listener's port finds at each address. *)
let dual_hold lst cli which net =
  let p =
    Eio.Switch.run @@ fun sw ->
    port_of (Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip lst, 0)))
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:1 net (`Tcp (ip lst, p)) in
  let c = Eio.Net.connect ~sw net (`Tcp (ip cli, p)) in
  let s, _ = Eio.Net.accept ~sw l in
  Eio.Resource.close l;
  (match which with
  | `Server_first ->
      Eio.Flow.close s;
      Eio.Flow.close c
  | `Client_first ->
      Eio.Flow.close c;
      Eio.Flow.close s
  | `Open -> ());
  String.concat " "
    (List.map
       (fun a -> a ^ "=" ^ listen_at net (`Tcp (ip a, p)))
       [ "::"; "::1"; "0.0.0.0"; "127.0.0.1" ])

(* The ephemeral ports of the range, which a kernel's range narrowed to a
   few ports runs out of in a few connects; a whole range would take the
   host's. Outcomes count no ports: a count is compared with the first
   destination's. Each case listens at its own ports, below both ranges, so
   that one case's TIME_WAITs are not another's. *)

(* Connects to [addr], each accepted and kept open, until one fails: the
   connections and its error. *)
let fill ?bind_to ~sw net l addr =
  let rec go n acc =
    if n = 70000 then (acc, "max")
    else
      match Eio.Net.connect ~sw ?bind_to net addr with
      | c ->
          let s, _ = Eio.Net.accept ~sw l in
          go (n + 1) ((c, s) :: acc)
      | exception e -> (acc, classify e)
  in
  go 0 []

(* Closes the server's ends first, so that the clients hold no port. *)
let close_all ?(client_first = false) conns =
  List.iter
    (fun (c, s) ->
      let a, b = if client_first then (c, s) else (s, c) in
      Eio.Flow.close a;
      Eio.Flow.close b)
    conns

let as_many first l =
  if first = [] then "none"
  else if List.compare_lengths l first = 0 then "as many"
  else if List.compare_length_with l (List.length first - 1) = 0 then
    "one fewer"
  else Fmt.str "%d of %d" (List.length l) (List.length first)

let ports_listen ~sw net a p =
  let addr = `Tcp (ip a, p) in
  (Eio.Net.listen ~sw ~backlog:16 net addr, addr)

(* Connects toward three destinations, each until one fails, then a listen
   at port 0. *)
let ports_shared ~wait:_ net =
  Eio.Switch.run @@ fun sw ->
  let la, a = ports_listen ~sw net "127.0.0.1" 7101 in
  let lb, b = ports_listen ~sw net "127.0.0.1" 7102 in
  let l6, c = ports_listen ~sw net "::1" 7103 in
  let ca, ea = fill ~sw net la a in
  let cb, eb = fill ~sw net lb b in
  let c6, e6 = fill ~sw net l6 c in
  let l0 = listen_at net (`Tcp (ip "127.0.0.1", 0)) in
  List.iter close_all [ ca; cb; c6 ];
  Fmt.str "A %s %s B %s %s ::1 %s %s listen 0 %s" (as_many ca ca) ea
    (as_many ca cb) eb (as_many ca c6) e6 l0

(* Connects until one fails, closed server first, then as many again. *)
let ports_server_first ~wait:_ net =
  Eio.Switch.run @@ fun sw ->
  let l, a = ports_listen ~sw net "127.0.0.1" 7104 in
  let first, e = fill ~sw net l a in
  close_all first;
  let again, e' = fill ~sw net l a in
  close_all again;
  Fmt.str "%s %s again %s %s" (as_many first first) e (as_many first again) e'

(* Connects bound at port 0 until one fails, then one that binds no
   address, toward another destination. *)
let ports_bound ~wait:_ net =
  Eio.Switch.run @@ fun sw ->
  let la, a = ports_listen ~sw net "127.0.0.1" 7105 in
  let lb, b = ports_listen ~sw net "127.0.0.1" 7106 in
  let bound, e = fill ~bind_to:(`Tcp (ip "127.0.0.1", 0)) ~sw net la a in
  let unbound, e' = fill ~sw net lb b in
  List.iter close_all [ bound; unbound ];
  Fmt.str "bound %s then %s" e
    (if unbound = [] then e' else Fmt.str "%d then %s" (List.length unbound) e')

(* How many ports a destination has, then a connect bound at port 0 that
   closes first, and the ports another destination has. *)
let ports_bound_time_wait ~wait:_ net =
  Eio.Switch.run @@ fun sw ->
  let la, a = ports_listen ~sw net "127.0.0.1" 7107 in
  let lb, b = ports_listen ~sw net "127.0.0.1" 7108 in
  let first, _ = fill ~sw net la a in
  close_all first;
  let c = Eio.Net.connect ~sw ~bind_to:(`Tcp (ip "127.0.0.1", 0)) net a in
  let s, _ = Eio.Net.accept ~sw la in
  close_all ~client_first:true [ (c, s) ];
  let other, e = fill ~sw net lb b in
  close_all other;
  Fmt.str "other %s %s" (as_many first other) e

(* Connects until one fails, closed client first, so that every port is in
   TIME_WAIT toward the destination: a connect to it and to another at once,
   and to it after 0.5 s and 1.5 s. *)
let ports_client_first ~wait net =
  Eio.Switch.run @@ fun sw ->
  let la, a = ports_listen ~sw net "127.0.0.1" 7109 in
  let lb, b = ports_listen ~sw net "127.0.0.1" 7110 in
  let first, _ = fill ~sw net la a in
  close_all ~client_first:true first;
  let connect l addr =
    outcome (fun () ->
        let c = Eio.Net.connect ~sw net addr in
        let s, _ = Eio.Net.accept ~sw l in
        close_all [ (c, s) ];
        "ok")
  in
  let at_once = connect la a in
  let other = connect lb b in
  wait 0.5;
  let half = connect la a in
  wait 1.0;
  Fmt.str "%s other %s 0.5 s %s 1.5 s %s" at_once other half (connect la a)

let ports_cases =
  [
    ("ports: destinations share a port", ports_shared);
    ("ports: server closes first", ports_server_first);
    ("ports: bound connects", ports_bound);
    ("ports: a bound connect in TIME_WAIT", ports_bound_time_wait);
    ("ports: clients close first", ports_client_first);
  ]

(* Socket options: each option's default, a set and the value read back, on
   every kind of socket; options a listener passes to the connections it
   accepts; options given to a connect; edge values; and what the buffer
   sizes change in the queues. *)

type opt = Opt : string * 'a Eio.Net.Sockopt.t * 'a * ('a -> string) -> opt

let b = string_of_bool
let i = string_of_int
let oi = function None -> "none" | Some n -> string_of_int n
let f = Fmt.str "%g"

let mtu_discover = function
  | `Want -> "want"
  | `Dont -> "dont"
  | `Do -> "do"
  | `Probe -> "probe"

let range (l, u) = Fmt.str "%d-%d" l u

let opts =
  let module O = Eio.Net.Sockopt in
  let module U = Eio_unix.Net in
  [
    Opt ("SO_DEBUG", O.SO_DEBUG, true, b);
    Opt ("SO_BROADCAST", O.SO_BROADCAST, true, b);
    Opt ("SO_REUSEADDR", O.SO_REUSEADDR, true, b);
    Opt ("SO_KEEPALIVE", O.SO_KEEPALIVE, true, b);
    Opt ("SO_DONTROUTE", O.SO_DONTROUTE, true, b);
    Opt ("SO_OOBINLINE", O.SO_OOBINLINE, true, b);
    Opt ("TCP_NODELAY", O.TCP_NODELAY, true, b);
    Opt ("IPV6_ONLY", O.IPV6_ONLY, true, b);
    Opt ("SO_REUSEPORT", O.SO_REUSEPORT, true, b);
    Opt ("SO_SNDBUF", O.SO_SNDBUF, 10000, i);
    Opt ("SO_RCVBUF", O.SO_RCVBUF, 10000, i);
    Opt ("SO_RCVLOWAT", O.SO_RCVLOWAT, 10, i);
    Opt ("SO_SNDLOWAT", O.SO_SNDLOWAT, 10, i);
    Opt ("SO_LINGER", O.SO_LINGER, Some 5, oi);
    Opt ("SO_RCVTIMEO", O.SO_RCVTIMEO, 1.5, f);
    Opt ("SO_SNDTIMEO", O.SO_SNDTIMEO, 1.5, f);
    Opt ("TCP_KEEPIDLE", O.TCP_KEEPIDLE, 30, i);
    Opt ("TCP_KEEPINTVL", O.TCP_KEEPINTVL, 5, i);
    Opt ("TCP_KEEPCNT", O.TCP_KEEPCNT, 3, i);
    Opt ("TCP_CONGESTION", O.TCP_CONGESTION, "reno", Fun.id);
    Opt ("TCP_CORK", O.TCP_CORK, true, b);
    Opt ("TCP_USER_TIMEOUT", O.TCP_USER_TIMEOUT, 1000, i);
    Opt ("TCP_MAXSEG", O.TCP_MAXSEG, 1000, i);
    Opt ("TCP_LINGER2", O.TCP_LINGER2, Some 10, oi);
    Opt ("TCP_DEFER_ACCEPT", O.TCP_DEFER_ACCEPT, 5, i);
    Opt ("TCP_SYNCNT", O.TCP_SYNCNT, 3, i);
    Opt ("TCP_WINDOW_CLAMP", O.TCP_WINDOW_CLAMP, 10000, i);
    Opt ("TCP_QUICKACK", O.TCP_QUICKACK, true, b);
    Opt ("TCP_FASTOPEN", O.TCP_FASTOPEN, 5, i);
    Opt ("IP_FREEBIND", O.IP_FREEBIND, true, b);
    Opt ("IP_BIND_ADDRESS_NO_PORT", O.IP_BIND_ADDRESS_NO_PORT, true, b);
    Opt ("IP_LOCAL_PORT_RANGE", O.IP_LOCAL_PORT_RANGE, (40000, 40100), range);
    Opt ("IP_TTL", O.IP_TTL, 32, i);
    Opt ("IP_MTU", O.IP_MTU, 1000, i);
    Opt ("IP_MTU_DISCOVER", O.IP_MTU_DISCOVER, `Do, mtu_discover);
    Opt ("Unix.SO_DEBUG", U.Sockopt_bool SO_DEBUG, true, b);
    Opt ("Unix.SO_BROADCAST", U.Sockopt_bool SO_BROADCAST, true, b);
    Opt ("Unix.SO_REUSEADDR", U.Sockopt_bool SO_REUSEADDR, true, b);
    Opt ("Unix.SO_KEEPALIVE", U.Sockopt_bool SO_KEEPALIVE, true, b);
    Opt ("Unix.SO_DONTROUTE", U.Sockopt_bool SO_DONTROUTE, true, b);
    Opt ("Unix.SO_OOBINLINE", U.Sockopt_bool SO_OOBINLINE, true, b);
    Opt ("Unix.SO_ACCEPTCONN", U.Sockopt_bool SO_ACCEPTCONN, true, b);
    Opt ("Unix.TCP_NODELAY", U.Sockopt_bool TCP_NODELAY, true, b);
    Opt ("Unix.IPV6_ONLY", U.Sockopt_bool IPV6_ONLY, true, b);
    Opt ("Unix.SO_REUSEPORT", U.Sockopt_bool SO_REUSEPORT, true, b);
    Opt ("Unix.SO_SNDBUF", U.Sockopt_int SO_SNDBUF, 10000, i);
    Opt ("Unix.SO_RCVBUF", U.Sockopt_int SO_RCVBUF, 10000, i);
    Opt ("Unix.SO_ERROR", (U.Sockopt_int SO_ERROR [@alert "-deprecated"]), 0, i);
    Opt ("Unix.SO_TYPE", U.Sockopt_int SO_TYPE, 1, i);
    Opt ("Unix.SO_RCVLOWAT", U.Sockopt_int SO_RCVLOWAT, 10, i);
    Opt ("Unix.SO_SNDLOWAT", U.Sockopt_int SO_SNDLOWAT, 10, i);
    Opt ("Unix.SO_LINGER", U.Sockopt_optint SO_LINGER, Some 5, oi);
    Opt ("Unix.SO_RCVTIMEO", U.Sockopt_float SO_RCVTIMEO, 1.5, f);
    Opt ("Unix.SO_SNDTIMEO", U.Sockopt_float SO_SNDTIMEO, 1.5, f);
  ]

let opt_name (Opt (n, _, _, _)) = n

(* Edge values, set on a connected TCP client. *)
let edge_values =
  let module O = Eio.Net.Sockopt in
  let ints name o vs = List.map (fun v -> Opt (name, o, v, i)) vs in
  ints "SO_SNDBUF" O.SO_SNDBUF [ 0; 1; 3000; 100000; 100000000; -1 ]
  @ ints "SO_RCVBUF" O.SO_RCVBUF [ 0; 1; 3000; 100000; 100000000; -1 ]
  @ ints "SO_RCVLOWAT" O.SO_RCVLOWAT [ 0; -1; 1000000000 ]
  @ ints "TCP_KEEPIDLE" O.TCP_KEEPIDLE [ -1; 0; 1; 32767; 32768 ]
  @ ints "TCP_KEEPINTVL" O.TCP_KEEPINTVL [ 0; 32767; 32768 ]
  @ ints "TCP_KEEPCNT" O.TCP_KEEPCNT [ 0; 127; 128; 255; 256 ]
  @ ints "TCP_USER_TIMEOUT" O.TCP_USER_TIMEOUT [ -1; 0 ]
  @ ints "TCP_MAXSEG" O.TCP_MAXSEG [ 0; 87; 88; 536; 32767; 32768; 70000 ]
  @ ints "TCP_DEFER_ACCEPT" O.TCP_DEFER_ACCEPT [ 0; 1; 2; 3; 4; 10; 100; 1000 ]
  @ ints "TCP_SYNCNT" O.TCP_SYNCNT [ 0; 1; 127; 128; 255; 256 ]
  @ ints "TCP_WINDOW_CLAMP" O.TCP_WINDOW_CLAMP [ 0; 1; 100; 2000; 100000000 ]
  @ ints "TCP_FASTOPEN" O.TCP_FASTOPEN [ 0; 1000000 ]
  @ ints "IP_TTL" O.IP_TTL [ -1; 0; 1; 255; 256 ]
  @ List.map
      (fun v -> Opt ("SO_LINGER", O.SO_LINGER, v, oi))
      [ None; Some 0; Some 327; Some 328; Some 32767; Some 32768; Some 100000 ]
  @ List.map
      (fun v -> Opt ("TCP_LINGER2", O.TCP_LINGER2, v, oi))
      [
        None;
        Some 0;
        Some (-1);
        Some 1;
        Some 60;
        Some 120;
        Some 121;
        Some 100000;
      ]
  @ List.map
      (fun v -> Opt ("SO_RCVTIMEO", O.SO_RCVTIMEO, v, f))
      [ 0.; -1.; 1e-7; 0.0000015; 1e9 ]
  @ List.map
      (fun v -> Opt ("TCP_CONGESTION", O.TCP_CONGESTION, v, Fun.id))
      [ "cubic"; "nope"; ""; String.make 20 'x' ]
  @ List.map
      (fun v -> Opt ("IP_LOCAL_PORT_RANGE", O.IP_LOCAL_PORT_RANGE, v, range))
      [ (0, 0); (40000, 0); (0, 40000); (5, 4); (-1, 0); (0, 65536) ]
  @ List.map
      (fun v -> Opt ("IP_MTU_DISCOVER", O.IP_MTU_DISCOVER, v, mtu_discover))
      [ `Want; `Dont; `Probe ]

(* Edge values of the options a listener takes on its own. *)
let listener_values =
  let module O = Eio.Net.Sockopt in
  let ints name o vs = List.map (fun v -> Opt (name, o, v, i)) vs in
  ints "TCP_FASTOPEN" O.TCP_FASTOPEN [ 0; 1; 4096; 4097; 1000000 ]
  @ ints "TCP_MAXSEG" O.TCP_MAXSEG [ 0; 88; 176; 512; 1000; 32767 ]
  @ ints "TCP_WINDOW_CLAMP" O.TCP_WINDOW_CLAMP [ 0; 100 ]

(* The edge values of socket-level options, which every socket takes. *)
let socket_values =
  List.filter
    (fun o ->
      List.mem (opt_name o)
        [ "SO_SNDBUF"; "SO_RCVBUF"; "SO_RCVLOWAT"; "SO_LINGER"; "SO_RCVTIMEO" ])
    edge_values

type sock = {
  set : 'a. 'a Eio.Net.Sockopt.t -> 'a -> unit;
  get : 'a. 'a Eio.Net.Sockopt.t -> 'a;
}

let sock s =
  {
    set = (fun o v -> Eio.Net.setsockopt s o v);
    get = (fun o -> Eio.Net.getsockopt s o);
  }

let rec sockopt_error = function
  | Eio.Io (Eio.Net.E Invalid_option, _) -> "Invalid_option"
  | Eio.Io (Eio.Exn.Not_available b, _) -> "Not_available " ^ backend b
  | Eio.Io (Eio.Fs.E (Permission_denied b), _) ->
      "Permission_denied " ^ backend b
  | Eio.Io (Eio.Fs.E (Not_found b), _) -> "Not_found " ^ backend b
  | Eio.Io (Eio.Exn.X b, _) -> backend b
  | Eio.Exn.Multiple ((e, _) :: _) -> sockopt_error e
  | e -> classify e

and backend = function
  | Eio_unix.Unix_error (e, _, _) -> errno e
  | b -> (
      match mem_errno b with
      | Some e -> e
      | None -> Fmt.str "%a" Eio.Exn.Backend.pp b)

let sockopt_outcome f =
  try f () with Eio.Cancel.Cancelled _ as e -> raise e | e -> sockopt_error e

(* macOS reads an IP or TCP option on a connected Unix stream socket as the
   local option of the same number: TCP_MAXSEG as LOCAL_PEERPID, IP_TTL as
   LOCAL_PEERUUID, which differ from run to run. *)
let peer_info ~kind name r =
  let connected = kind = "unix client" || kind = "unix server" in
  if
    connected
    && List.mem name [ "TCP_MAXSEG"; "IP_TTL" ]
    && int_of_string_opt r <> None
  then "peer"
  else r

let get ?(kind = "") s (Opt (name, o, _, pp)) =
  peer_info ~kind name (sockopt_outcome (fun () -> pp (s.get o)))

let set s (Opt (_, o, v, _)) =
  sockopt_outcome (fun () ->
      s.set o v;
      "ok")

let probe ~kind s opt =
  let g0 = get ~kind s opt in
  let r = set s opt in
  Fmt.str "get=%s set=%s get=%s" g0 r (get ~kind s opt)

let sockopt_kinds =
  [
    "tcp4 listener";
    "tcp4 client";
    "tcp4 server";
    "tcp6 listener";
    "tcp6 client";
    "tcp6 server";
    "unix listener";
    "unix client";
    "unix server";
    "udp4";
    "udp6";
    "unix dgram";
  ]

let so_path = "so.sock"

(* Runs [f] on a socket of [kind]. With [listener], the listener's options
   are set first and the client sends a byte before the accept, which a
   listener with TCP_DEFER_ACCEPT waits for. *)
let on_socket ?(options = Eio.Net.Sockopt.[]) ?listener kind net f =
  unlink so_path;
  Fun.protect ~finally:(fun () -> unlink so_path) @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let family, which =
    match String.split_on_char ' ' kind with
    | [ family; which ] -> (family, which)
    | [ family ] -> (family, "")
    | _ -> invalid_arg kind
  in
  let stream addr =
    let l = Eio.Net.listen ~sw ~backlog:1 net addr in
    Option.iter (fun set -> set (sock l)) listener;
    if which = "listener" then f (sock l)
    else
      let dst =
        match addr with `Unix _ -> addr | `Tcp _ -> Eio.Net.listening_addr l
      in
      match Eio.Net.connect ~sw ~options net dst with
      | exception e -> "connect=" ^ sockopt_error e
      | c ->
          if Option.is_some listener then Eio.Flow.copy_string "x" c;
          let s, _ = Eio.Net.accept ~sw l in
          f (sock (if which = "client" then c else s))
  in
  let dgram addr = f (sock (Eio.Net.datagram_socket ~sw net addr)) in
  match (family, which) with
  | "tcp4", _ -> stream (`Tcp (ip "127.0.0.1", 0))
  | "tcp6", _ -> stream (`Tcp (ip "::1", 0))
  | "tcp6any", _ -> stream (`Tcp (ip "::", 0))
  | "udp6any", _ -> dgram (`Udp (ip "::", 0))
  | "unix", "dgram" -> dgram (`Unix so_path)
  | "unix", _ -> stream (`Unix so_path)
  | "udp4", _ -> dgram (`Udp (ip "127.0.0.1", 0))
  | "udp6", _ -> dgram (`Udp (ip "::1", 0))
  | _ -> invalid_arg kind

let core_opts =
  List.filter
    (fun o -> not (String.starts_with ~prefix:"Unix." (opt_name o)))
    opts

(* The amount a TCP or Unix-domain writer queues to a reader that does not
   read, with the writer's send buffer and the reader's receive buffer set
   once connected, [0] for the default. *)
let stream_fill kind ~snd ~rcv net =
  Eio.Switch.run @@ fun sw ->
  unlink so_path;
  let addr =
    match kind with `Tcp -> `Tcp (ip "127.0.0.1", 0) | `Unix -> `Unix so_path
  in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let dst =
    match addr with `Unix _ -> addr | `Tcp _ -> Eio.Net.listening_addr l
  in
  let c = Eio.Net.connect ~sw net dst in
  let r, _ = Eio.Net.accept ~sw l in
  if snd > 0 then Eio.Net.setsockopt c Eio.Net.Sockopt.SO_SNDBUF snd;
  if rcv > 0 then Eio.Net.setsockopt r Eio.Net.Sockopt.SO_RCVBUF rcv;
  let chunk = Cstruct.create 1024 in
  let rec go total =
    if total > 64 * 1024 * 1024 then "more"
    else
      match now (fun () -> Eio.Flow.single_write c [ chunk ]) with
      | n -> go (total + n)
      | exception Blocked -> Fmt.str "queued=%d" total
  in
  go 0

(* A close with SO_LINGER at [linger] and what the peer then reads. *)
let linger_close kind linger net =
  Eio.Switch.run @@ fun sw ->
  unlink so_path;
  let addr =
    match kind with `Tcp -> `Tcp (ip "127.0.0.1", 0) | `Unix -> `Unix so_path
  in
  let l = Eio.Net.listen ~sw ~backlog:1 net addr in
  let dst =
    match addr with `Unix _ -> addr | `Tcp _ -> Eio.Net.listening_addr l
  in
  let c = Eio.Net.connect ~sw net dst in
  let r, _ = Eio.Net.accept ~sw l in
  Eio.Net.setsockopt c Eio.Net.Sockopt.SO_LINGER linger;
  Eio.Resource.close c;
  let buf = Cstruct.create 8 in
  outcome (fun () -> string_of_int (now (fun () -> Eio.Flow.single_read r buf)))

let sockopt_scenarios =
  List.concat_map
    (fun (kind, name) ->
      List.map
        (fun linger ->
          ( Fmt.str "sockopt linger close: %s %s" name (oi linger),
            linger_close kind linger ))
        [ None; Some 0; Some 1 ])
    [ (`Tcp, "tcp4"); (`Unix, "unix") ]
  @ List.concat_map
      (fun kind ->
        List.map
          (fun o ->
            ( Fmt.str "sockopt: %s %s" kind (opt_name o),
              fun net -> on_socket kind net (fun s -> probe ~kind s o) ))
          opts)
      sockopt_kinds
  @ List.concat_map
      (fun kind ->
        List.map
          (fun o ->
            ( Fmt.str "sockopt inherit: %s %s" kind (opt_name o),
              fun net ->
                let r = ref "" in
                let got =
                  on_socket kind net
                    ~listener:(fun l -> r := set l o)
                    (fun s -> get ~kind s o)
                in
                Fmt.str "set=%s get=%s" !r got ))
          core_opts)
      [ "tcp4 server"; "unix server" ]
  @ List.concat_map
      (fun kind ->
        List.map
          (fun (Opt (_, o, v, _) as opt) ->
            ( Fmt.str "sockopt connect: %s %s" kind (opt_name opt),
              fun net ->
                on_socket kind net
                  ~options:Eio.Net.Sockopt.[ (o, v) ]
                  (fun s -> "connect=ok get=" ^ get ~kind s opt) ))
          core_opts)
      [ "tcp4 client"; "unix client" ]
  @ List.concat_map
      (fun kind ->
        List.filter_map
          (fun o ->
            if List.mem (opt_name o) [ "IPV6_ONLY"; "TCP_MAXSEG"; "IP_TTL" ]
            then
              Some
                ( Fmt.str "sockopt: %s %s" kind (opt_name o),
                  fun net -> on_socket kind net (fun s -> probe ~kind s o) )
            else None)
          core_opts)
      [ "tcp6any listener"; "tcp6any client"; "tcp6any server"; "udp6any" ]
  @ List.filter_map
      (fun (Opt (_, o, v, _) as opt) ->
        if List.mem (opt_name opt) [ "IPV6_ONLY"; "TCP_MAXSEG"; "IP_TTL" ] then
          Some
            ( Fmt.str "sockopt connect: tcp6 client %s" (opt_name opt),
              fun net ->
                on_socket "tcp6 client" net
                  ~options:Eio.Net.Sockopt.[ (o, v) ]
                  (fun s -> "connect=ok get=" ^ get s opt) )
        else None)
      core_opts
  @ List.concat_map
      (fun kind ->
        List.map
          (fun (Opt (name, _, v, pp) as opt) ->
            ( Fmt.str "sockopt value: %s %s=%s" kind name (pp v),
              fun net ->
                on_socket kind net (fun s ->
                    let r = set s opt in
                    Fmt.str "set=%s get=%s" r (get s opt)) ))
          edge_values)
      [ "tcp4 client" ]
  @ List.concat_map
      (fun kind ->
        List.map
          (fun (Opt (name, _, v, pp) as opt) ->
            ( Fmt.str "sockopt value: %s %s=%s" kind name (pp v),
              fun net ->
                on_socket kind net (fun s ->
                    let r = set s opt in
                    Fmt.str "set=%s get=%s" r (get s opt)) ))
          socket_values)
      [ "unix client"; "udp4"; "unix dgram" ]
  @ List.map
      (fun (Opt (name, _, v, pp) as opt) ->
        ( Fmt.str "sockopt value: tcp4 listener %s=%s" name (pp v),
          fun net ->
            on_socket "tcp4 listener" net (fun s ->
                let r = set s opt in
                Fmt.str "set=%s get=%s" r (get s opt)) ))
      listener_values
  @ List.concat_map
      (fun (kind, name) ->
        List.concat_map
          (fun snd ->
            List.map
              (fun rcv ->
                ( Fmt.str "sockopt fill: %s sndbuf=%d rcvbuf=%d" name snd rcv,
                  stream_fill kind ~snd ~rcv ))
              (if snd = 0 then [ 2000; 10000; 100000 ]
               else [ 0; 2000; 10000; 100000 ]))
          (match kind with
          | `Tcp -> [ 0; 2000; 10000; 100000 ]
          | `Unix -> [ 2000; 10000; 100000 ]))
      [ (`Tcp, "tcp4"); (`Unix, "unix") ]
  @ List.map
      (fun rcvbuf ->
        ( Fmt.str "sockopt udp rcvbuf=%d: send 99" rcvbuf,
          udp_rmem ~rcvbuf [ `Send 99 ] ))
      [ 1; 3000; 10000; 50000; 100000; 1000000 ]
  @ List.concat_map
      (fun sndbuf ->
        List.map
          (fun size ->
            ( Fmt.str "sockopt unix dgram sndbuf=%d: size=%d" sndbuf size,
              unix_sndbuf ~sndbuf size ))
          [ 1000; 8000 ])
      [ 1; 3000; 10000; 50000; 1000000 ]
  @ List.map
      (fun rcvbuf ->
        ( Fmt.str "sockopt unix dgram rcvbuf=%d: size=1000" rcvbuf,
          unix_sndbuf ~rcvbuf 1000 ))
      [ 1; 3000; 10000 ]

let untimed_scenarios : (string * (net -> string)) list =
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
  @ [ ("unix paths", unix_paths); ("bind error shapes", bind_shapes) ]
  @ [ ("autobind", autobind) ]
  @ List.map
      (fun (name, binds) -> ("udp reuse " ^ name, udp_reuse binds))
      [
        ("two at 127.0.0.1", [ "127.0.0.1"; "127.0.0.1" ]);
        ("0.0.0.0 then 127.0.0.1", [ "0.0.0.0"; "127.0.0.1" ]);
        ("127.0.0.1 then 0.0.0.0", [ "127.0.0.1"; "0.0.0.0" ]);
        ("two at 0.0.0.0", [ "0.0.0.0"; "0.0.0.0" ]);
        ("three at 127.0.0.1", [ "127.0.0.1"; "127.0.0.1"; "127.0.0.1" ]);
        (":: then 0.0.0.0", [ "::"; "0.0.0.0" ]);
        ("0.0.0.0 then ::", [ "0.0.0.0"; "::" ]);
        ("127.0.0.1 then ::", [ "127.0.0.1"; "::" ]);
        (":: then 127.0.0.1", [ "::"; "127.0.0.1" ]);
      ]
  @ List.concat_map
      (fun (name, src, dst) ->
        List.map
          (fun len ->
            (Fmt.str "dgram error %s len=%d" name len, dgram_error src dst len))
          [ 10; 65508; 65536; 300000 ])
      dgram_error_cases
  @ List.map
      (fun a -> ("edge bind " ^ a, edge_bind a))
      [
        "255.255.255.255";
        "224.0.0.1";
        "::ffff:224.0.0.1";
        "ff02::1";
        "fe80::1";
        "ff05::1";
      ]
  @ List.map
      (fun (d, bind) ->
        ( Fmt.str "edge connect %s from %s" d (Option.value bind ~default:"-"),
          fun net -> edge_connect ?bind d net ))
      [
        ("255.255.255.255", None);
        ("224.0.0.1", None);
        ("::ffff:224.0.0.1", None);
        ("ff02::1", None);
        ("fe80::1", None);
        ("10.1.2.3", Some "127.0.0.1");
        ("10.1.2.3", Some "127.0.0.2");
        ("::ffff:10.1.2.3", Some "::ffff:127.0.0.1");
      ]
  @ [ ("self connect", self_connect) ]
  @ List.map
      (fun (name, how) -> ("queued close: " ^ name, queued_close how))
      [
        ("relisten", `Relisten);
        ("relisten with reuse_addr", `Relisten_reuse);
        ("connect bound there", `Bind_to);
      ]
  @ List.map (fun (name, f) -> ("abstract " ^ name, f)) abstract_cases
  @ List.map (fun (name, f) -> ("empty path " ^ name, f)) empty_path_cases
  @ [ ("listening_addr after close", listening_addr_closed) ]
  @ List.map
      (fun n -> (Fmt.str "getaddrinfo %S %S" n "80", gai n "80"))
      odd_names
  @ List.map
      (fun (n, a) -> ("getnameinfo " ^ n, gni_host a))
      [
        ("tcp ::ffff:127.0.0.1:80", `Tcp (ip "::ffff:127.0.0.1", 80));
        ("tcp ::ffff:10.1.2.3:80", `Tcp (ip "::ffff:10.1.2.3", 80));
        ("tcp 127.0.0.2:80", `Tcp (ip "127.0.0.2", 80));
        ("unix x.sock", `Unix "x.sock");
        ("tcp ::7f00:1:80", `Tcp (ip "::7f00:1", 80));
        ("tcp ::1:0:80", `Tcp (ip "::1:0", 80));
        ("tcp ::2:80", `Tcp (ip "::2", 80));
        ("tcp ::ffff:0:a01:203:80", `Tcp (ip "::ffff:0:a01:203", 80));
        ("unix abstract", `Unix "\000abc");
      ]
  @ List.map
      (fun (name, f) -> ("failed connect: " ^ name, f))
      failed_connect_cases
  @ [ ("unix path with a NUL", nul_path) ]
  @ List.concat_map
      (fun (name, what) ->
        [
          ( Fmt.str "wildcard bind: bound at :: after %s" name,
            wildcard_bind what `Hold_first );
          ( Fmt.str "wildcard bind: %s after bound at ::" name,
            wildcard_bind what `Bound_first );
        ])
      [
        ("listen 0.0.0.0", `Listen "0.0.0.0");
        ("listen 127.0.0.1", `Listen "127.0.0.1");
        ("listen ::ffff:127.0.0.1", `Listen "::ffff:127.0.0.1");
        ("listen ::1", `Listen "::1");
        ("client 127.0.0.1", `Client "127.0.0.1");
      ]
  @ blocked_writers
  @ List.concat_map
      (fun (lst, cli) ->
        List.map
          (fun (name, which) ->
            ( Fmt.str "dual-stack hold: listen %s client %s %s" lst cli name,
              dual_hold lst cli which ))
          [
            ("open", `Open);
            ("server closes first", `Server_first);
            ("client closes first", `Client_first);
          ])
      [
        ("::", "::1");
        ("::", "127.0.0.1");
        ("::1", "::1");
        ("0.0.0.0", "127.0.0.1");
      ]
  @ List.map
      (fun size ->
        (Fmt.str "unix dgram sndbuf: size=%d" size, unix_sndbuf size))
      [
        1;
        500;
        1000;
        2000;
        4000;
        6000;
        8000;
        16000;
        16064;
        20000;
        50000;
        100000;
        212960;
      ]
  @ [
      ( "unix dgram blocked send: receiver shuts receive",
        unix_blocked_send `Receiver );
      ("unix dgram blocked send: sender shuts send", unix_blocked_send `Sender);
    ]
  @ List.map
      (fun (name, steps) -> ("udp rmem: " ^ name, udp_rmem steps))
      [
        ("send 99 recv 3 send 10", [ `Send 99; `Recv 3; `Send 10 ]);
        ("send 99 recv 24 send 10", [ `Send 99; `Recv 24; `Send 10 ]);
        ("send 99 recv 25 send 10", [ `Send 99; `Recv 25; `Send 10 ]);
        ("send 99 recv 98 send 10", [ `Send 99; `Recv 98; `Send 10 ]);
        ("send 99 recv 99 send 10", [ `Send 99; `Recv 99; `Send 10 ]);
        ( "send 10 recv 1 send 89 recv 9 send 10",
          [ `Send 10; `Recv 1; `Send 89; `Recv 9; `Send 10 ] );
        ( "send 10 recv 1 send 89 recv 8 send 10",
          [ `Send 10; `Recv 1; `Send 89; `Recv 8; `Send 10 ] );
        ( "send 99 recv 50 send 10 recv 25 send 30",
          [ `Send 99; `Recv 50; `Send 10; `Recv 25; `Send 30 ] );
      ]
  @ sockopt_scenarios
  @ List.map
      (fun d -> ("off loopback: connect " ^ d, off_loopback_connect d))
      off_loopback_ips
  @ List.concat_map
      (fun d ->
        List.map
          (fun src ->
            ( Fmt.str "off loopback: dgram %s from %s to ::" d src,
              dgram_case "::" src d ))
          (if not (String.contains d ':') then [ "0.0.0.0"; "::" ]
           else if String.contains d '.' then [ "::" ]
           else [ "::"; "::ffff:127.0.0.1" ]))
      off_loopback_ips
  @ off_loopback_routes

(* A scenario sent off loopback is recorded on a host with loopback only,
   which mem models: a host with other routes refuses a loopback source
   with EINVAL instead. *)
let routed_off (name, f) =
  let prefixes = [ "dgram error"; "edge connect" ] in
  let off =
    List.exists (fun prefix -> String.starts_with ~prefix name) prefixes
    &&
    let n = String.length name and m = String.length "10.1.2.3" in
    let rec has i =
      i + m <= n && (String.sub name i m = "10.1.2.3" || has (i + 1))
    in
    has 0
  in
  ((if off then "off loopback: " ^ name else name), f)

let scenario_list : (string * (wait:(float -> unit) -> net -> string)) list =
  let untimed (name, f) = (name, fun ~wait:_ net -> f net) in
  List.map untimed (List.map routed_off untimed_scenarios)
  @ List.map
      (fun (name, order) -> ("timed: " ^ name, timed_close order))
      timed_cases
  @ ports_cases

let scenarios = List.map fst scenario_list
let timed name = String.starts_with ~prefix:"timed: " name
let off_loopback name = String.starts_with ~prefix:"off loopback: " name
let narrow_ports name = String.starts_with ~prefix:"ports: " name

let scenario ?(wait = fun _ -> ()) net name =
  let f = List.assoc name scenario_list in
  let r = try f ~wait net with e -> "abort " ^ classify e in
  String.map (function ',' | '\n' -> ';' | '\000' -> '0' | c -> c) r
