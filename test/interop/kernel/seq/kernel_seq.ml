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

let scenario_list : (string * (wait:(float -> unit) -> net -> string)) list =
  let untimed (name, f) = (name, fun ~wait:_ net -> f net) in
  List.map untimed untimed_scenarios
  @ List.map
      (fun (name, order) -> ("timed: " ^ name, timed_close order))
      timed_cases

let scenarios = List.map fst scenario_list
let timed name = String.starts_with ~prefix:"timed: " name

let scenario ?(wait = fun _ -> ()) net name =
  let f = List.assoc name scenario_list in
  let r = try f ~wait net with e -> "abort " ^ classify e in
  String.map (function ',' | '\n' -> ';' | '\000' -> '0' | c -> c) r
