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
