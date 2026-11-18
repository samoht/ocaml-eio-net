open Common

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

(* Socket options: each option's default, a set and the value read back, on
   every kind of socket; options a listener passes to the connections it
   accepts; options given to a connect; edge values; and what the buffer
   sizes change in the queues. *)

type opt = Opt : string * 'a Eio.Net.Sockopt.t * 'a * ('a -> string) -> opt

let b = string_of_bool
let i = string_of_int
let oi = function None -> "none" | Some n -> string_of_int n
let f = Common.float_value

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

let backend = function
  | Eio_unix.Unix_error (e, _, _) -> errno e
  | b -> (
      match mem_errno b with
      | Some e -> e
      | None -> Fmt.str "%a" Eio.Exn.Backend.pp b)

let rec sockopt_error = function
  | Eio.Io (Eio.Net.E Invalid_option, _) -> "Invalid_option"
  | Eio.Io (Eio.Exn.Not_available b, _) -> "Not_available " ^ backend b
  | Eio.Io (Eio.Fs.E (Permission_denied b), _) ->
      "Permission_denied " ^ backend b
  | Eio.Io (Eio.Fs.E (Not_found b), _) -> "Not_found " ^ backend b
  | Eio.Io (Eio.Exn.X b, _) -> backend b
  | Eio.Exn.Multiple ((e, _) :: _) -> sockopt_error e
  | e -> classify e

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

let scenarios =
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
