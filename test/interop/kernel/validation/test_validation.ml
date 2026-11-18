(* Every setsockopt the kernel's traces record, and every getsockopt the
   level of an option refuses, against Validation alone: the kernel, the
   socket and the value decide, as recorded through eio_posix on Linux and
   on macOS. A scenario names the option it sets, and the value unless it is
   the recorder's usual one; a name this table does not know fails. *)

module O = Eio.Net.Sockopt
module U = Eio_unix.Net

(* An option, how the traces print and read its values, and the value a
   scenario that names none sets. *)
type opt =
  | Opt : 'a Eio.Net.Sockopt.t * ('a -> string) * (string -> 'a) * 'a -> opt

let bool = (string_of_bool, bool_of_string)
let int = (string_of_int, int_of_string)
let float = (Eio_net_kernel.float_value, float_of_string)
let str = (Fun.id, Fun.id)

let optint =
  ( (function None -> "none" | Some n -> string_of_int n),
    function "none" -> None | s -> Some (int_of_string s) )

(* "40000-40100", "-1-0" *)
let range =
  ( (fun (l, u) -> Fmt.str "%d-%d" l u),
    fun s ->
      let i = String.index_from s (if s.[0] = '-' then 1 else 0) '-' in
      ( int_of_string (String.sub s 0 i),
        int_of_string (String.sub s (i + 1) (String.length s - i - 1)) ) )

let mtu_discover =
  ( (function
    | `Want -> "want"
    | `Dont -> "dont"
    | `Do -> "do"
    | `Probe -> "probe"),
    function
    | "want" -> `Want
    | "dont" -> `Dont
    | "do" -> `Do
    | "probe" -> `Probe
    | s -> Fmt.failwith "bad mtu_discover %S" s )

let opt o (pp, of_string) v = Opt (o, pp, of_string, v)

[@@@alert "-deprecated"]

let options =
  [
    ("SO_DEBUG", opt O.SO_DEBUG bool true);
    ("SO_BROADCAST", opt O.SO_BROADCAST bool true);
    ("SO_REUSEADDR", opt O.SO_REUSEADDR bool true);
    ("SO_KEEPALIVE", opt O.SO_KEEPALIVE bool true);
    ("SO_DONTROUTE", opt O.SO_DONTROUTE bool true);
    ("SO_OOBINLINE", opt O.SO_OOBINLINE bool true);
    ("TCP_NODELAY", opt O.TCP_NODELAY bool true);
    ("IPV6_ONLY", opt O.IPV6_ONLY bool true);
    ("SO_REUSEPORT", opt O.SO_REUSEPORT bool true);
    ("SO_SNDBUF", opt O.SO_SNDBUF int 10000);
    ("SO_RCVBUF", opt O.SO_RCVBUF int 10000);
    ("SO_RCVLOWAT", opt O.SO_RCVLOWAT int 10);
    ("SO_SNDLOWAT", opt O.SO_SNDLOWAT int 10);
    ("SO_LINGER", opt O.SO_LINGER optint (Some 5));
    ("SO_RCVTIMEO", opt O.SO_RCVTIMEO float 1.5);
    ("SO_SNDTIMEO", opt O.SO_SNDTIMEO float 1.5);
    ("TCP_KEEPIDLE", opt O.TCP_KEEPIDLE int 30);
    ("TCP_KEEPINTVL", opt O.TCP_KEEPINTVL int 5);
    ("TCP_KEEPCNT", opt O.TCP_KEEPCNT int 3);
    ("TCP_CONGESTION", opt O.TCP_CONGESTION str "reno");
    ("TCP_CORK", opt O.TCP_CORK bool true);
    ("TCP_USER_TIMEOUT", opt O.TCP_USER_TIMEOUT int 1000);
    ("TCP_MAXSEG", opt O.TCP_MAXSEG int 1000);
    ("TCP_LINGER2", opt O.TCP_LINGER2 optint (Some 10));
    ("TCP_DEFER_ACCEPT", opt O.TCP_DEFER_ACCEPT int 5);
    ("TCP_SYNCNT", opt O.TCP_SYNCNT int 3);
    ("TCP_WINDOW_CLAMP", opt O.TCP_WINDOW_CLAMP int 10000);
    ("TCP_QUICKACK", opt O.TCP_QUICKACK bool true);
    ("TCP_FASTOPEN", opt O.TCP_FASTOPEN int 5);
    ("IP_FREEBIND", opt O.IP_FREEBIND bool true);
    ("IP_BIND_ADDRESS_NO_PORT", opt O.IP_BIND_ADDRESS_NO_PORT bool true);
    ("IP_LOCAL_PORT_RANGE", opt O.IP_LOCAL_PORT_RANGE range (40000, 40100));
    ("IP_TTL", opt O.IP_TTL int 32);
    ("IP_MTU", opt O.IP_MTU int 1000);
    ("IP_MTU_DISCOVER", opt O.IP_MTU_DISCOVER mtu_discover `Do);
    ("Unix.SO_DEBUG", opt (U.Sockopt_bool SO_DEBUG) bool true);
    ("Unix.SO_BROADCAST", opt (U.Sockopt_bool SO_BROADCAST) bool true);
    ("Unix.SO_REUSEADDR", opt (U.Sockopt_bool SO_REUSEADDR) bool true);
    ("Unix.SO_KEEPALIVE", opt (U.Sockopt_bool SO_KEEPALIVE) bool true);
    ("Unix.SO_DONTROUTE", opt (U.Sockopt_bool SO_DONTROUTE) bool true);
    ("Unix.SO_OOBINLINE", opt (U.Sockopt_bool SO_OOBINLINE) bool true);
    ("Unix.SO_ACCEPTCONN", opt (U.Sockopt_bool SO_ACCEPTCONN) bool true);
    ("Unix.TCP_NODELAY", opt (U.Sockopt_bool TCP_NODELAY) bool true);
    ("Unix.IPV6_ONLY", opt (U.Sockopt_bool IPV6_ONLY) bool true);
    ("Unix.SO_REUSEPORT", opt (U.Sockopt_bool SO_REUSEPORT) bool true);
    ("Unix.SO_SNDBUF", opt (U.Sockopt_int SO_SNDBUF) int 10000);
    ("Unix.SO_RCVBUF", opt (U.Sockopt_int SO_RCVBUF) int 10000);
    ("Unix.SO_ERROR", opt (U.Sockopt_int SO_ERROR) int 0);
    ("Unix.SO_TYPE", opt (U.Sockopt_int SO_TYPE) int 1);
    ("Unix.SO_RCVLOWAT", opt (U.Sockopt_int SO_RCVLOWAT) int 10);
    ("Unix.SO_SNDLOWAT", opt (U.Sockopt_int SO_SNDLOWAT) int 10);
    ("Unix.SO_LINGER", opt (U.Sockopt_optint SO_LINGER) optint (Some 5));
    ("Unix.SO_RCVTIMEO", opt (U.Sockopt_float SO_RCVTIMEO) float 1.5);
    ("Unix.SO_SNDTIMEO", opt (U.Sockopt_float SO_SNDTIMEO) float 1.5);
  ]

[@@@alert "+deprecated"]

let option_named name =
  match List.assoc_opt name options with
  | Some o -> o
  | None -> Fmt.failwith "no option %S" name

(* "TCP_KEEPCNT=128": the option and the value it names, which must print
   back as named. *)
let with_value spec =
  match String.index_opt spec '=' with
  | None -> Fmt.failwith "no value in %S" spec
  | Some i ->
      let name = String.sub spec 0 i in
      let v = String.sub spec (i + 1) (String.length spec - i - 1) in
      let (Opt (o, pp, of_string, _)) = option_named name in
      let x = of_string v in
      if pp x <> v then Fmt.failwith "%S reads back as %S" v (pp x);
      Opt (o, pp, of_string, x)

let errno : Unix.error -> string = function
  | EINVAL -> "EINVAL"
  | ENOTCONN -> "ENOTCONN"
  | EOPNOTSUPP -> "EOPNOTSUPP"
  | ENOENT -> "ENOENT"
  | e -> Unix.error_message e

let backend = function
  | Eio_unix.Unix_error (e, _, _) -> errno e
  | b -> Fmt.str "%a" Eio.Exn.Backend.pp b

(* As the recorder prints what an option raised. *)
let outcome = function
  | Eio.Io (Eio.Net.E Invalid_option, _) -> "Invalid_option"
  | Eio.Io (Eio.Exn.Not_available b, _) -> "Not_available " ^ backend b
  | Eio.Io (Eio.Fs.E (Permission_denied b), _) ->
      "Permission_denied " ^ backend b
  | Eio.Io (Eio.Fs.E (Not_found b), _) -> "Not_found " ^ backend b
  | Eio.Io (Eio.Exn.X b, _) -> backend b
  | Invalid_argument _ -> "Invalid_argument"
  | e -> Printexc.to_string e

let rows () =
  let file = Filename.concat ".." (Filename.concat "traces" "scenarios.csv") in
  match In_channel.with_open_text file In_channel.input_lines with
  | [] -> Fmt.failwith "%s: empty" file
  | header :: lines ->
      let names = String.split_on_char ',' header in
      List.map
        (fun line ->
          let cells = String.split_on_char ',' line in
          if List.compare_lengths cells names <> 0 then
            Fmt.failwith "%s: bad row %S" file line;
          let cell c = List.assoc c (List.combine names cells) in
          (cell "ops", cell "linux", cell "macos", cell "windows eio_windows"))
        lines

let sub_from s i = String.sub s i (String.length s - i)

(* The text after [key] in [s], up to the next field or the end: a field of
   a scenario's outcome. *)
let field key s =
  let n = String.length key in
  let rec find i =
    if i + n > String.length s then None
    else if String.sub s i n = key && (i = 0 || s.[i - 1] = ' ') then
      Some (sub_from s (i + n))
    else find (i + 1)
  in
  let rec stop rest i =
    if i + 5 > String.length rest then rest
    else if List.mem (String.sub rest i 5) [ " get="; " set=" ] then
      String.sub rest 0 i
    else stop rest (i + 1)
  in
  Option.map (fun rest -> stop rest 0) (find 0)

(* The socket a scenario's kind names, as the kernel sees it. *)
let socket ~os ?state kind : Validation.socket =
  let family, which =
    match String.split_on_char ' ' kind with
    | [ f; w ] -> (f, w)
    | [ f ] -> (f, "")
    | _ -> Fmt.failwith "bad kind %S" kind
  in
  let family : Validation.family =
    match family with
    | "tcp4" | "udp4" -> V4
    | "tcp6" | "tcp6any" | "udp6" | "udp6any" -> V6
    | "unix" -> Local
    | f -> Fmt.failwith "bad family %S" f
  in
  let kind : Validation.kind =
    match (state, which) with
    | Some k, _ -> k
    | None, "listener" -> Stream Listening
    | None, "client" -> Stream (Connected Client)
    | None, "server" -> Stream (Connected Server)
    | None, ("dgram" | "") -> Datagram
    | None, w -> Fmt.failwith "bad socket %S" w
  in
  { os; family; kind; maxseg = Validation.default_maxseg family kind }

let set_outcome s (Opt (o, _, _, v)) =
  match Validation.set s o v with
  | Ok () -> "ok"
  | Error r -> outcome (Validation.exn `Set r)

let level_refusal s (Opt (o, _, _, _)) =
  match Validation.get s o with
  | Ok () -> None
  | Error r -> Some (outcome (Validation.exn `Get r))

(* What a scenario that sets an option sets: the socket, the option and
   value, the field of its outcome that holds the set's result, and whether
   a get came first. A [sockopt inherit] scenario sets it on a listener, and
   a [sockopt connect] one on a socket not connected yet. *)
let case ~os name =
  let last s =
    match String.rindex_opt s ' ' with
    | Some i -> (String.sub s 0 i, sub_from s (i + 1))
    | None -> Fmt.failwith "bad scenario %S" name
  in
  let family kind = List.hd (String.split_on_char ' ' kind) in
  let parts prefix = last (sub_from name (String.length prefix + 2)) in
  match String.index_opt name ':' with
  | None -> None
  | Some i -> (
      match String.sub name 0 i with
      | "sockopt" as p ->
          let kind, o = parts p in
          Some (socket ~os kind, option_named o, "set=", true)
      | "sockopt value" as p ->
          let kind, o = parts p in
          Some (socket ~os kind, with_value o, "set=", false)
      | "sockopt inherit" as p ->
          let kind, o = parts p in
          let s = socket ~os ~state:(Stream Listening) (family kind) in
          Some (s, option_named o, "set=", false)
      | "sockopt connect" as p ->
          let kind, o = parts p in
          let s = socket ~os ~state:(Stream Unconnected) (family kind) in
          Some (s, option_named o, "connect=", false)
      | _ -> None)

let differences ~os (name, linux, macos, windows) =
  let kernel =
    match os with Kernel.Linux -> linux | Macos -> macos | Windows -> windows
  in
  match case ~os name with
  | None -> None
  | Some (s, opt, key, probe) ->
      let set =
        match field key kernel with
        | Some k when k = set_outcome s opt -> []
        | k ->
            let k = Option.value k ~default:kernel in
            [ Fmt.str "%s: set %s, kernel %s" name (set_outcome s opt) k ]
      in
      let get =
        match level_refusal s opt with
        | Some r when probe && field "get=" kernel <> Some r ->
            [ Fmt.str "%s: get %s, kernel %s" name r kernel ]
        | _ -> []
      in
      Some (set @ get)

let check os () =
  let cases = List.filter_map (differences ~os) (rows ()) in
  let n = List.length cases in
  (* The traces hold 993 scenarios that set an option. *)
  if n < 993 then Alcotest.failf "only %d scenarios set an option" n;
  match List.concat cases with
  | [] -> ()
  | l ->
      Alcotest.failf "%d of %d differ:@.%s" (List.length l) n
        (String.concat "\n" l)

let suite =
  ( "validation",
    [
      Alcotest.test_case "Linux" `Quick (check Kernel.Linux);
      Alcotest.test_case "macOS" `Quick (check Kernel.Macos);
    ] )
