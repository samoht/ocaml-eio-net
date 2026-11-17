(* Outside the Hermetic prelude, which refuses Unix: this names the
   Unix.error codes of the errors eio_posix raises. It reaches nothing. *)

(* eio_posix raises bind(2)'s error, and that of the sockaddr conversion,
   through Eio_unix.Err.v. *)
let bind_error = function
  | `In_use -> Eio_unix.Err.v Unix.EADDRINUSE "bind" ""
  | `Not_available -> Eio_unix.Err.v Unix.EADDRNOTAVAIL "bind" ""
  | `Invalid -> Eio_unix.Err.v Unix.EINVAL "bind" ""
  | `Family -> Eio_unix.Err.v Unix.EAFNOSUPPORT "bind" ""
  | `Name_too_long path -> Eio_unix.Err.v Unix.ENAMETOOLONG "" path
  | `No_file path -> Eio_unix.Err.v Unix.ENOENT "" path

let timed_out op =
  let call =
    match op with
    | `Connect -> "connect-in-progress"
    | `Read -> "readv"
    | `Write -> "writev"
  in
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, call, "")))

let no_buffer_space () =
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENOBUFS, "sendmsg", "")))

let sockopt_error op code =
  let code =
    match code with
    | `No_option -> Unix.ENOPROTOOPT
    | `Not_supported -> Unix.EOPNOTSUPP
    | `Invalid -> Unix.EINVAL
    | `Denied -> Unix.EACCES
    | `Unknown -> Unix.ENOENT
    | `Not_connected -> Unix.ENOTCONN
    | `Domain -> Unix.EDOM
  in
  let op = match op with `Get -> "getsockopt" | `Set -> "setsockopt" in
  let e = Eio_unix.Unix_error (code, op, "") in
  (* As eio_posix classifies the error. *)
  match code with
  | Unix.ENOPROTOOPT -> Eio.Net.err Invalid_option
  | Unix.EOPNOTSUPP -> Eio.Exn.create (Eio.Exn.Not_available e)
  | Unix.EACCES -> Eio.Fs.err (Permission_denied e)
  | Unix.ENOENT -> Eio.Fs.err (Not_found e)
  | _ -> Eio.Exn.create (Eio.Exn.X e)

type 'a extra = Acceptconn : bool extra | Error : int extra | Type : int extra

(* Unix deprecates SO_ERROR, but a caller can still pass it through
   Eio_unix.Net.Sockopt_int, and a socket answers it. *)
let is_so_error (o : Unix.socket_int_option) =
  (match o with SO_ERROR -> true | _ -> false) [@alert "-deprecated"]

let extra : type a. a Eio.Net.Sockopt.t -> a extra option = function
  | Eio_unix.Net.Sockopt_bool Unix.SO_ACCEPTCONN -> Some Acceptconn
  | Eio_unix.Net.Sockopt_int o when is_so_error o -> Some Error
  | Eio_unix.Net.Sockopt_int Unix.SO_TYPE -> Some Type
  | _ -> None

let canonical : type a. a Eio.Net.Sockopt.t -> a Eio.Net.Sockopt.t =
  let open Eio.Net.Sockopt in
  function
  | Eio_unix.Net.Sockopt_bool Unix.SO_DEBUG -> SO_DEBUG
  | Eio_unix.Net.Sockopt_bool Unix.SO_BROADCAST -> SO_BROADCAST
  | Eio_unix.Net.Sockopt_bool Unix.SO_REUSEADDR -> SO_REUSEADDR
  | Eio_unix.Net.Sockopt_bool Unix.SO_KEEPALIVE -> SO_KEEPALIVE
  | Eio_unix.Net.Sockopt_bool Unix.SO_DONTROUTE -> SO_DONTROUTE
  | Eio_unix.Net.Sockopt_bool Unix.SO_OOBINLINE -> SO_OOBINLINE
  | Eio_unix.Net.Sockopt_bool Unix.TCP_NODELAY -> TCP_NODELAY
  | Eio_unix.Net.Sockopt_bool Unix.IPV6_ONLY -> IPV6_ONLY
  | Eio_unix.Net.Sockopt_bool Unix.SO_REUSEPORT -> SO_REUSEPORT
  | Eio_unix.Net.Sockopt_int Unix.SO_SNDBUF -> SO_SNDBUF
  | Eio_unix.Net.Sockopt_int Unix.SO_RCVBUF -> SO_RCVBUF
  | Eio_unix.Net.Sockopt_int Unix.SO_RCVLOWAT -> SO_RCVLOWAT
  | Eio_unix.Net.Sockopt_int Unix.SO_SNDLOWAT -> SO_SNDLOWAT
  | Eio_unix.Net.Sockopt_optint Unix.SO_LINGER -> SO_LINGER
  | Eio_unix.Net.Sockopt_float Unix.SO_RCVTIMEO -> SO_RCVTIMEO
  | Eio_unix.Net.Sockopt_float Unix.SO_SNDTIMEO -> SO_SNDTIMEO
  | o -> o
