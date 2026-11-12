(* Outside the Hermetic prelude, which refuses Unix: eio_posix raises a raw
   Unix.Unix_error from listen and datagram_socket when bind(2) or the
   sockaddr conversion fails, and this builds the same exception. It reaches
   nothing. It names Eio_unix.Unix_error for a timed-out connect. *)

let bind_error = function
  | `In_use -> Unix.Unix_error (Unix.EADDRINUSE, "bind", "")
  | `Not_available -> Unix.Unix_error (Unix.EADDRNOTAVAIL, "bind", "")
  | `Invalid -> Unix.Unix_error (Unix.EINVAL, "bind", "")
  | `Family -> Unix.Unix_error (Unix.EAFNOSUPPORT, "bind", "")
  | `Name_too_long path -> Unix.Unix_error (Unix.ENAMETOOLONG, "", path)
  | `No_file path -> Unix.Unix_error (Unix.ENOENT, "", path)
  | `Unlink path -> Unix.Unix_error (Unix.ENOENT, "unlink", path)

let timed_out () =
  Eio.Exn.create
    (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, "connect-in-progress", "")))

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

[@@@alert "-deprecated"]

let extra : type a. a Eio.Net.Sockopt.t -> a extra option = function
  | Eio_unix.Net.Sockopt_bool Unix.SO_ACCEPTCONN -> Some Acceptconn
  | Eio_unix.Net.Sockopt_int Unix.SO_ERROR -> Some Error
  | Eio_unix.Net.Sockopt_int Unix.SO_TYPE -> Some Type
  | _ -> None

[@@@alert "+deprecated"]

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
