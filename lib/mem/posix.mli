(** The raw errors eio_posix raises. *)

val bind_error :
  [ `In_use
  | `Not_available
  | `Invalid
  | `Family
  | `Name_too_long of string
  | `No_file of string
  | `Unlink of string ] ->
  exn
(** [bind_error e] is the [Unix.Unix_error] eio_posix raises for [e]:
    [EADDRINUSE], [EADDRNOTAVAIL], [EINVAL] or [EAFNOSUPPORT] from bind(2),
    [ENAMETOOLONG] for a Unix path [sun_path] cannot hold, [ENOENT] for one with
    a NUL past its first byte, and [ENOENT] from the unlink(2) of such a path.
*)

val timed_out : [ `Connect | `Read | `Write ] -> exn
(** [timed_out op] is the [ETIMEDOUT] eio_posix raises for a connect whose
    handshake times out, or for a read or a write on a connection the kernel
    gave up on. *)

val no_buffer_space : unit -> exn
(** [no_buffer_space ()] is the error eio_posix raises for a datagram that finds
    no buffer space on macOS. *)

val sockopt_error :
  [ `Get | `Set ] ->
  [ `No_option
  | `Not_supported
  | `Invalid
  | `Denied
  | `Unknown
  | `Not_connected
  | `Domain ] ->
  exn
(** [sockopt_error op e] is the error eio_posix raises when getsockopt(2) or
    setsockopt(2) fails with [ENOPROTOOPT], [EOPNOTSUPP], [EINVAL], [EACCES],
    [ENOENT], [ENOTCONN] or [EDOM]. *)

(** The options of {!Eio_unix.Net} that {!Eio.Net.Sockopt} does not name. *)
type 'a extra =
  | Acceptconn : bool extra  (** [SO_ACCEPTCONN] *)
  | Error : int extra  (** [SO_ERROR] *)
  | Type : int extra  (** [SO_TYPE] *)

val extra : 'a Eio.Net.Sockopt.t -> 'a extra option
(** [extra o] is the option [o] names, if {!Eio.Net.Sockopt} names none. *)

val canonical : 'a Eio.Net.Sockopt.t -> 'a Eio.Net.Sockopt.t
(** [canonical o] is the option of {!Eio.Net.Sockopt} that [o], an option of
    {!Eio_unix.Net}, names, or [o]. *)
