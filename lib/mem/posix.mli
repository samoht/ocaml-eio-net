(** The raw errors eio_posix raises from [listen] and [datagram_socket]. *)

val bind_error :
  [ `In_use | `Not_available | `Invalid | `Name_too_long of string ] -> exn
(** [bind_error e] is the [Unix.Unix_error] eio_posix raises for [e]:
    [EADDRINUSE], [EADDRNOTAVAIL] or [EINVAL] from bind(2), or [ENAMETOOLONG]
    for a Unix path [sun_path] cannot hold. *)
