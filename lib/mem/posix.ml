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
