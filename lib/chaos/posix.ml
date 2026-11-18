(* Outside the Hermetic prelude, which refuses Eio_unix (Hermetic.Unsafe
   restores only Stdlib): this names Eio_unix.Unix_error to build the errors
   eio_posix raises, and reaches nothing. *)

let error code name =
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (code, name, "")))

let timeout () = error Unix.ETIMEDOUT "connect-in-progress"
let unreachable () = error Unix.ENETUNREACH "connect"
let emfile () = error Unix.EMFILE "accept"
