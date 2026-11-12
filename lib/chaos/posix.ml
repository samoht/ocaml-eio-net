(* Outside the Hermetic prelude, which refuses Eio_unix (Hermetic.Unsafe
   restores only Stdlib): this names Eio_unix.Unix_error to build the errors
   eio_posix raises, and the effect of Eio_unix.run_in_systhread, and reaches
   nothing. *)

let error code name =
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (code, name, "")))

let timeout () = error Unix.ETIMEDOUT "connect-in-progress"
let unreachable () = error Unix.ENETUNREACH "connect"
let emfile () = error Unix.EMFILE "accept"

(* The pool is one no systhread starts in: [Eio_unix.run_in_systhread] adds
   its clean-up timer to the pool's queue, which nothing runs. *)
let systhread : type a. a Effect.t -> (unit -> a) option = function
  | Eio_unix.Private.Thread_pool.Run_in_systhread f ->
      Some
        (fun () ->
          let r =
            match f () with
            | v -> Ok v
            | exception ex -> Error (ex, Printexc.get_raw_backtrace ())
          in
          ( r,
            Eio_unix.Private.Thread_pool.create
              ~sleep_q:(Eio_utils.Zzz.create ()) ))
  | _ -> None
