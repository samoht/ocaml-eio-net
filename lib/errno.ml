(* Outside the Hermetic prelude, which refuses Eio_unix: this names the
   Unix.error codes of the errors Eio's backends raise. It reaches nothing. *)

external pending_unnamed : unit -> Unix.error list
  = "caml_eio_net_pending_accept_errors"

let pending_unnamed = pending_unnamed ()

let code = function
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (e, _, _)), _) -> Some e
  | _ -> None

let accept exn =
  match code exn with
  | Some (EMFILE | ENFILE) -> Some `Descriptors
  | Some (ENETDOWN | ENOPROTOOPT | EHOSTDOWN | EHOSTUNREACH | ENETUNREACH) ->
      Some `Aborted
  | Some e when List.mem e pending_unnamed -> Some `Aborted
  | _ -> None

let connect exn =
  match code exn with
  | Some (ENETUNREACH | EHOSTUNREACH) -> Some `Unreachable
  | _ -> None
