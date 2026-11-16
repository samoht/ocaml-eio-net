module Frame = Frame
module Stream = Stream
module Local = Local
module Tcp = Tcp
module Interface = Interface

let connect_failure = function
  | Eio.Io
      (Eio.Exn.X (Eio_unix.Unix_error ((ENETUNREACH | EHOSTUNREACH), _, _)), _)
    ->
      Some `Unreachable
  | e -> Eio_net_connect.classify e

external pending_accept_errors : unit -> Unix.error list
  = "caml_eio_net_pending_accept_errors"

let pending = pending_accept_errors ()

(* Eio reads ENOPROTOOPT as Invalid_option, which in an accept, that sets no
   option, is the error pending on the new connection. *)
let accept_failure = function
  | Eio.Io (Eio.Net.E Invalid_option, _) -> Some `Aborted
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error ((EMFILE | ENFILE), _, _)), _) ->
      Some `Descriptors
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (e, _, _)), _)
    when List.mem e pending ->
      Some `Aborted
  | e -> Eio_net_accept.classify e
