module Frame = Frame
module Stream = Stream
module Local = Local
module Tcp = Tcp
module Interface = Interface

let connect_failure = function
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> Some `Refused
  | Eio.Io
      (Eio.Exn.X (Eio_unix.Unix_error ((ENETUNREACH | EHOSTUNREACH), _, _)), _)
    ->
      Some `Unreachable
  | _ -> None

let accept_failure = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> Some `Aborted
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error ((EMFILE | ENFILE), _, _)), _) ->
      Some `Descriptors
  | _ -> None

let () = Eio_net_connect.register ~name:"eio-net.unix" connect_failure
let () = Eio_net_accept.register ~name:"eio-net.unix" accept_failure
