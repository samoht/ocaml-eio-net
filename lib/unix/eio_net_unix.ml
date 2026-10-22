module Frame = Frame
module Stream = Stream
module Local = Local
module Tcp = Tcp

let connect_failure = function
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> Some `Refused
  | Eio.Io
      (Eio.Exn.X (Eio_unix.Unix_error ((ENETUNREACH | EHOSTUNREACH), _, _)), _)
    ->
      Some `Unreachable
  | _ -> None

let () = Eio_net_connect.register ~name:"eio-net.unix" connect_failure
