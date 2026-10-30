external addresses_raw : unit -> (string * string * int * bool) list
  = "caml_eio_net_interface_addresses"

external watch_raw : unit -> Unix.file_descr = "caml_eio_net_interface_watch"
external drain : Unix.file_descr -> bool = "caml_eio_net_interface_drain"

type address = { interface : string; prefix : Ipaddr.Prefix.t; loopback : bool }

let prefix octets bits =
  if String.length octets = 4 then
    Ipaddr.V4 (Ipaddr.V4.Prefix.make bits (Ipaddr.V4.of_octets_exn octets))
  else Ipaddr.V6 (Ipaddr.V6.Prefix.make bits (Ipaddr.V6.of_octets_exn octets))

let addresses () =
  List.rev_map
    (fun (interface, octets, bits, loopback) ->
      { interface; prefix = prefix octets bits; loopback })
    (addresses_raw ())

type watch = Eio_unix.Fd.t

let watch ~sw =
  Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true (watch_raw ())

let rec changed watch =
  let changed_now =
    Eio_unix.Fd.use_exn "Eio_net_unix.Interface.changed" watch (fun fd ->
        Eio_unix.await_readable fd;
        drain fd)
  in
  if not changed_now then changed watch
