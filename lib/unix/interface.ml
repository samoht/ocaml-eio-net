external addresses_raw : unit -> (string * string * int * bool) list
  = "caml_eio_net_interface_addresses"

external watch_raw : unit -> Unix.file_descr = "caml_eio_net_interface_watch"
external drain : Unix.file_descr -> bool = "caml_eio_net_interface_drain"

let eio f x =
  try f x with Unix.Unix_error (e, fn, arg) -> raise (Eio_unix.Err.v e fn arg)

module Address = struct
  type t = { interface : string; prefix : Ipaddr.Prefix.t; loopback : bool }

  let v ~interface ~prefix ~loopback = { interface; prefix; loopback }
  let interface a = a.interface
  let prefix a = a.prefix
  let is_loopback a = a.loopback

  let equal a0 a1 =
    String.equal a0.interface a1.interface
    && Ipaddr.Prefix.compare a0.prefix a1.prefix = 0
    && Bool.equal a0.loopback a1.loopback

  let pp ppf a = Fmt.pf ppf "%s %a" a.interface Ipaddr.Prefix.pp a.prefix
end

type address = Address.t

module type S = sig
  val addresses : unit -> address list

  type watch

  val watch : sw:Eio.Switch.t -> watch
  val changed : watch -> unit
end

let prefix octets bits =
  if String.length octets = 4 then
    Ipaddr.V4 (Ipaddr.V4.Prefix.make bits (Ipaddr.V4.of_octets_exn octets))
  else Ipaddr.V6 (Ipaddr.V6.Prefix.make bits (Ipaddr.V6.of_octets_exn octets))

let addresses () =
  List.rev_map
    (fun (interface, octets, bits, loopback) ->
      Address.v ~interface ~prefix:(prefix octets bits) ~loopback)
    (eio addresses_raw ())

type watch = Eio_unix.Fd.t

let watch ~sw =
  Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true (eio watch_raw ())

let rec changed watch =
  let changed_now =
    Eio_unix.Fd.use_exn "Eio_net_unix.Interface.changed" watch (fun fd ->
        Eio_unix.await_readable fd;
        eio drain fd)
  in
  if not changed_now then changed watch
