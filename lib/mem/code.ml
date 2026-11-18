open! Hermetic

type Eio.Exn.Backend.t +=
  | Closed
  | No_listener
  | Address_in_use
  | No_destination
  | Message_too_long
  | Invalid_port
  | Network_unreachable
  | Address_family
  | Wrong_socket_type
  | Not_connected
  | Address_not_available
  | Name_too_long
  | Invalid_address
  | Broadcast
  | Host_unreachable
  | Exclusive_address
  | Shut_down

let label = function
  | Closed -> Some "Closed"
  | No_listener -> Some "No listener bound"
  | Address_in_use -> Some "Address in use"
  | No_destination -> Some "No destination"
  | Message_too_long -> Some "Message too long"
  | Invalid_port -> Some "Invalid port"
  | Network_unreachable -> Some "Network unreachable"
  | Address_family -> Some "Address family not supported"
  | Wrong_socket_type -> Some "Wrong socket type"
  | Not_connected -> Some "Not connected"
  | Address_not_available -> Some "Address not available"
  | Name_too_long -> Some "File name too long"
  | Invalid_address -> Some "Invalid address"
  | Broadcast -> Some "Broadcast not allowed"
  | Host_unreachable -> Some "No route to host"
  | Exclusive_address -> Some "Address held without SO_REUSEADDR"
  | Shut_down -> Some "Receive side shut down"
  | _ -> None

let () =
  Eio.Exn.Backend.register_pp (fun ppf code ->
      match label code with
      | Some l ->
          Fmt.pf ppf "%s (eio-net.mem)" l;
          true
      | None -> false)

let shut_down_exn () = Eio.Exn.create (Eio.Exn.X Shut_down)
let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)
let address_in_use () = Eio.Exn.create (Eio.Exn.X Address_in_use)
let fail code = raise (Eio.Exn.create (Eio.Exn.X code))
let refused () = raise (Eio.Net.err (Connection_failure (Refused No_listener)))

(* A Unix path no socket binds has no file, as Eio's backends remove a
   listener's file when the switch it listened under finishes. *)
let no_file () = Eio.Fs.err (Not_found No_listener)
