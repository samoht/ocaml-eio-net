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

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Closed ->
        Format.pp_print_string ppf "Closed (eio-net.mem)";
        true
    | No_listener ->
        Format.pp_print_string ppf "No listener bound (eio-net.mem)";
        true
    | Address_in_use ->
        Format.pp_print_string ppf "Address in use (eio-net.mem)";
        true
    | No_destination ->
        Format.pp_print_string ppf "No destination (eio-net.mem)";
        true
    | Message_too_long ->
        Format.pp_print_string ppf "Message too long (eio-net.mem)";
        true
    | Invalid_port ->
        Format.pp_print_string ppf "Invalid port (eio-net.mem)";
        true
    | Network_unreachable ->
        Format.pp_print_string ppf "Network unreachable (eio-net.mem)";
        true
    | Address_family ->
        Format.pp_print_string ppf "Address family not supported (eio-net.mem)";
        true
    | Wrong_socket_type ->
        Format.pp_print_string ppf "Wrong socket type (eio-net.mem)";
        true
    | Not_connected ->
        Format.pp_print_string ppf "Not connected (eio-net.mem)";
        true
    | Address_not_available ->
        Format.pp_print_string ppf "Address not available (eio-net.mem)";
        true
    | Name_too_long ->
        Format.pp_print_string ppf "File name too long (eio-net.mem)";
        true
    | Invalid_address ->
        Format.pp_print_string ppf "Invalid address (eio-net.mem)";
        true
    | Broadcast ->
        Format.pp_print_string ppf "Broadcast not allowed (eio-net.mem)";
        true
    | Host_unreachable ->
        Format.pp_print_string ppf "No route to host (eio-net.mem)";
        true
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)
let address_in_use () = Eio.Exn.create (Eio.Exn.X Address_in_use)
let fail code = raise (Eio.Exn.create (Eio.Exn.X code))

(* A Unix path no socket binds has no file, as Eio's backends remove a
   listener's file when the switch it listened under finishes. *)
let no_file () = Eio.Fs.err (Not_found No_listener)
