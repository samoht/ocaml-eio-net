let errno : Unix.error -> string = function
  | EADDRINUSE -> "EADDRINUSE"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | ENAMETOOLONG -> "ENAMETOOLONG"
  | EDESTADDRREQ -> "EDESTADDRREQ"
  | EMSGSIZE -> "EMSGSIZE"
  | EINVAL -> "EINVAL"
  | ENETUNREACH -> "ENETUNREACH"
  | EAFNOSUPPORT -> "EAFNOSUPPORT"
  | EPROTOTYPE -> "EPROTOTYPE"
  | ENOTCONN -> "ENOTCONN"
  | EPIPE -> "EPIPE"
  | ECONNRESET -> "ECONNRESET"
  | ECONNREFUSED -> "ECONNREFUSED"
  | ENOENT -> "ENOENT"
  | EBADF -> "EBADF"
  | EISCONN -> "EISCONN"
  | EOPNOTSUPP -> "EOPNOTSUPP"
  | ENOBUFS -> "ENOBUFS"
  | e -> Unix.error_message e

(* eio-net.mem's codes, named by the system error each stands for. *)
let mem_errno : Eio.Exn.Backend.t -> string option = function
  | Eio_net_mem.Address_in_use -> Some "EADDRINUSE"
  | Eio_net_mem.No_destination -> Some "EDESTADDRREQ"
  | Eio_net_mem.Not_connected -> Some "ENOTCONN"
  | Eio_net_mem.Host_unreachable -> Some (Unix.error_message Unix.EHOSTUNREACH)
  | Eio_net_mem.Message_too_long -> Some "EMSGSIZE"
  | Eio_net_mem.Invalid_port -> Some "EINVAL"
  | Eio_net_mem.Network_unreachable -> Some "ENETUNREACH"
  | Eio_net_mem.Address_family -> Some "EAFNOSUPPORT"
  | Eio_net_mem.Wrong_socket_type -> Some "EPROTOTYPE"
  | Eio_net_mem.Address_not_available -> Some "EADDRNOTAVAIL"
  | Eio_net_mem.Name_too_long -> Some "ENAMETOOLONG"
  | Eio_net_mem.Invalid_address -> Some "EINVAL"
  | _ -> None

let rec classify = function
  | Eio.Exn.Multiple l ->
      Fmt.str "multiple(%s)"
        (String.concat "+" (List.map (fun (e, _) -> classify e) l))
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> "Reset"
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> "Refused"
  | Eio.Io (Eio.Net.E _, _) -> "Net"
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> "Not_found"
  | Eio.Io (Eio.Fs.E _, _) -> "Fs"
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (e, _, _)), _) -> errno e
  | Eio.Io (Eio.Exn.X b, _) -> (
      match mem_errno b with Some e -> e | None -> "error")
  | Unix.Unix_error (e, _, _) -> errno e
  | End_of_file -> "EOF"
  | Invalid_argument _ -> "Invalid_argument"
  | e -> Printexc.to_string e

exception Blocked

(* One attempt that does not wait: a real socket's backend tries the system
   call before it waits for readiness, and mem completes or waits at once. *)
let now f =
  match
    Eio.Fiber.first
      (fun () -> `Done (f ()))
      (fun () ->
        for _ = 1 to 64 do
          Eio.Fiber.yield ()
        done;
        `Blocked)
  with
  | `Done v -> v
  | `Blocked -> raise Blocked

let outcome f =
  try f () with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Blocked -> "blk"
  | e -> classify e

let unlink path = try Unix.unlink path with Unix.Unix_error _ -> ()

type net = [ `Generic ] Eio.Net.ty Eio.Resource.t

let ip s = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string s)
