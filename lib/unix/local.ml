external sun_path_size : unit -> int = "caml_eio_net_sun_path_size"

let max_length = sun_path_size () - 1

type fd = Unix.file_descr

external connect_at_dir : fd -> fd -> string -> unit = "caml_eio_net_connect_at"
external bind_at : fd -> fd -> string -> unit = "caml_eio_net_bind_at"
external private_dir : fd -> string -> fd = "caml_eio_net_private_dir"
external chmod_at : fd -> string -> int -> unit = "caml_eio_net_chmod_at"
external id_at : fd -> string -> bool * int64 * int64 = "caml_eio_net_id_at"
external link_at : fd -> string -> fd -> string -> unit = "caml_eio_net_link_at"

external rename_at : fd -> string -> fd -> string -> unit
  = "caml_eio_net_rename_at"

external unlink_at : fd -> string -> bool -> unit = "caml_eio_net_unlink_at"

type stream = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
type listening = [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t

let eio f =
  try f ()
  with Unix.Unix_error (e, fn, arg) -> raise (Eio_unix.Err.v e fn arg)

let connect ~sw ~net path =
  let s = Fpath.to_string path in
  if String.length s > max_length then
    raise (Eio_unix.Err.v ENAMETOOLONG "connect" s);
  (Eio.Net.connect ~sw net (`Unix s) :> stream)

(* The directory that holds [path], opened through [path]'s own capability, so
   that a confined directory confines its resolution, and [path]'s base name.
   Opening it for reading gives the descriptor the system calls below resolve
   the name from. *)
let parent ~sw path =
  match Eio.Path.split path with
  | Some (dir, name) when name <> "." && name <> ".." -> (
      let flow = Eio.Path.open_in ~sw dir in
      match Eio_unix.Resource.fd_opt flow with
      | Some fd -> (fd, name)
      | None ->
          Fmt.invalid_arg
            "Eio_net_unix.Local: %a is not a directory of the host" Eio.Path.pp
            dir)
  | _ -> Fmt.invalid_arg "Eio_net_unix.Local: %a names no file" Eio.Path.pp path

(* unix-socket.c, unix_sockaddr_init reaches a long path from its directory
   (local_stubs.c). The connect blocks only while the listener's backlog is
   full, on a thread of the pool. *)
let connect_at ~sw ~net:_ path =
  let sock = eio (fun () -> Unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0) in
  match
    Eio.Switch.run ~name:"Local.connect_at" @@ fun dsw ->
    let dir, name = parent ~sw:dsw path in
    Eio_unix.Fd.use_exn "connect_at" dir @@ fun dir ->
    eio @@ fun () ->
    Eio_unix.run_in_systhread ~label:"local-connect" (fun () ->
        connect_at_dir dir sock name)
  with
  | () ->
      Unix.set_nonblock sock;
      (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true sock :> stream)
  | exception e ->
      Unix.close sock;
      raise e

let ignore_unix_error f = try f () with Unix.Unix_error _ -> ()

(* A private directory in [dir], whose name nobody else holds. *)
let rec private_dir_in dir ~attempts =
  let name =
    Fmt.str ".eio-net-%d-%08x" (Unix.getpid ())
      (Random.State.bits (Random.State.make_self_init ()))
  in
  match private_dir dir name with
  | fd -> (name, fd)
  | exception Unix.Unix_error (EEXIST, _, _) when attempts > 1 ->
      private_dir_in dir ~attempts:(attempts - 1)

(* bind(2) makes the socket's file with the modes the process's umask leaves,
   and changing the umask would change them for every thread. The socket is
   bound in a directory of its own, which no other user can enter, its mode
   set there, and it listens before it is linked to [name]: a peer reaches it
   only with its final mode. linkat(2) refuses a name that is taken;
   renameat(2), when [replace], takes the place of a socket. *)
let bind_and_place dir ~replace ~backlog ~perm name =
  let tmp, private_fd = private_dir_in dir ~attempts:16 in
  Fun.protect ~finally:(fun () ->
      ignore_unix_error (fun () -> unlink_at private_fd "s" false);
      Unix.close private_fd;
      ignore_unix_error (fun () -> unlink_at dir tmp true))
  @@ fun () ->
  let sock = Unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  match
    bind_at private_fd sock "s";
    chmod_at private_fd "s" perm;
    let _, dev, ino = id_at private_fd "s" in
    Unix.listen sock backlog;
    let in_use () = raise (Unix.Unix_error (EADDRINUSE, "bind", name)) in
    (match id_at dir name with
    | exception Unix.Unix_error (ENOENT, _, _) -> (
        try link_at private_fd "s" dir name
        with Unix.Unix_error (EEXIST, _, _) -> in_use ())
    | true, _, _ when replace -> rename_at private_fd "s" dir name
    | _ -> in_use ());
    Unix.set_nonblock sock;
    (dev, ino)
  with
  | id -> (sock, id)
  | exception e ->
      Unix.close sock;
      raise e

let listen ?(replace = false) ~sw ~net:_ ~backlog ~perm path =
  let dir, name = parent ~sw path in
  let sock, id =
    Eio_unix.Fd.use_exn "listen" dir @@ fun dir ->
    eio @@ fun () ->
    Eio_unix.run_in_systhread ~label:"local-listen" (fun () ->
        bind_and_place dir ~replace ~backlog ~perm name)
  in
  let socket = Eio_unix.Net.import_socket_listening ~sw ~close_unix:true sock in
  (* Release runs its hooks last registered first, so [dir], opened on [sw]
     before this hook, is still open. *)
  Eio.Switch.on_release sw (fun () ->
      Eio_unix.Fd.use_exn "listen" dir @@ fun dir ->
      ignore_unix_error (fun () ->
          let _, dev, ino = id_at dir name in
          if (dev, ino) = id then unlink_at dir name false));
  (socket :> listening)
