external sun_path_size : unit -> int = "caml_eio_net_sun_path_size"

let max_length = sun_path_size () - 1

external connect_at : Unix.file_descr -> Unix.file_descr -> string -> unit
  = "caml_eio_net_connect_at"

(* unix-socket.c, unix_sockaddr_init: a path an address holds is used as it
   is, a longer one from its directory (local_connect.c). The connect
   blocks only while the
   listener's backlog is full, on a thread of the pool. *)
let connect_from_dir path =
  let dir, name = Fpath.split_base path in
  let sock = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  match
    Eio_unix.run_in_systhread ~label:"local-connect" (fun () ->
        let dir =
          Unix.openfile (Fpath.to_string dir)
            [ Unix.O_RDONLY; Unix.O_CLOEXEC ]
            0
        in
        Fun.protect
          ~finally:(fun () -> Unix.close dir)
          (fun () -> connect_at dir sock (Fpath.to_string name)))
  with
  | () ->
      Unix.set_nonblock sock;
      sock
  | exception Unix.Unix_error (e, fn, arg) ->
      Unix.close sock;
      raise (Eio_unix.Err.v e fn arg)

let connect ~sw ~net path =
  let s = Fpath.to_string path in
  if String.length s < sun_path_size () then
    (Eio.Net.connect ~sw net (`Unix s)
      :> [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t)
  else
    (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true
       (connect_from_dir path)
      :> [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t)

let eio f =
  try f ()
  with Unix.Unix_error (e, fn, arg) -> raise (Eio_unix.Err.v e fn arg)

let file_id p =
  let s = Unix.LargeFile.lstat p in
  (s.st_dev, s.st_ino)

(* bind(2) makes the socket's file with the modes the process's umask
   leaves, so the mask is [lnot perm] for the call, as OpenSSH's ssh-agent
   sets it. The lock keeps each bind under its own mask. *)
let umask = Eio.Mutex.create ()

let bind fd ~perm p =
  Eio.Mutex.use_ro umask (fun () ->
      let mask = Unix.umask (0o777 land lnot perm) in
      Fun.protect
        ~finally:(fun () -> ignore (Unix.umask mask))
        (fun () -> Unix.bind fd (Unix.ADDR_UNIX p)))

let listen ~sw ~backlog ~perm path =
  let p = Fpath.to_string path in
  let fd, id =
    eio @@ fun () ->
    (match Unix.lstat p with
    | { st_kind = S_SOCK; _ } -> Unix.unlink p
    | _ -> ()
    | exception Unix.Unix_error (ENOENT, _, _) -> ());
    let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    match bind fd ~perm p with
    | exception e ->
        Unix.close fd;
        raise e
    | () -> (
        match
          let id = file_id p in
          Unix.listen fd backlog;
          Unix.set_nonblock fd;
          id
        with
        | id -> (fd, id)
        | exception e ->
            Unix.close fd;
            Unix.unlink p;
            raise e)
  in
  let socket = Eio_unix.Net.import_socket_listening ~sw ~close_unix:true fd in
  Eio.Switch.on_release sw (fun () ->
      eio @@ fun () ->
      match file_id p with
      | id' -> if id' = id then Unix.unlink p
      | exception Unix.Unix_error (ENOENT, _, _) -> ());
  (socket :> [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t)
