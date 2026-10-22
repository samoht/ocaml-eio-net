let listen ?reuse_addr ?reuse_port ~sw ~backlog net ip port =
  try
    (Eio.Net.listen ?reuse_addr ?reuse_port ~sw ~backlog net (`Tcp (ip, port))
      :> [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t)
  with Unix.Unix_error (e, fn, arg) -> raise (Eio_unix.Err.v e fn arg)
