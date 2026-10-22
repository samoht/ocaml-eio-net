(** TCP sockets. *)

val listen :
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  sw:Eio.Switch.t ->
  backlog:int ->
  _ Eio.Net.t ->
  Eio.Net.Ipaddr.v4v6 ->
  int ->
  [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t
(** [listen ~sw ~backlog net ip port] is {!Eio.Net.listen} on [`Tcp (ip, port)],
    but a bind(2) or listen(2) that fails, which Eio's POSIX backend raises as
    [Unix.Unix_error], is the {!Eio.Io} error {!Eio_unix.Err.v} makes of it: a
    port in use is [EADDRINUSE], unclassified. *)
