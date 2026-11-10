(** Review 3's programs: correct programs that real stacks run, and the bugs
    they hide. Each is a workload for {!Eio_net_chaos.check}. *)

val banner : Eio_net_chaos.env -> unit
(** [banner env] runs an SSH-style banner exchange, where both ends write before
    they read. *)

val connfail : ?bug:[ `None | `Connection_failure ] -> Eio_net_chaos.env -> unit
(** [connfail ~bug env] runs a client that reconnects after a failed connect.
    With [`Connection_failure] it retries only [Connection_failure]. *)

val busy_accept : unit -> unit
(** [busy_accept ()] runs a server whose accept loop retries a failed accept at
    once, on a network wrapped to fail its second accept with [Fault.Emfile]. *)

val accept_loop : unit -> unit
(** [accept_loop ()] runs [Eio_net_accept.run] on a network wrapped to fail its
    second accept with [Fault.Emfile], with an idle client to shed and a client
    to serve. *)

val echo : int -> Eio_net_chaos.env -> unit
(** [echo n env] echoes [n] lines of varying length. *)

val shed : ?bug:[ `None | `Leak ] -> Eio_net_chaos.env -> unit
(** [shed ~bug env] runs a server that answers BUSY past two requests at once,
    and five clients that retry. With [`Leak] a handler that fails is not
    counted out. *)

val rpc : ?bug:[ `None | `Lenient ] -> Eio_net_chaos.env -> unit
(** [rpc ~bug env] runs length-prefixed requests on one connection. With
    [`Lenient] the client takes a truncated last frame for a whole one. *)

val greetings : Eio_net_chaos.env -> unit
(** [greetings env] runs an exchange where both ends write tens of kilobytes
    before they read. *)

val unix_stream : Eio_net_chaos.env -> unit
(** [unix_stream env] runs a ping-pong over a Unix-domain stream, whose client
    retries only a refusal or a closed connection. *)

val unix_datagram : Eio_net_chaos.env -> unit
(** [unix_datagram env] runs a ping-pong over Unix-domain datagrams, with no
    retry. *)

val pipelined : Eio_net_chaos.env -> unit
(** [pipelined env] runs a client that writes its whole batch of requests before
    it reads a reply. *)
