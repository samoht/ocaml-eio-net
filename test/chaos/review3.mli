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

val pipelined : Eio_net_chaos.env -> unit
(** [pipelined env] runs a client that writes its whole batch of requests before
    it reads a reply. *)
