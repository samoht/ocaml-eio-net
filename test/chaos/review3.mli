(** Review 3's programs: correct programs that real stacks run, and the bugs
    they hide. Each is a workload for {!Eio_net_chaos.check}. *)

val banner : Eio_net_chaos.env -> unit
(** [banner env] runs an SSH-style banner exchange, where both ends write before
    they read. *)

val pipelined : Eio_net_chaos.env -> unit
(** [pipelined env] runs a client that writes its whole batch of requests before
    it reads a reply. *)
