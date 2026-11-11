(** Review 4's programs: what Linux promises on AF_UNIX, and a client that
    reconnects at once. Each is a workload for {!Eio_net_chaos.check}. *)

val unix_lines : Eio_net_chaos.env -> unit
(** [unix_lines env] runs a server that writes two lines on a Unix-domain
    connection, and a client that, once both writes returned, must read both
    before any error: AF_UNIX queues what a returned write sent. *)
