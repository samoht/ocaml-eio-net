(** A client that counts on TCP to notice a server gone silent. Each is a
    workload for {!Eio_net_chaos.check}: three requests, each on a connection of
    its own, tried three times. *)

val keepalive : Eio_net_chaos.env -> unit
(** [keepalive env] sets [SO_KEEPALIVE] with probes after 1 s, every 1 s, 3 of
    them. *)

val user_timeout : Eio_net_chaos.env -> unit
(** [user_timeout env] sets [SO_KEEPALIVE] with probes after 60 s, every 10 s,
    and [TCP_USER_TIMEOUT] at 10 s, and demands that the client is done within
    300 s, a connect that times out included. Without the user timeout, data the
    server never acknowledged would hold it for 948.6 s. Linux only. *)

val nothing : Eio_net_chaos.env -> unit
(** [nothing env] sets no option: a reply that never comes holds its read for
    ever. *)
