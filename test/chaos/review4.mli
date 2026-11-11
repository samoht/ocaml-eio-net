(** Review 4's programs: what Linux promises on AF_UNIX, and a client that
    reconnects at once. Each is a workload for {!Eio_net_chaos.check}. *)

val unix_lines : Eio_net_chaos.env -> unit
(** [unix_lines env] runs a server that writes two lines on a Unix-domain
    connection, and a client that, once both writes returned, must read both
    before any error: AF_UNIX queues what a returned write sent. *)

val echo : ?bug:[ `None | `Ignore_short ] -> Eio_net_chaos.env -> unit
(** [echo ~bug env] echoes 4 KiB. With [`Ignore_short] the server ignores the
    count [single_write] returns, so a failure's trace runs to thousands of
    one-byte operations. *)

val unix_upload : ?bug:[ `None | `Ignore_short ] -> Eio_net_chaos.env -> unit
(** [unix_upload ~bug env] sends 300,000 bytes on a Unix-domain connection. With
    [`Ignore_short] the client ignores the count of its one [single_write]. *)

val unix_written : Eio_net_chaos.env -> unit
(** [unix_written env] runs a server that writes two lines on a Unix-domain
    connection, and a client that reads only once both writes returned, so it
    must read both: AF_UNIX queues what a returned write sent, even from a peer
    that then stops. *)

val unix_ask : ?bug:[ `None | `No_timeout ] -> Eio_net_chaos.env -> unit
(** [unix_ask ~bug env] runs one request and its reply on a Unix-domain
    connection. With [`No_timeout] the client waits for the reply with no
    timeout, so a server that stops hangs it. *)
