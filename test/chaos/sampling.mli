(** A workload whose bug only the sampled tier of {!Eio_net_chaos.run} can
    reach. *)

val download : ?bug:[ `None | `Body ] -> body:int -> Eio_net_chaos.env -> unit
(** [download ~bug ~body env] runs a server that sends a head of four lines and
    [body] more, all eight bytes long, then answers the client's [ACK] with
    [BYE]. The client reads each line under a timeout, and gives up on any
    error. With [`Body] it reads the body with none, so a connection that goes
    silent in the body hangs it. The linear tier cuts the stream only around the
    ends of the first three lines and of the last, all read under a timeout. *)
