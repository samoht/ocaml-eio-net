(** A broadcaster whose subscriber goes silent. Each is a workload for
    {!Eio_net_chaos.check}. *)

val broadcast : ?bug:[ `None | `Sync ] -> Eio_net_chaos.env -> unit
(** [broadcast ~bug env] streams 1.25 MiB to each of three subscribers over TCP,
    more than a connection queues under macOS, and demands that one gets all of
    it. Each subscriber gives up and closes after 8 s with no byte. The
    broadcaster queues two frames per subscriber and shuts down one whose queue
    stays full for 1 s, or whose writer is not done 1 s after the last frame.
    With [`Sync] it writes to each subscriber in turn, so a silent subscriber
    blocks it until that subscriber's close reaches it, which a silent
    connection never sends. *)
