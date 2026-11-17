(** What the sequences and the scenarios share: the name of an operation's
    outcome, and the network they run on. *)

val errno : Unix.error -> string
(** [errno e] names [e] as the system calls it, such as ["EADDRINUSE"]. *)

val mem_errno : Eio.Exn.Backend.t -> string option
(** [mem_errno code] names eio-net.mem's [code] by the system error it stands
    for, or is [None] for a code of another backend. *)

val classify : exn -> string
(** [classify e] names the error [e], whichever backend raised it. *)

exception Blocked
(** Raised by {!now} for an operation that would wait. *)

val now : (unit -> 'a) -> 'a
(** [now f] is [f ()] if it completes without waiting. Raises {!Blocked} if it
    would wait: a real socket's backend tries the system call before it waits
    for readiness, and mem completes or waits at once. *)

val outcome : (unit -> string) -> string
(** [outcome f] is [f ()], ["blk"] if it would wait, or the error it raised, by
    {!classify}. *)

val unlink : string -> unit
(** [unlink path] removes [path] if it exists. *)

type net = [ `Generic ] Eio.Net.ty Eio.Resource.t
(** A network, real or eio-net.mem. *)

val ip : string -> Eio.Net.Ipaddr.v4v6
(** [ip s] is the address [s] writes in numeric form. *)
