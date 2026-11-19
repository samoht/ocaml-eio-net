(** The holds sockets of one kind have on each port, kept as they bind and
    close, so a bind checks only the holds on its port. A hold is an address,
    whether its socket has [SO_REUSEADDR], whether it listens, and, once it
    lingers in TIME_WAIT, until when. Holds alike in all but when they end share
    a group, so a port that many connections held costs a bind the groups on it,
    not the connections. On Linux, a connect that binds no address holds its
    port toward its destination. Ports come from the dynamic range, 49152-65535
    (RFC 6335, section 6). *)

type t
(** The holds of one kind of socket. *)

type hold
(** One socket's hold on an address. *)

val v : os:Kernel.os -> ?now:(unit -> Mtime.t) -> unit -> t
(** [v ~os ~now ()] holds nothing. A hold in TIME_WAIT ends on [now]; without
    it, none does. *)

val os : t -> Kernel.os
(** [os t] is the kernel [t] follows. *)

val now : t -> (unit -> Mtime.t) option
(** [now t] is the clock [t] was given. *)

val time_wait : t -> Mtime.Span.t
(** [time_wait t] is how long an end holds its address in TIME_WAIT: 60 seconds
    on Linux, 30 on macOS. *)

val fin_timeout : Mtime.Span.t
(** [fin_timeout] is how long an orphaned end waits for its peer's FIN in
    FIN_WAIT2, 60 seconds, the default [tcp_fin_timeout] of both kernels. *)

val add :
  t ->
  ?wildcard:bool ->
  ?reuse:bool ->
  ?listening:bool ->
  ?connected:bool ->
  ?accepted:bool ->
  ?toward:Eio.Net.Ipaddr.v4v6 * int ->
  Eio.Net.Ipaddr.v4v6 * int ->
  hold
(** [add t ~wildcard ~reuse ~listening ~connected ~accepted ~toward a] holds [a]
    for a socket bound at a wildcard address if [wildcard], with [SO_REUSEADDR]
    if [reuse], that listens if [listening], that is connected if [connected],
    that a listener accepted if [accepted], and on Linux, for a connect that
    binds no address, [toward] its destination. *)

val remove : t -> int -> hold -> unit
(** [remove t port h] lets go of [h] on [port] now. *)

val set_until : hold -> time_wait:bool -> Mtime.t -> unit
(** [set_until h ~time_wait u] lets go of [h] at [u], or later if it already
    would; [time_wait] when it lingers in TIME_WAIT. *)

val in_use : t -> ?reuse:bool -> Eio.Net.Ipaddr.v4v6 * int -> bool
(** [in_use t ~reuse a] is [true] if a bind at [a], with [SO_REUSEADDR] if
    [reuse], conflicts with a hold, as the kernel looks a bind up. *)

val denied : t -> ?reuse:bool -> Eio.Net.Ipaddr.v4v6 * int -> bool
(** [denied t ~reuse a] is [true] under Windows if a bind at [a] with
    [SO_REUSEADDR] meets a socket bound there without it, which Windows refuses
    with [WSAEACCES]. *)

val free : t -> int option
(** [free t] is the lowest port nothing holds, at any address. *)

val free_next : t -> int option
(** [free_next t] is the next port nothing holds, counting up from the last one
    taken and round, as macOS takes TCP ports. *)

val free_toward : t -> Eio.Net.Ipaddr.v4v6 * int -> int option
(** [free_toward t dst] is the port a connect toward [dst] that binds no address
    takes on Linux: the next one down from the last such connect's, round the
    range, that only connects toward other destinations hold, or toward [dst] in
    TIME_WAIT for more than a second since the peer's FIN. *)

val free_down : t -> int option
(** [free_down t] is the highest port nothing holds. *)
