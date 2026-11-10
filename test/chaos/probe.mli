(** Review 2's probes: the one-sided reset, coalesced reads, and the budget. *)

val one_sided : unit -> unit
(** [one_sided ()] waits for a server's handlers, which have no idle timeout, to
    end. *)

val one_sided_timeout : unit -> unit
(** [one_sided_timeout ()] is {!one_sided} with an idle timeout. *)

val coalesce : ?bug:bool -> unit -> unit
(** [coalesce ~bug ()] pipelines two requests to a server that, with [bug],
    takes one request per read; the client gives up on any error. *)

val coalesce_retry : ?bug:bool -> unit -> unit
(** [coalesce_retry ~bug ()] is {!coalesce} with a client that retries and
    demands success. *)

val stream : int -> unit -> unit
(** [stream n ()] downloads [n] chunks of 100 bytes. *)
