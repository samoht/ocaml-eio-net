(** What a check is expected to do with a workload. *)

val field : string -> string -> string
(** [field name report] is the [name] line of a check's failure report. *)

val report : (unit -> unit) -> string
(** [report f] is the failure report of the check [f], which must fail. *)

val caught : tier:string -> (unit -> unit) -> unit -> unit
(** [caught ~tier f ()] tests that the check [f] fails under a fault found by
    [tier], ["linear"] or ["sampled"]. *)

val pinned : (unit -> unit) -> unit -> unit
(** [pinned f ()] tests that the pinned test [f] fails, with [Failure] or
    [Eio.Time.Timeout]. *)

val passes : (unit -> unit) -> unit -> unit
(** [passes f ()] tests that [f] returns. *)
