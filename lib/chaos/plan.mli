(** The scenarios of a check, as pure functions of a recorded run. *)

type rule = {
  kind : Op.kind;
  socket : int;
  side : Op.side;
  at : int;  (** The [offset] of the operation the fault lands on. *)
  fault : Fault.t;
}
(** The type for one fault at one position. *)

(** The type for the conditions that last a whole run. *)
type lasting =
  | Short_reads  (** Every read of the first 4 KiB returns one byte. *)
  | Short_writes  (** Every write of the first 4 KiB takes one byte. *)
  | Coalesce  (** Every read waits 5 ms first. *)
  | Tiny_buffers  (** Every connection queues 32 KiB each way. *)
  | Outage  (** Every connect is refused and every datagram lost. *)

type scenario = { rules : rule list; lasting : lasting option }
(** The type for scenarios: the rules fire in order, once each. *)

type trace = (Op.t * Fault.t option) list
(** The type for a run's operations, in order, with the fault each met. *)

val none : scenario
(** [none] is the scenario with no fault. *)

val key : scenario -> string
(** [key s] names [s] in a replay. *)

val text : scenario -> string
(** [text s] says what [s] does, for a report. *)

val linear : trace -> scenario list
(** [linear first] is the linear tier of a check whose first run is [first]:
    each fault alone at each structural position, and each lasting condition. *)

val others : trace -> scenario list
(** [others first] is each fault alone at the positions [linear first] leaves.
*)

val seconds : rule -> first:trace -> after:trace -> scenario list
(** [seconds r ~first ~after] is the pairs of faults whose first is [r] and
    whose second lands after it, in [after], the run [r] alone made. *)

val spread : int -> 'a list -> 'a list
(** [spread n l] is [n] elements of [l], evenly spread, or [l] if it has [n] or
    fewer. *)

val sampled_share : budget:int -> linear:int -> classes:int -> int
(** [sampled_share ~budget ~linear ~classes] is the runs of the sampled tier for
    each of its [classes]: in all at most half of [linear], the runs the linear
    tier took, and at least 200, within [budget]. *)

val faults : scenario -> Op.t -> Fault.t option
(** [faults s op] is the fault [op] meets in a run of [s]. A rule cuts the read
    or write that spans its position short, so the next one starts there.
    [faults s] keeps the rules left to fire, so each run takes its own. *)
