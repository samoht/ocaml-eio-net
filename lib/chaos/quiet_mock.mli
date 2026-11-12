(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected
(** Raised by {!run} when every fiber waits and no timer is due. *)

exception Time_limit
(** Raised by {!run} when its clock would move past the limit. *)

val polls : int
(** [polls] is the turns at one instant in which every runnable fiber had just
    yielded after which time passes: [1_000]. *)

val wait_io : unit -> unit
(** [wait_io ()] yields, as a fiber that waits on I/O that is already ready
    does: {!run} does not count it as a poll. *)

val run :
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  'a) ->
  'a
(** [run ~limit main] calls [main clock mono_clock] in a fresh event loop whose
    clocks start at [0.] and move up to [limit] seconds: to the next timer when
    every fiber waits, and, as real time passes while a program polls, after
    {!polls} turns at one instant in which every runnable fiber had just
    yielded, by a tenth of how long the run has polled so far, 10 ms at least,
    or to the next timer if that comes first. It returns what [main] returns,
    and raises what [main] raises.

    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit if the clock would move past [limit]. *)
