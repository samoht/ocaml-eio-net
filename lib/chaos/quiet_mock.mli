(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected
(** Raised by {!run} when every fiber waits and no timer is due. *)

exception Time_limit
(** Raised by {!run} when its clock would move past the limit. *)

exception Busy_loop
(** Raised by {!run} after {!max_turns} turns with no wait of 1 ms or more. *)

val max_turns : int
(** [max_turns] is the turns a run may take in a row with no wait of 1 ms or
    more: [10_000_000]. *)

val run :
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  'a) ->
  'a
(** [run ~limit main] calls [main clock mono_clock] in a fresh event loop whose
    clocks start at [0.] and move up to [limit] seconds: by 1 us on each turn, a
    fiber resumed by a yield, an I/O completion or a timer, and to the next
    timer when every fiber waits, which ends a run of turns if that timer was
    set for 1 ms or more, or the clock moves that much. It returns what [main]
    returns, and raises what [main] raises.

    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit if the clock would move past [limit].
    @raise Busy_loop after {!max_turns} turns in a row. *)
