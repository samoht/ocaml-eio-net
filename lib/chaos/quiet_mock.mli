(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected of Eio.Private.Trace.id list
(** Raised by {!run} when every fiber waits and no timer is due, with the trace
    ids of the fibers that wait. *)

exception Time_limit
(** Raised by {!run} when its clock would move past the limit. *)

exception Busy_loop
(** Raised by {!run} after {!max_turns} turns with no wait of 1 ms or more. *)

exception Too_many_turns
(** Raised by {!run} after {!max_turns} turns, one of which was a wait of 1 ms
    or more. *)

val max_turns : int
(** [max_turns] is the turns a run may take: [10_000_000]. *)

val run :
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  (unit -> unit) ->
  'a) ->
  'a
(** [run ~limit main] calls [main clock mono_clock charge] in a fresh event loop
    whose clocks start at [0.] and move up to [limit] seconds: by 1 us on each
    turn, a fiber resumed by a yield, an I/O completion or a timer, and to the
    next timer when every fiber waits, which is a wait of 1 ms or more if that
    timer was set for 1 ms or more, or the clock moves that much. [charge ()]
    takes a turn in the fiber that calls it, for work that does not suspend:
    past the limits, that fiber waits for ever and the run stops.
    [Eio_unix.run_in_systhread] runs its function at once, outside any fiber,
    and its caller resumes on its next turn. It returns what [main] returns, and
    raises what [main] raises.

    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit if the clock would move past [limit].
    @raise Busy_loop after {!max_turns} turns with no wait of 1 ms or more.
    @raise Too_many_turns after {!max_turns} turns with such a wait. *)
