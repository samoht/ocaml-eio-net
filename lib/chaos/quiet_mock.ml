open! Hermetic

(* A silent copy of Eio_mock's scheduler and clock.

   Copied from eio 1.6, lib_eio/mock/backend.ml and lib_eio/mock/clock.ml,
   trimmed to what Eio_net_chaos.check needs, with these changes:
   - moving the clock prints nothing. Eio_mock's clock calls [traceln] from
     the scheduler, where no fiber binding reaches, so a check built on it
     cannot keep stderr clean. A quiet mock clock belongs upstream; this copy
     stands in until then;
   - the scheduler stops instead of moving the clock past [limit], so a run
     that every fiber waits in reports as a hang, and one that keeps a timer
     going reports as a run past its time limit;
   - every turn of the scheduler, a fiber resumed by a yield, an I/O
     completion or a timer, moves the clock on by 1 us, as real time passes
     while a program runs, and a run stops after [max_turns] turns;
   - [charge] takes a turn for a call that does not suspend, as a network
     operation does, so a loop of them moves the clock and meets the limits;
   - [Eio_unix.run_in_systhread] runs its function at once, in the
     scheduler, so a run stays deterministic; the fiber resumes on its next
     turn, as it would once the systhread is done.

   A loop run by the library reopens commit 495b8c2, which removed one along
   with the eio.mock dependency: hang detection and deterministic virtual time
   need a loop the check owns. This copy needs no eio.mock.

   Copyright (C) 2021 Anil Madhavapeddy
   Copyright (C) 2022 Thomas Leonard

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
   IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace
module Lf_queue = Eio_utils.Lf_queue
module Suspended = Eio_utils.Suspended

exception Deadlock_detected
exception Time_limit
exception Busy_loop
exception Too_many_turns

(* {1 Clock} *)

module Mono = struct
  module Key = struct
    type t = < >

    let compare = compare
  end

  module Job = struct
    type t = { time : Mtime.t; since : Mtime.t; resolver : unit Eio.Promise.u }

    let compare a b = Mtime.compare a.time b.time
  end

  module Q = Psq.Make (Key) (Job)

  module Impl = struct
    type time = Mtime.t
    type t = { mutable now : Mtime.t; mutable q : Q.t }

    let now t = t.now

    let sleep_until t time =
      if Mtime.compare time t.now <= 0 then Eio.Fiber.yield ()
      else
        let p, r = Eio.Promise.create () in
        let k = object end in
        t.q <- Q.add k { time; since = t.now; resolver = r } t.q;
        try Eio.Promise.await p
        with Eio.Cancel.Cancelled _ as ex ->
          t.q <- Q.remove k t.q;
          raise ex

    let set_time t time =
      let rec drain () =
        match Q.min t.q with
        | Some (_, v) when Mtime.compare v.time time <= 0 ->
            Eio.Promise.resolve v.resolver ();
            t.q <- Option.get (Q.rest t.q);
            drain ()
        | _ -> ()
      in
      drain ();
      t.now <- time

    (* Eio_mock's [try_advance], which does not move past [limit]. It says
       whether the timer it moves to waited 1 ms or more. *)
    let try_advance t ~limit =
      match Q.min t.q with
      | None -> `Idle
      | Some (_, v) when Mtime.compare v.time limit > 0 -> `Past_limit
      | Some (_, v) ->
          let waited =
            Mtime.Span.compare (Mtime.span v.since v.time) Mtime.Span.ms >= 0
            || Mtime.Span.compare (Mtime.span t.now v.time) Mtime.Span.ms >= 0
          in
          set_time t v.time;
          `Advanced waited
  end

  let handler = Eio.Time.Pi.clock (module Impl)
end

module Wall = struct
  type t = Mono.Impl.t
  type time = float

  let wall_of_mtime m = Int64.to_float (Mtime.to_uint64_ns m) /. 1e9
  let wall_to_mtime w = Mtime.of_uint64_ns (Int64.of_float (w *. 1e9))
  let now t = wall_of_mtime (Mono.Impl.now t)
  let sleep_until t time = Mono.Impl.sleep_until t (wall_to_mtime time)
end

let wall_handler = Eio.Time.Pi.clock (module Wall)

(* {1 Scheduler} *)

type exit = [ `Exit_scheduler ]

type t = {
  run_q : (unit -> exit) Lf_queue.t;
  clock : Mono.Impl.t;
  limit : Mtime.t;
  mutable past_limit : bool;
  mutable turns : int;
  mutable waited : bool;
}

(* What a turn costs: a few microseconds of work between two suspensions, as
   a program does on eio_posix. *)
let turn = Mtime.Span.(1 * us)
let max_turns = 10_000_000

(* A turn taken in a fiber: past the limits, the fiber waits for ever, and
   the scheduler stops at its next turn. *)
let charge t () =
  t.turns <- t.turns + 1;
  let now = Option.get (Mtime.add_span t.clock.now turn) in
  let past = Mtime.compare now t.limit > 0 in
  if t.turns > max_turns || past then begin
    if past then t.past_limit <- true;
    Eio.Cancel.protect (fun () ->
        Eio.Promise.await (fst (Eio.Promise.create ())))
  end
  else Mono.Impl.set_time t.clock now

let rec schedule t : exit =
  match Lf_queue.pop t.run_q with
  | Some f ->
      t.turns <- t.turns + 1;
      let now = Option.get (Mtime.add_span t.clock.now turn) in
      if t.turns > max_turns then `Exit_scheduler
      else if Mtime.compare now t.limit > 0 then begin
        t.past_limit <- true;
        `Exit_scheduler
      end
      else begin
        Mono.Impl.set_time t.clock now;
        f ()
      end
  | None -> (
      match Mono.Impl.try_advance t.clock ~limit:t.limit with
      | `Advanced waited ->
          if waited then t.waited <- true;
          schedule t
      | `Idle -> `Exit_scheduler
      | `Past_limit ->
          t.past_limit <- true;
          `Exit_scheduler)

let run ~limit main =
  let clock = { Mono.Impl.now = Mtime.of_uint64_ns 0L; q = Mono.Q.empty } in
  let wall : float Eio.Time.clock_ty Eio.Resource.t =
    Eio.Resource.T (clock, wall_handler)
  in
  let mono : Eio.Time.Mono.ty Eio.Resource.t =
    Eio.Resource.T (clock, Mono.handler)
  in
  let t =
    {
      run_q = Lf_queue.create ();
      clock;
      limit = Wall.wall_to_mtime limit;
      past_limit = false;
      turns = 0;
      waited = false;
    }
  in
  let rec fork ~new_fiber:fiber fn =
    Trace.fiber (Fiber_context.tid fiber);
    Effect.Deep.match_with fn ()
      {
        retc =
          (fun () ->
            Fiber_context.destroy fiber;
            schedule t);
        exnc =
          (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex bt);
        effc =
          (fun (type a)
            (e : a Effect.t)
            :
            ((a, exit) Effect.Deep.continuation -> exit) option
          ->
            match e with
            | Eio.Private.Effects.Suspend f ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    f fiber (fun result ->
                        Lf_queue.push t.run_q (fun () ->
                            Fiber_context.clear_cancel_fn fiber;
                            match result with
                            | Ok v -> Suspended.continue k v
                            | Error ex -> Suspended.discontinue k ex));
                    schedule t)
            | Eio.Private.Effects.Fork (new_fiber, f) ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    Lf_queue.push_head t.run_q (fun () ->
                        Suspended.continue k ());
                    fork ~new_fiber f)
            | Eio.Private.Effects.Get_context ->
                Some (fun k -> Effect.Deep.continue k fiber)
            | e ->
                Option.map
                  (fun run k ->
                    let v = run () in
                    let k = { Suspended.k; fiber } in
                    Lf_queue.push t.run_q (fun () -> Suspended.continue k v);
                    schedule t)
                  (Posix.systhread e));
      }
  in
  let new_fiber = Fiber_context.make_root () in
  let result = ref None in
  let (`Exit_scheduler : exit) =
    Domain_local_await.using ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
        fork ~new_fiber (fun () -> result := Some (main wall mono (charge t))))
  in
  match !result with
  | Some x -> x
  | None ->
      if t.turns > max_turns then
        raise (if t.waited then Too_many_turns else Busy_loop)
      else if t.past_limit then raise Time_limit
      else raise Deadlock_detected
