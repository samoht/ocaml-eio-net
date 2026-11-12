open! Hermetic

type Eio.Exn.Backend.t += Injected_fault = Wrapper.Injected_fault

type env =
  < net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t >

module Op = Op
module Fault = Fault

let wrap ?os ~clock f net = Wrapper.v ?os ~clock ~outage:false f net
let pair = Wrapper.pair
let outage (env : env) = Wrapper.outage env#net

(* {1 Runner} *)

let name = "Eio_net_chaos.check"
let fail fmt = Fmt.failwith ("%s: " ^^ fmt) name

(* The operations a run may make: a run that makes more spins, or is more
   than a check plans from, and the trace it keeps stays bounded. *)
let max_ops = 100_000
let too_large = "a busy loop, or a workload too large for a check"

let ops_message =
  "the run made 100,000 network operations, more than a check plans from: "
  ^ too_large

let in_kind = function
  | `Connect -> "a connect"
  | `Accept -> "an accept"
  | `Read -> "a read"
  | `Write -> "a write"
  | `Recv -> "a receive"

(* What a hang adds when the workload's own fiber, [root], waits in no
   network operation on other fibers that still run, as a switch whose body
   returned waits on the fibers it forked: those fibers, by what they do. *)
let waits_on net ~root tids =
  let others = List.filter (( <> ) root) tids in
  let named = 5 in
  match others with
  | [] -> ""
  | _ when not (List.mem root tids) -> ""
  | _ when Option.is_some (fst (Wrapper.activity net root)) -> ""
  | _ ->
      let fiber tid =
        match Wrapper.activity net tid with
        | Some k, Some op ->
            Fmt.str "a fiber in %s, last %a" (in_kind k) Op.pp op
        | Some k, None -> Fmt.str "a fiber in %s" (in_kind k)
        | None, Some op ->
            Fmt.str "a fiber in no network operation, last %a" Op.pp op
        | None, None -> "a fiber that made no network operation"
      in
      let n = List.length others in
      Fmt.str
        ". The workload's own fiber waits on %d fiber%s still running, as a \
         switch does once its body returned: %s%s"
        n
        (if n = 1 then "" else "s")
        (String.concat "; "
           (List.map fiber (List.filteri (fun i _ -> i < named) others)))
        (if n > named then Fmt.str "; and %d more" (n - named) else "")

(* A run of [s]: why it failed if it did. [observe] sees each operation and
   the fault it met. *)
let exec_with ~os ~observe run (s : Plan.scenario) =
  let faults = Plan.faults s in
  let f op =
    let fault = faults op in
    observe op fault;
    fault
  in
  let wrapped = ref None and stopped = ref false in
  let root = ref None and waiting = ref None in
  let failed =
    match
      Quiet_mock.run ~limit:3600. @@ fun clock mono_clock charge ->
      root :=
        Some
          (Eio.Private.Fiber_context.tid
             (Effect.perform Eio.Private.Effects.Get_context));
      Eio.Switch.run @@ fun sw ->
      let capacity =
        match s.lasting with
        | Some Tiny_buffers -> Some Plan.tiny_buffer
        | _ -> None
      in
      let base = Eio_net_mem.v ~os ?capacity ~clock:mono_clock ~sw () in
      let net =
        Wrapper.v ~os ~max_ops ~split:(Plan.split s) ~charge ~clock
          ~outage:(s.lasting = Some Outage) f base
      in
      wrapped := Some net;
      run
        object
          method net = net
          method clock = clock
          method mono_clock = mono_clock
        end
    with
    | () -> None
    | exception Quiet_mock.Deadlock_detected tids ->
        waiting := Some tids;
        Some "the run hangs: every fiber waits and no timer is due"
    | exception Quiet_mock.Time_limit ->
        Some "the run hangs: it is still running after an hour of its clock"
    | exception Quiet_mock.Busy_loop ->
        stopped := true;
        Some
          "the run woke 10,000,000 times without waiting 1 ms or more: a busy \
           loop"
    | exception Quiet_mock.Too_many_turns ->
        stopped := true;
        Some
          ("the run took 10,000,000 turns, more than a check can repeat: "
         ^ too_large)
    | exception x -> Some (Printexc.to_string x)
  in
  (* A run stopped at a limit says how many of its operations a one-byte
     condition split. *)
  let multiplied net msg =
    match (s.lasting, Wrapper.splits net) with
    | Some ((Short_reads | Short_writes) as l), n when n > 0 ->
        Fmt.str "%s; one-byte %s multiplied its operations, %d of them" msg
          (if l = Short_reads then "reads" else "writes")
          n
    | _ -> msg
  in
  match !wrapped with
  | Some net when Wrapper.overrun net -> Some (multiplied net ops_message)
  | Some net when Option.is_some !waiting ->
      Option.map
        (fun msg ->
          msg ^ waits_on net ~root:(Option.get !root) (Option.get !waiting))
        failed
  | Some net when Wrapper.livelocked net ->
      Some ("the run hangs: " ^ Wrapper.livelock_message)
  | Some net when !stopped -> Option.map (multiplied net) failed
  | _ -> failed

(* The operations of a run a report shows: all of them, or, for a lasting
   condition, which a one-byte one may multiply up to the limit on turns, the
   first and the last [kept], and how many were left out between. *)
type shown = { head : Plan.trace; left_out : int; tail : Plan.trace }

let kept = 10_000

(* A run of [s]: the operations a report shows, and why it failed if it
   did. *)
let exec_shown ~os run (s : Plan.scenario) =
  let head = ref [] and n = ref 0 in
  let tail = Queue.create () and left_out = ref 0 in
  let observe op fault =
    if Option.is_none s.lasting || !n < kept then begin
      head := (op, fault) :: !head;
      incr n
    end
    else begin
      Queue.push (op, fault) tail;
      if Queue.length tail > kept then begin
        ignore (Queue.pop tail : Op.t * Fault.t option);
        incr left_out
      end
    end
  in
  let failed = exec_with ~os ~observe run s in
  ( {
      head = List.rev !head;
      left_out = !left_out;
      tail = List.of_seq (Queue.to_seq tail);
    },
    failed )

(* A run of a scenario with no lasting condition: all its operations, and why
   it failed if it did. *)
let exec ~os run s =
  let shown, failed = exec_shown ~os run s in
  (shown.head, failed)

(* {1 Reports} *)

(* A read or a write a rule of [rules] cuts short where its fault lands says
   which fault that is. *)
let pp_op rules ppf ((op : Op.t), fault) =
  let cut_by k (r : Plan.rule) =
    r.kind = op.kind && r.socket = op.socket && r.side = op.side
    && r.at = op.offset + k
  in
  match fault with
  | None -> Fmt.pf ppf "    %a@\n" Op.pp op
  | Some (Fault.Short k as f) when List.exists (cut_by k) rules ->
      let r = List.find (cut_by k) rules in
      Fmt.pf ppf "    %a  <- %a, up to where it %a@\n" Op.pp op Fault.pp f
        Fault.pp r.fault
  | Some f -> Fmt.pf ppf "    %a  <- %a@\n" Op.pp op Fault.pp f

(* What a repeated operation keeps: all but its place, socket, offset and
   length. *)
let shape ((op : Op.t), fault) = (op.kind, op.side, op.family, fault)
let socket ((op : Op.t), _) = op.socket

(* The lines of a report: an operation, or a block of [period] operations
   repeated, of which the first and the last are shown. *)
type line =
  | Op of (Op.t * Fault.t option)
  | Repeated of { period : int; times : int; first : int; last : int }
  | Left_out of int

let max_period = 8
let min_times = 3

(* The times the block of [p] operations at [i] repeats from [i]: each
   repeat has the operations of the one before, each on the socket numbered
   [d] more, as a loop that opens a connection each time makes them. *)
let times a i p =
  let n = Array.length a in
  let d = if i + p < n then socket a.(i + p) - socket a.(i) else 0 in
  let rec same j k =
    k = p
    || shape a.(j + k) = shape a.(j + k - p)
       && socket a.(j + k) - socket a.(j + k - p) = d
       && same j (k + 1)
  in
  let rec go r =
    let j = i + (r * p) in
    if j + p <= n && same j 0 then go (r + 1) else r
  in
  go 1

(* Runs of a repeated block collapse to its first and last instances, so a
   run of one-byte operations costs a few lines. A collapse hides two lines
   at least, as one that hides one saves none. *)
let collapse trace =
  let a = Array.of_list trace in
  let n = Array.length a in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let best =
        List.fold_left
          (fun (bp, br) p ->
            let r = times a i p in
            if r >= min_times && r * p > bp * br then (p, r) else (bp, br))
          (1, 1)
          (List.init max_period (fun k -> k + 1))
      in
      match best with
      | p, r when r >= min_times && (r - 2) * p > 1 ->
          let block j = List.init p (fun k -> Op a.(j + k)) in
          let last = i + ((r - 1) * p) in
          let acc =
            List.rev_append
              (block i
              @ [
                  Repeated
                    {
                      period = p;
                      times = r - 2;
                      first = (fst a.(i + p)).index;
                      last = (fst a.(last - 1)).index;
                    };
                ]
              @ block last)
              acc
          in
          go (i + (r * p)) acc
      | _ -> go (i + 1) (Op a.(i) :: acc)
  in
  go 0 []

(* A report shows at most [keep] lines at each end, and those around the
   first fault, where the run leaves the first one. *)
let keep = 100
let context = 10

let pp_trace rules ppf { head; left_out; tail } =
  let lines =
    Array.of_list
      (collapse head
      @ (if left_out > 0 then [ Left_out left_out ] else [])
      @ collapse tail)
  in
  let n = Array.length lines in
  let first_fault =
    let rec find i =
      if i >= n then None
      else
        match lines.(i) with
        | Op (_, Some _) -> Some i
        | Op (_, None) | Repeated _ | Left_out _ -> find (i + 1)
    in
    find 0
  in
  let shown i =
    n <= (2 * keep) + (2 * context) + 1
    || i < keep
    || i >= n - keep
    || match first_fault with Some f -> abs (i - f) <= context | None -> false
  in
  let rec pp i =
    if i < n then (
      if shown i then begin
        (match lines.(i) with
        | Op o -> pp_op rules ppf o
        | Repeated { period; times; first; last } ->
            Fmt.pf ppf
              "    ... the %d operation%s above, %d more time%s, #%d to #%d@\n"
              period
              (if period = 1 then "" else "s")
              times
              (if times = 1 then "" else "s")
              first last
        | Left_out n ->
            Fmt.pf ppf "    ... %d more operation%s, not kept@\n" n
              (if n = 1 then "" else "s"));
        pp (i + 1)
      end
      else
        let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
        let j = skip i in
        Fmt.pf ppf "    ... %d more line%s@\n" (j - i)
          (if j - i = 1 then "" else "s");
        pp j)
  in
  pp 0

let report ?matched ~hash ~tier s trace error =
  Fmt.failwith
    "%s: the workload failed under a fault.@\n\
    \  faults:   %s@\n\
    \  tier:     %s@\n\
     %a  error:    %s@\n\
    \  trace:@\n\
     %a  replay:   ~replay:\"net/%s/%s\""
    name (Plan.text s)
    (match tier with `Linear -> "linear" | `Sampled -> "sampled")
    Fmt.(option (any "  matched:  " ++ string ++ any "@\n"))
    matched error (pp_trace s.rules) trace hash (Plan.key s)

let digest (trace : Plan.trace) =
  let ops = List.map fst trace in
  let s = Fmt.str "%a" Fmt.(list ~sep:(any ";") Op.pp) ops in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

(* Where a run with no fault first differs from [first], if it does, seen
   as it runs, so that its operations are not kept. *)
let diverges ~os run (first : Plan.trace) =
  let rest = ref first and diverged = ref None in
  let observe op _ =
    if Option.is_none !diverged then
      match !rest with
      | (x, _) :: tl when x = op -> rest := tl
      | (x, _) :: _ ->
          diverged := Fmt.kstr Option.some "%a, then %a" Op.pp x Op.pp op
      | [] -> diverged := Fmt.kstr Option.some "%a, then nothing" Op.pp op
  in
  ignore (exec_with ~os ~observe run Plan.none : string option);
  match (!diverged, !rest) with
  | Some d, _ -> Some d
  | None, (x, _) :: _ -> Fmt.kstr Option.some "%a, then nothing" Op.pp x
  | None, [] -> None

(* A replay is [net/HASH/KEY], with the 8 hexadecimal digits of a first
   run's digest and a scenario's key. *)
let parse_replay r =
  let hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  match String.split_on_char '/' r with
  | [ "net"; h; k ]
    when String.length h = 8 && String.for_all hex h && Plan.is_key k ->
      (String.lowercase_ascii h, k)
  | _ ->
      fail
        "~replay:%S is not a replay of eio-net: one is net/HASH/SCENARIO, HASH \
         8 hexadecimal digits and SCENARIO as a failure names it"
        r

let find k = List.find_opt (fun s -> Plan.key s = k)

(* The scenario a replay names, if the check still has it. *)
let scenario ~os run ~first ~linear k =
  match find k linear with
  | Some s -> Some (s, `Linear)
  | None when k = Plan.key Plan.none -> Some (Plan.none, `Linear)
  | None -> (
      match find k (Plan.others first) with
      | Some s -> Some (s, `Sampled)
      | None -> (
          match String.index_opt k '+' with
          | None -> None
          | Some i -> (
              match find (String.sub k 0 i) linear with
              | Some ({ rules = [ r ]; _ } as s1) ->
                  let after, _ = exec ~os run s1 in
                  Plan.seconds r ~first ~after
                  |> find k
                  |> Option.map (fun s -> (s, `Sampled))
              | _ -> None)))

(* A failure of the run with no fault, with its operations and the replay
   that runs it again. *)
let first_failed ~hash trace error =
  Fmt.failwith
    "%s: the run with no fault failed: %s@\n\
    \  trace:@\n\
     %a  replay:   ~replay:\"net/%s/%s\""
    name error (pp_trace [])
    { head = trace; left_out = 0; tail = [] }
    hash (Plan.key Plan.none)

let check ?os ?(budget = 2_000) ?replay run =
  let os =
    match os with Some `Macos -> `Macos | Some `Linux | None -> `Linux
  in
  if budget < 1 then Fmt.invalid_arg "%s: the budget is %d" name budget;
  let replay = Option.map parse_replay replay in
  let first, failed = exec ~os run Plan.none in
  Option.iter (first_failed ~hash:(digest first) first) failed;
  if first = [] then
    fail "the workload makes no network operation, so it tests nothing";
  Option.iter
    (fail
       "two runs with no fault made different operations (%s); the workload \
        must be deterministic")
    (diverges ~os run first);
  let hash = digest first in
  let linear = Plan.linear first in
  let attempt ?matched tier s =
    let shown, failed = exec_shown ~os run s in
    Option.iter (report ?matched ~hash ~tier s shown) failed;
    shown.head
  in
  match replay with
  | Some (h, k) -> (
      (* After a fix the first run may differ: the scenario is found by its
         position and fault, which the key names. *)
      let matched =
        if h = hash then None
        else
          Some
            (Fmt.str
               "the replay named first run %s, and the workload's is now %s" h
               hash)
      in
      match scenario ~os run ~first ~linear k with
      | Some (s, tier) -> ignore (attempt ?matched tier s : Plan.trace)
      | None when h = hash ->
          fail "the replay names %s, a scenario this check no longer has" k
      | None ->
          fail
            "the replay names %s, a scenario this check no longer has: its \
             first run was %s and is now %s. Run the check without ~replay."
            k h hash)
  | None ->
      let n = List.length linear in
      if n > budget then
        fail
          "the linear tier needs %d runs, more than the budget of %d, and a \
           check never passes on a partial one. Shrink the workload, or raise \
           ~budget."
          n budget;
      let others = Plan.others first in
      let share classes = Plan.sampled_share ~budget ~linear:n ~classes in
      (* The second faults each run allows, spread as it ends, so no run's
         operations outlive it: a share of the tier's, evenly. *)
      let singles =
        List.length
          (List.filter
             (fun (s : Plan.scenario) -> List.length s.rules = 1)
             linear)
      in
      let quota =
        let sh = share (if others = [] then 1 else 2) in
        (sh + singles - 1) / max 1 singles
      in
      let pairs =
        List.concat_map
          (fun (s : Plan.scenario) ->
            let after = attempt `Linear s in
            match s.rules with
            | [ r ] -> Plan.spread quota (Plan.seconds r ~first ~after)
            | _ -> [])
          linear
      in
      let classes = List.filter (( <> ) []) [ others; pairs ] in
      let share = share (List.length classes) in
      List.iter
        (fun c ->
          List.iter
            (fun s -> ignore (attempt `Sampled s : Plan.trace))
            (Plan.spread share c))
        classes
