open! Hermetic

type Eio.Exn.Backend.t += Injected_fault = Wrapper.Injected_fault

type env =
  < net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t >

module Op = Op
module Fault = Fault

let wrap ~clock f net = Wrapper.v ~clock ~outage:false f net
let pair = Wrapper.pair
let outage (env : env) = Wrapper.outage env#net

(* {1 Runner} *)

let name = "Eio_net_chaos.check"
let fail fmt = Fmt.failwith ("%s: " ^^ fmt) name

(* The connects, accepts and receives of a run. *)
let arrivals (trace : Plan.trace) =
  List.length
    (List.filter
       (fun ((op : Op.t), _) ->
         match op.kind with
         | `Connect | `Accept | `Recv -> true
         | `Read | `Write -> false)
       trace)

(* A run of [s]: its operations, why it failed if it did, and the connects,
   accepts and receives it made while its clock stood still if they went past
   [bound], which stopped it. *)
let exec ~bound run (s : Plan.scenario) =
  let trace = ref [] in
  let faults = Plan.faults s in
  let f op =
    let fault = faults op in
    trace := (op, fault) :: !trace;
    fault
  in
  let wrapped = ref None in
  let failed =
    match
      Quiet_mock.run ~limit:3600. @@ fun clock mono_clock ->
      Eio.Switch.run @@ fun sw ->
      let capacity =
        match s.lasting with
        | Some Tiny_buffers -> Some Plan.tiny_buffer
        | _ -> None
      in
      let base = Eio_net_mem.v ?capacity ~clock:mono_clock ~sw () in
      let net =
        Wrapper.v ~bound ~clock ~outage:(s.lasting = Some Outage) f base
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
    | exception Quiet_mock.Deadlock_detected ->
        Some "the run hangs: every fiber waits and no timer is due"
    | exception Quiet_mock.Time_limit ->
        Some "the run hangs: it is still running after an hour of its clock"
    | exception x -> Some (Printexc.to_string x)
  in
  let failed, overrun =
    match !wrapped with
    | Some net when Wrapper.livelocked net ->
        (Some ("the run hangs: " ^ Wrapper.livelock_message), None)
    | Some net -> (failed, Wrapper.overrun net)
    | None -> (failed, None)
  in
  (List.rev !trace, failed, overrun)

(* {1 Reports} *)

let pp_op ppf (op, fault) =
  match fault with
  | None -> Fmt.pf ppf "    %a@\n" Op.pp op
  | Some f -> Fmt.pf ppf "    %a  <- %a@\n" Op.pp op Fault.pp f

(* What a repeated operation keeps: all but its place, offset and length. *)
let shape ((op : Op.t), fault) = (op.kind, op.socket, op.side, op.family, fault)

(* The lines of a report: an operation, or a block of [period] operations
   repeated, of which the first and the last are shown. *)
type line =
  | Op of (Op.t * Fault.t option)
  | Repeated of { period : int; times : int; first : int; last : int }

let max_period = 8
let min_times = 3

(* The times the block of [p] operations at [i] repeats from [i]. *)
let times a i p =
  let n = Array.length a in
  let rec same j k =
    k = p || (shape a.(j + k) = shape a.(j + k - p) && same j (k + 1))
  in
  let rec go r =
    let j = i + (r * p) in
    if j + p <= n && same j 0 then go (r + 1) else r
  in
  go 1

(* Runs of a repeated block collapse to its first and last instances, so a
   run of one-byte operations costs a few lines. *)
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
      | p, r when r >= min_times ->
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

let pp_trace ppf trace =
  let lines = Array.of_list (collapse trace) in
  let n = Array.length lines in
  let first_fault =
    let rec find i =
      if i >= n then None
      else
        match lines.(i) with
        | Op (_, Some _) -> Some i
        | Op (_, None) | Repeated _ -> find (i + 1)
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
        | Op o -> pp_op ppf o
        | Repeated { period; times; first; last } ->
            Fmt.pf ppf
              "    ... the %d operation%s above, %d more time%s, #%d to #%d@\n"
              period
              (if period = 1 then "" else "s")
              times
              (if times = 1 then "" else "s")
              first last);
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
    matched error pp_trace trace hash (Plan.key s)

let digest (trace : Plan.trace) =
  let ops = List.map fst trace in
  let s = Fmt.str "%a" Fmt.(list ~sep:(any ";") Op.pp) ops in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

(* The first operation two runs differ at. *)
let rec diverge (a : Plan.trace) (b : Plan.trace) =
  match (a, b) with
  | (x, _) :: a, (y, _) :: b ->
      if x = y then diverge a b else Fmt.str "%a, then %a" Op.pp x Op.pp y
  | (x, _) :: _, [] | [], (x, _) :: _ -> Fmt.str "%a, then nothing" Op.pp x
  | [], [] -> "nothing"

let parse_replay r =
  match String.split_on_char '/' r with
  | [ "net"; h; k ] -> (h, k)
  | _ -> fail "~replay:%S is not a replay of eio-net" r

let find k = List.find_opt (fun s -> Plan.key s = k)

(* The scenario a replay names, if the check still has it. *)
let scenario ~bound run ~first ~linear k =
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
                  let after, _, _ = exec ~bound run s1 in
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
    name error pp_trace trace hash (Plan.key Plan.none)

let check ?(budget = 2_000) ?replay run =
  if budget < 1 then Fmt.invalid_arg "%s: the budget is %d" name budget;
  let replay = Option.map parse_replay replay in
  (* Each connect, accept and receive is at least one scenario of the linear
     tier, so a first run that makes more than [budget] stops there. *)
  let first, failed, overrun = exec ~bound:budget run Plan.none in
  Option.iter
    (fun n ->
      fail
        "the linear tier needs at least %d runs, more than the budget of %d, \
         and a check never passes on a partial one: the run with no fault made \
         that many connects, accepts and receives while its clock stood still, \
         and was stopped there. Shrink the workload, or raise ~budget."
        n budget)
    overrun;
  Option.iter (first_failed ~hash:(digest first) first) failed;
  if first = [] then
    fail "the workload makes no network operation, so it tests nothing";
  let again, _, _ = exec ~bound:budget run Plan.none in
  if again <> first then
    fail
      "two runs with no fault made different operations (%s); the workload \
       must be deterministic"
      (diverge first again);
  let hash = digest first in
  let linear = Plan.linear first in
  (* A run that makes [Wrapper.threshold] more connects, accepts and receives
     while its clock stands still than the run with no fault made in all
     spins, as a client that reconnects at once to a server a fault broke
     does, with no failure to count. *)
  let before = arrivals first in
  let bound = before + Wrapper.threshold in
  let attempt ?matched tier s =
    let trace, failed, overrun = exec ~bound run s in
    let failed =
      match overrun with
      | Some n ->
          Some
            (Fmt.str
               "the run hangs: it made %d connects, accepts and receives while \
                its clock stood still, %d more than the run with no fault made \
                in all"
               n (n - before))
      | None -> failed
    in
    Option.iter (report ?matched ~hash ~tier s trace) failed;
    trace
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
      match scenario ~bound run ~first ~linear k with
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
      let afters = List.map (fun s -> (s, attempt `Linear s)) linear in
      let pairs =
        List.concat_map
          (fun ((s : Plan.scenario), after) ->
            match s.rules with [ r ] -> Plan.seconds r ~first ~after | _ -> [])
          afters
      in
      let classes = List.filter (( <> ) []) [ Plan.others first; pairs ] in
      let share =
        Plan.sampled_share ~budget ~linear:n ~classes:(List.length classes)
      in
      List.iter
        (fun c ->
          List.iter
            (fun s -> ignore (attempt `Sampled s : Plan.trace))
            (Plan.spread share c))
        classes
