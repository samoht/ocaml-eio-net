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
let is_outage (env : env) = Wrapper.is_outage env#net

(* {1 Runner} *)

let name = "Eio_net_chaos.run"
let fail fmt = Fmt.failwith ("%s: " ^^ fmt) name
let plural n = if n = 1 then "" else "s"

(* A run that makes more operations is a busy loop or too large to check; the
   limit also bounds the trace a run keeps. *)
let max_ops = 100_000
let too_large = "a busy loop, or a workload too large for a check"

(* [count] formats [n] in groups of three digits, as prose writes it. *)
let count ppf n =
  let s = string_of_int n in
  let len = String.length s in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Fmt.char ppf ',';
      Fmt.char ppf c)
    s

let ops_message =
  Fmt.str "the run made %a network operations, more than a check plans from: %s"
    count max_ops too_large

let in_kind = function
  | `Connect -> "a connect"
  | `Accept -> "an accept"
  | `Read -> "a read"
  | `Write -> "a write"
  | `Recv -> "a receive"

(* What a hang adds when the workload's own fiber, [root], waits in no
   network operation on other fibers that still run, as a switch whose body
   returned waits on the fibers it forked: those fibers, by what they do. *)
let waits_on net ~(root : Eio.Private.Trace.id) tids =
  let is_root (tid : Eio.Private.Trace.id) =
    Int.equal (tid :> int) (root :> int)
  in
  let others = List.filter (fun tid -> not (is_root tid)) tids in
  let named = 5 in
  match others with
  | [] -> ""
  | _ when not (List.exists is_root tids) -> ""
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
        n (plural n)
        (String.concat "; "
           (List.map fiber (List.filteri (fun i _ -> i < named) others)))
        (if n > named then Fmt.str "; and %d more" (n - named) else "")

(* The environment a workload runs with: [net], on the clocks of its loop. *)
let env net clock mono_clock : env =
  object
    method net = net
    method clock = clock
    method mono_clock = mono_clock
  end

(* The failure a run reports, from [failed] and its network [wrapped]: a
   limit of the wrapper's first, then, when [waiting] says the run hung, the
   fibers the workload's [root] waits on. *)
let explain (s : Plan.scenario) ~wrapped ~root ~waiting ~stopped failed =
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
  match wrapped with
  | Some net when Wrapper.overrun net -> Some (multiplied net ops_message)
  | Some net when Option.is_some waiting ->
      Option.map
        (fun msg ->
          msg ^ waits_on net ~root:(Option.get root) (Option.get waiting))
        failed
  | Some net when Wrapper.livelocked net ->
      Some ("the run hangs: " ^ Wrapper.livelock_message)
  | Some net when stopped -> Option.map (multiplied net) failed
  | _ -> failed

(* Where a run failed: in itself, in [inspect] after it, or in the restart
   that follows it, with what the run with a fault raised, if anything. *)
type stage = Run | Inspect of exn option | Restart of exn option
type failure = { stage : stage; error : string }

(* What a check runs: [setup] before each run, [app], and [inspect] after a
   run with a fault, on a network that follows [os]. *)
type workload = {
  os : [ `Linux | `Macos ];
  setup : env -> unit;
  inspect : env -> unit;
  app : env -> unit;
}

(* An error the network gave, or a timeout it made the program take, which a
   run with a fault may let escape. *)
let rec indeterminate = function
  | Eio.Io _ | End_of_file | Eio.Time.Timeout -> true
  | Eio.Exn.Multiple l ->
      l <> [] && List.for_all (fun (x, _) -> indeterminate x) l
  | _ -> false

(* A run of [s] on a fresh network that [setup] prepared, then, if [s] has a
   fault, [inspect] and [app] again on the same network, healed: why it
   failed if it did. [observe] sees each operation of the run with the fault
   and the fault it met. *)
(* How a run's own loop ended: why it failed, if it did, the fibers that
   waited if it hung, and whether it stopped at a limit. *)
type ended = {
  failed : string option;
  waiting : Eio.Private.Trace.id list option;
  stopped : bool;
}

let in_loop main =
  let failed ?waiting ?(stopped = false) msg =
    { failed = Some msg; waiting; stopped }
  in
  match
    Quiet_mock.run ~effects:{ handle = Posix.systhread } ~limit:3600. main
  with
  | () -> { failed = None; waiting = None; stopped = false }
  | exception Quiet_mock.Deadlock_detected tids ->
      failed ~waiting:tids
        "the run hangs: every fiber waits and no timer is due"
  | exception Quiet_mock.Time_limit ->
      failed "the run hangs: it is still running after an hour of its clock"
  | exception Quiet_mock.Busy ->
      Fmt.kstr (failed ~stopped:true)
        "the run woke %a times without waiting 1 ms or more: a busy loop" count
        Quiet_mock.max_turns
  | exception Quiet_mock.Too_long ->
      Fmt.kstr (failed ~stopped:true)
        "the run took %a turns, more than a check can repeat: %s" count
        Quiet_mock.max_turns too_large
  | exception x -> failed (Printexc.to_string x)

let exec_with w ~observe (s : Plan.scenario) =
  let os = w.os in
  let faulty = s <> Plan.none in
  let faults = Plan.faults s in
  let f op =
    let fault = faults op in
    observe op fault;
    fault
  in
  let wrapped = ref None and root = ref None in
  let stage = ref Run and healed = ref false in
  (* Tiny buffers lower each connection's queue to 32 KiB, or keep the
     kernel's default for its family if that is smaller, until the network
     heals. *)
  let capacity =
    match s.lasting with
    | Some Tiny_buffers ->
        fun c -> if !healed then c else Int.min Plan.tiny_buffer c
    | _ -> Fun.id
  in
  let ended =
    in_loop @@ fun clock mono_clock charge ->
    root :=
      Some
        (Eio.Private.Fiber_context.tid
           (Effect.perform Eio.Private.Effects.Get_context));
    Eio.Switch.run @@ fun sw ->
    let base = Eio_net_mem.Private.v ~os ~capacity ~mono:mono_clock ~sw () in
    let quiet () =
      Wrapper.v ~os ~max_ops ~charge ~clock ~outage:false (fun _ -> None) base
    in
    w.setup (env (quiet ()) clock mono_clock);
    let net =
      Wrapper.v ~os ~max_ops ~split:(Plan.split s) ~charge ~clock
        ~outage:(s.lasting = Some Outage) f base
    in
    wrapped := Some net;
    let restart ended =
      healed := true;
      let net = quiet () in
      wrapped := Some net;
      stage := Inspect ended;
      w.inspect (env net clock mono_clock);
      stage := Restart ended;
      w.app (env net clock mono_clock)
    in
    match w.app (env net clock mono_clock) with
    | () -> if faulty then restart None
    | exception x when faulty && indeterminate x -> restart (Some x)
  in
  explain s ~wrapped:!wrapped ~root:!root ~waiting:ended.waiting
    ~stopped:ended.stopped ended.failed
  |> Option.map (fun error -> { stage = !stage; error })

(* The operations of a run a report shows: all of them, or, for a lasting
   condition, which a one-byte one may multiply up to the limit on turns, the
   first and the last [kept], and how many were left out between. *)
type shown = { head : Plan.trace; left_out : int; tail : Plan.trace }

let kept = 10_000

(* A run of [s]: the operations a report shows, and why it failed if it
   did. *)
let exec_shown w (s : Plan.scenario) =
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
  let failed = exec_with w ~observe s in
  ( {
      head = List.rev !head;
      left_out = !left_out;
      tail = List.of_seq (Queue.to_seq tail);
    },
    failed )

(* A run of a scenario with no lasting condition: all its operations, and why
   it failed if it did. *)
let exec w s =
  let shown, failed = exec_shown w s in
  (shown.head, failed)

(* {1 Reports} *)

(* A read or a write a rule of [rules] cuts short where its fault lands says
   which fault that is. *)
let pp_op rules ppf ((op : Op.t), fault) =
  let cut_by k (r : Plan.rule) =
    Op.equal_kind r.kind op.kind
    && r.socket = op.socket && r.side = op.side
    && r.at = op.offset + k
  in
  let cut =
    match fault with
    | Some (Fault.Short k) -> List.find_opt (cut_by k) rules
    | _ -> None
  in
  match (fault, cut) with
  | None, _ -> Fmt.pf ppf "    %a@\n" Op.pp op
  | Some f, Some r ->
      Fmt.pf ppf "    %a  <- %a, up to where it %a@\n" Op.pp op Fault.pp f
        Fault.pp r.fault
  | Some f, None -> Fmt.pf ppf "    %a  <- %a@\n" Op.pp op Fault.pp f

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

(* The trace of a report, under its heading, if the run made any
   operation. *)
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
              period (plural period) times (plural times) first last
        | Left_out n ->
            Fmt.pf ppf "    ... %d more operation%s, not kept@\n" n (plural n));
        pp (i + 1)
      end
      else
        let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
        let j = skip i in
        Fmt.pf ppf "    ... %d more line%s@\n" (j - i) (plural (j - i));
        pp j)
  in
  if n > 0 then Fmt.pf ppf "  trace:@\n";
  pp 0

(* The report of a scenario whose run, [inspect] or restart failed. *)
let report ?matched ~hash s trace { stage; error } =
  let title, ended =
    match stage with
    | Run -> ("the workload failed under a fault", None)
    | Inspect e -> ("inspect rejected what the fault left", Some e)
    | Restart e ->
        ( "the restart failed: the workload, run again with no fault on the \
           same network, did not return",
          Some e )
  in
  let ended =
    Option.map
      (function
        | None -> "the faulty run returned"
        | Some x -> "the faulty run raised " ^ Printexc.to_string x)
      ended
  in
  Fmt.failwith
    "%s: %s@\n\
    \  faults:   %s@\n\
     %a%a  error:    %s@\n\
     %a  replay:   %s ~replay:\"net/%s/%s\" app"
    name title (Plan.text s)
    Fmt.(option (any "  ended:    " ++ string ++ any "@\n"))
    ended
    Fmt.(option (any "  matched:  " ++ string ++ any "@\n"))
    matched error (pp_trace s.rules) trace name hash (Plan.key s)

let digest (trace : Plan.trace) =
  let ops = List.map fst trace in
  let s = Fmt.str "%a" Fmt.(list ~sep:(any ";") Op.pp) ops in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

(* Where a run with no fault first differs from [first], if it does, seen
   as it runs, so that its operations are not kept. *)
let diverges w (first : Plan.trace) =
  let rest = ref first and diverged = ref None in
  let observe op _ =
    if Option.is_none !diverged then
      match !rest with
      | (x, _) :: tl when x = op -> rest := tl
      | (x, _) :: _ ->
          diverged := Fmt.kstr Option.some "%a, then %a" Op.pp x Op.pp op
      | [] -> diverged := Fmt.kstr Option.some "%a, then nothing" Op.pp op
  in
  ignore (exec_with w ~observe Plan.none : failure option);
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
      Fmt.invalid_arg
        "%s: replay is %S, not a replay string of eio-net: one is \
         net/HASH/SCENARIO, HASH 8 hexadecimal digits and SCENARIO as a \
         failure names it"
        name r

let find k = List.find_opt (fun s -> Plan.key s = k)

(* The scenario a replay names, if the check still has it. *)
let scenario w ~first ~linear k =
  match find k linear with
  | Some _ as s -> s
  | None when k = Plan.key Plan.none -> Some Plan.none
  | None -> (
      match find k (Plan.others first) with
      | Some _ as s -> s
      | None -> (
          match String.index_opt k '+' with
          | None -> None
          | Some i -> (
              match find (String.sub k 0 i) linear with
              | Some ({ rules = [ r ]; _ } as s1) ->
                  let after, _ = exec w s1 in
                  Plan.seconds r ~first ~after |> find k
              | _ -> None)))

(* A failure of the run with no fault, with its operations and the replay
   that runs it again. *)
let first_failed ~hash trace error =
  Fmt.failwith
    "%s: the run with no fault failed: %s@\n\
     %a  replay:   %s ~replay:\"net/%s/%s\" app"
    name error (pp_trace [])
    { head = trace; left_out = 0; tail = [] }
    name hash (Plan.key Plan.none)

(* The run a replay names: [(h, k)], the first run's hash and the
   scenario's key, against the check's first run, of hash [hash]. *)
let replay_one w ~first ~linear ~hash ~attempt (h, k) =
  (* After a fix the first run may differ: the scenario is found by its
     position and fault, which the key names. *)
  let matched =
    if h = hash then None
    else
      Fmt.kstr Option.some
        "the replay named first run %s, and the workload's is now %s" h hash
  in
  match scenario w ~first ~linear k with
  | Some s -> ignore (attempt ?matched s : Plan.trace)
  | None when h = hash ->
      fail "the replay names %s, a scenario this check no longer has" k
  | None ->
      fail
        "the replay names %s, a scenario this check no longer has: its first \
         run was %s and is now %s. Run the check without ~replay."
        k h hash

(* The linear tier, then the sampled one, within [budget] runs. *)
let explore ~budget ~seed ~first ~linear ~attempt =
  let n = List.length linear in
  if n > budget then
    fail
      "the linear tier needs %d runs, more than the budget of %d, and a check \
       never passes on a partial one. Shrink the workload, or raise ~budget."
      n budget;
  let others = Plan.others first in
  let rng = Random.State.make [| seed |] in
  let share classes = Plan.sampled_share ~budget ~linear:n ~classes in
  (* The second faults each run allows, drawn as it ends, so no run's
     operations outlive it: an even share of the tier's. *)
  let singles =
    List.length
      (List.filter (fun (s : Plan.scenario) -> List.length s.rules = 1) linear)
  in
  let quota =
    let sh = share (if others = [] then 1 else 2) in
    (sh + singles - 1) / max 1 singles
  in
  let pairs =
    List.concat_map
      (fun (s : Plan.scenario) ->
        let after = attempt s in
        match s.rules with
        | [ r ] -> Plan.sample rng quota (Plan.seconds r ~first ~after)
        | _ -> [])
      linear
  in
  let classes = List.filter (( <> ) []) [ others; pairs ] in
  let share = share (List.length classes) in
  List.iter
    (fun c ->
      List.iter
        (fun s -> ignore (attempt s : Plan.trace))
        (Plan.sample rng share c))
    classes

let run ?os ?(budget = 2_000) ?(seed = 0) ?replay ?(setup = ignore)
    ?(inspect = ignore) app =
  let os =
    match os with Some `Macos -> `Macos | Some `Linux | None -> `Linux
  in
  if budget < 1 then
    Fmt.invalid_arg "%s: budget is %d, it must be positive" name budget;
  let replay = Option.map parse_replay replay in
  let w = { os; setup; inspect; app } in
  let first, failed = exec w Plan.none in
  Option.iter
    (fun { error; _ } -> first_failed ~hash:(digest first) first error)
    failed;
  if first = [] then
    fail "the workload makes no network operation, so it tests nothing";
  Option.iter
    (fail
       "two runs with no fault made different operations (%s); the workload \
        must be deterministic")
    (diverges w first);
  let hash = digest first in
  let linear = Plan.linear first in
  let attempt ?matched s =
    let shown, failed = exec_shown w s in
    Option.iter (report ?matched ~hash s shown) failed;
    shown.head
  in
  match replay with
  | Some r -> replay_one w ~first ~linear ~hash ~attempt r
  | None -> explore ~budget ~seed ~first ~linear ~attempt

let device = Device.check

module Private = struct
  let device_allowances = Device.allowances
  let device_allowed os net = (Device.replay os net).allowed
end
