open! Hermetic

type Eio.Exn.Backend.t +=
  | Injected_fault = Wrapper.Injected_fault
  | Emfile_fault = Wrapper.Emfile_fault

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

(* A run of [s]: its operations, and why it failed if it did. *)
let exec run (s : Plan.scenario) =
  let trace = ref [] in
  let faults = Plan.faults s in
  let f op =
    let fault = faults op in
    trace := (op, fault) :: !trace;
    fault
  in
  let failed =
    match
      Quiet_mock.run ~limit:3600. @@ fun clock mono_clock ->
      Eio.Switch.run @@ fun sw ->
      let capacity =
        match s.lasting with Some Tiny_buffers -> Some 4096 | _ -> None
      in
      let base = Eio_net_mem.v ?capacity ~sw () in
      let net = Wrapper.v ~clock ~outage:(s.lasting = Some Outage) f base in
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
  (List.rev !trace, failed)

let pp_trace ppf trace =
  List.iter
    (fun (op, fault) ->
      match fault with
      | None -> Fmt.pf ppf "    %a@\n" Op.pp op
      | Some f -> Fmt.pf ppf "    %a  <- %a@\n" Op.pp op Fault.pp f)
    trace

let report ~hash ~tier s trace error =
  Fmt.failwith
    "%s: the workload failed under a fault.@\n\
    \  faults:   %s@\n\
    \  tier:     %s@\n\
    \  error:    %s@\n\
    \  trace:@\n\
     %a  replay:   ~replay:\"net/%s/%s\""
    name (Plan.text s)
    (match tier with `Linear -> "linear" | `Sampled -> "sampled")
    error pp_trace trace hash (Plan.key s)

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
let scenario run ~first ~linear k =
  match find k linear with
  | Some s -> Some (s, `Linear)
  | None -> (
      match find k (Plan.others first) with
      | Some s -> Some (s, `Sampled)
      | None -> (
          match String.index_opt k '+' with
          | None -> None
          | Some i -> (
              match find (String.sub k 0 i) linear with
              | Some ({ rules = [ r ]; _ } as s1) ->
                  let after, _ = exec run s1 in
                  Plan.seconds r ~first ~after
                  |> find k
                  |> Option.map (fun s -> (s, `Sampled))
              | _ -> None)))

let check ?(budget = 2_000) ?replay run =
  let replay = Option.map parse_replay replay in
  let first, failed = exec run Plan.none in
  Option.iter (fail "the run with no fault failed: %s") failed;
  if first = [] then
    fail "the workload makes no network operation, so it tests nothing";
  let again, _ = exec run Plan.none in
  if again <> first then
    fail
      "two runs with no fault made different operations (%s); the workload \
       must be deterministic"
      (diverge first again);
  let hash = digest first in
  let linear = Plan.linear first in
  let attempt tier s =
    let trace, failed = exec run s in
    Option.iter (report ~hash ~tier s trace) failed;
    trace
  in
  match replay with
  | Some (h, _) when h <> hash ->
      fail
        "the replay names first run %s, but the workload's first run is now \
         %s. Run the check without ~replay and paste the new one."
        h hash
  | Some (_, k) -> (
      match scenario run ~first ~linear k with
      | Some (s, tier) -> ignore (attempt tier s : Plan.trace)
      | None ->
          fail "the replay names %s, a scenario this check no longer has" k)
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
