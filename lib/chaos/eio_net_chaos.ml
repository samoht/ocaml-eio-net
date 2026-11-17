open! Hermetic

type Eio.Exn.Backend.t += Injected_fault = Wrapper.Injected_fault
type env = Eio_chaos.env

module Op = Op
module Fault = Fault

let wrap ?os ~clock f net =
  Wrapper.v ?os ~clock ~outage:false
    (fun op -> Option.map (fun f -> (f, 0)) (f op))
    net

let pair = Wrapper.pair
let is_outage env = Wrapper.is_outage env#net
let network = Network.device

(* Reads and writes draw from states of their own, so the faults of each
   depend on [seed] alone, not on how the two interleave. *)
let seeded ?dribble ?stall ?short_write ?reset_after ?(reset_kind = `Reset)
    ?fail_write_after seed =
  let reads = Random.State.make [| seed; 1 |]
  and writes = Random.State.make [| seed; 2 |] in
  let capped st = function
    | Some n when n > 0 -> Some (Fault.Short (1 + Random.State.int st n))
    | _ -> None
  in
  let reset =
    match reset_kind with `Reset -> Fault.Reset | `Eof -> Fault.Eof
  in
  fun (op : Op.t) ->
    match op.kind with
    | `Read -> (
        match reset_after with
        | Some n when op.offset >= n -> Some reset
        | _ -> (
            match stall with
            | Some n when n > 0 && Random.State.int reads (n + 1) = 0 ->
                Some (Fault.Stall 0.)
            | _ -> capped reads dribble))
    | `Write -> (
        match fail_write_after with
        | Some n when op.offset >= n -> Some Fault.Reset
        | _ -> capped writes short_write)
    | `Connect | `Accept | `Recv -> None

let by_side ~client ~server (op : Op.t) =
  match op.side with `Client -> client op | `Server -> server op

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Eio_chaos.run ~name:"Eio_net_chaos.run" ?os ?budget ?seed ?replay ?setup
    ?inspect
    [ network () ]
    app

let conformance ?os ?under ?budget ?seed ?replay net =
  Conformance.check ?os ?under ?budget ?seed ?replay net

module Private = struct
  let conformance_allowances = Conformance.allowances
  let conformance_defects = Conformance.defect_findings
  let conformance_allowed os net = (Conformance.replay os net).allowed
  let conformance_in_loop = Conformance.check_in_loop
end
