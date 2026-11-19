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
