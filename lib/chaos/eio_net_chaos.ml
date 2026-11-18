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
let network = Network.part

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Eio_chaos.Private.run ~name:"Eio_net_chaos.run" ?os ?budget ?seed ?replay
    ?setup ?inspect
    [ network () ]
    app

let device ?os ?under ?budget ?seed ?replay net =
  Device.check ?os ?under ?budget ?seed ?replay net

module Private = struct
  let device_allowances = Device.allowances
  let device_allowed os net = (Device.replay os net).allowed
  let device_in_loop = Device.check_in_loop
end
