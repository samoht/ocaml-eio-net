(* A conformance check that fails, its report left uncaught. *)

let reset (op : Eio_net_chaos.Op.t) =
  if op.kind = `Connect && op.family = `Unix && op.offset = 2 then
    Some Eio_net_chaos.Fault.Reset
  else None

let () =
  Eio_net_chaos.conformance (fun ~sw env ->
      Eio_net_chaos.wrap ~clock:env#clock reset
        (Eio_net_mem.v ~clock:env#mono_clock ~sw ()))
