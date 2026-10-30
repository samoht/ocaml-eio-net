(* The processor time of one Interface.addresses, the getifaddrs(3) read a
   gateway without an address watch makes before it opens a flow. *)
let run count =
  let entries = List.length (Eio_net_unix.Interface.addresses ()) in
  let start = Unix.times () in
  for _ = 1 to count do
    ignore (Eio_net_unix.Interface.addresses () : _ list)
  done;
  let stop = Unix.times () in
  let cpu =
    stop.tms_utime -. start.tms_utime +. (stop.tms_stime -. start.tms_stime)
  in
  Fmt.pr "%d reads of %d addresses: %.1f us of processor time per read@." count
    entries
    (cpu /. float_of_int count *. 1e6)

let () = List.iter run [ 1_000; 10_000 ]
