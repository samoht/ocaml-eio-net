(* Planning looks up what each end of a connection did in an index of the
   run, so its work grows with the run, not with the run times its
   connections: a scan of the run for each end looks at every operation
   once per end. *)

let trace n : Plan.trace =
  let op index kind socket side offset length : Op.t =
    { index; kind; socket; side; family = `Inet; offset; length }
  in
  List.concat
    (List.init n (fun i ->
         let s = i + 1 and k = 6 * i in
         [
           op (k + 1) `Connect s `Client i 0;
           op (k + 2) `Accept s `Server i 0;
           op (k + 3) `Write s `Client 0 10;
           op (k + 4) `Read s `Server 0 64;
           op (k + 5) `Write s `Server 0 10;
           op (k + 6) `Read s `Client 0 64;
         ]))
  |> List.map (fun op -> (op, None))

let test_cost () =
  let work n =
    let t = trace n in
    let before = Plan.looked_at () in
    ignore (Plan.linear t : Plan.scenario list);
    ignore (Plan.others t : Plan.scenario list);
    Plan.looked_at () - before
  in
  let small = work 20 and large = work 80 in
  if large > 5 * small then
    Alcotest.failf
      "planning 4 times the connections looked at %d operations, %d times as \
       many"
      large
      (large / max 1 small)

let () =
  Alcotest.run "eio-net.chaos plan"
    [ ("plan", [ Alcotest.test_case "planning is linear" `Quick test_cost ]) ]
