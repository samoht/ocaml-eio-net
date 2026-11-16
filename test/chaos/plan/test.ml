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

(* The sampled tier draws one candidate in each of [n] runs of neighbours. *)
let draw seed n len =
  Plan.sample (Random.State.make [| seed |]) n (List.init len Fun.id)

let ints = Alcotest.(list int)

let test_sample_small () =
  Alcotest.check ints "all of a short list" [ 0; 1; 2 ] (draw 0 3 3);
  Alcotest.check ints "all of a shorter list" [ 0; 1 ] (draw 0 3 2)

let test_sample_strata () =
  for seed = 0 to 99 do
    let l = draw seed 3 10 in
    Alcotest.(check int) "one per run" 3 (List.length l);
    List.iteri
      (fun i x ->
        if x < i * 10 / 3 || x >= (i + 1) * 10 / 3 then
          Alcotest.failf "seed %d: draw %d is %d, outside its run" seed i x)
      l
  done

let test_sample_seed () =
  Alcotest.check ints "same seed, same draw" (draw 7 10 30) (draw 7 10 30);
  if List.for_all (fun s -> draw s 10 30 = draw 0 10 30) [ 1; 2; 3 ] then
    Alcotest.fail "every seed draws the same"

(* Candidates that alternate between two kinds, as two sockets or a read and
   a write do: every other one meets only the first kind, the draw of seed 0
   meets both, and some seed meets each candidate. *)
let test_sample_period () =
  let kinds l = List.sort_uniq compare (List.map (fun x -> x mod 2) l) in
  Alcotest.check ints "both kinds" [ 0; 1 ] (kinds (draw 0 10 20));
  let met = Array.make 20 false in
  for seed = 0 to 63 do
    List.iter (fun x -> met.(x) <- true) (draw seed 10 20)
  done;
  Array.iteri (fun x m -> if not m then Alcotest.failf "no seed draws %d" x) met

let () =
  Alcotest.run "eio-net.chaos plan"
    [
      ( "plan",
        [
          Alcotest.test_case "planning is linear" `Quick test_cost;
          Alcotest.test_case "a short list is taken whole" `Quick
            test_sample_small;
          Alcotest.test_case "one draw in each run of neighbours" `Quick
            test_sample_strata;
          Alcotest.test_case "a seed fixes the draw" `Quick test_sample_seed;
          Alcotest.test_case "a period hides no candidate" `Quick
            test_sample_period;
        ] );
    ]
