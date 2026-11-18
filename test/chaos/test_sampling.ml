open Sampling

(* A cut inside the body, where one of its 19 lines ends, catches a client
   with no timeout there: a place each line's write made, in the linear
   tier. *)
let check w () = Eio_net_chaos.run w

(* A replay names its scenario, not the draw that found it, and the program
   draws the same entropy under every seed: one found under seed 7 runs
   again with no seed. *)
let test_seed_replay () =
  let w = download ~bug:`Body ~body:19 in
  let found = Expect.report (fun () -> Eio_net_chaos.run ~seed:7 w) in
  let replay =
    Scanf.sscanf
      (Expect.field "replay" found)
      "Eio_net_chaos.run ~replay:%S app" Fun.id
  in
  let again = Expect.report (fun () -> Eio_net_chaos.run ~replay w) in
  Alcotest.(check string)
    "faults"
    (Expect.field "faults" found)
    (Expect.field "faults" again)

let suite =
  ( "sampling",
    [
      Alcotest.test_case "download: correct" `Quick
        (Expect.passes (check (download ~body:19)));
      Alcotest.test_case
        "download: no timeout in the body, by the linear tier, at the end of a \
         line"
        `Quick
        (Expect.caught (check (download ~bug:`Body ~body:19)));
      Alcotest.test_case "a failure found under seed 7 replays with no seed"
        `Quick test_seed_replay;
    ] )
