open Sampling

(* With 19 body lines the sampled tier has 300 candidates and 100 runs for
   them. Each position of a read carries a reset, an end of stream and a
   silence, in that order, so taking every third candidate would only ever
   reset the body. *)
let check w () = Eio_net_chaos.check w

(* A replay names its scenario, so one found under another seed runs again
   with none. *)
let test_seed_replay () =
  let w = download ~bug:`Body ~body:19 in
  let found = Expect.report (fun () -> Eio_net_chaos.check ~seed:7 w) in
  let replay = Scanf.sscanf (Expect.field "replay" found) "~replay:%S" Fun.id in
  let again = Expect.report (fun () -> Eio_net_chaos.check ~replay w) in
  Alcotest.(check string)
    "faults"
    (Expect.field "faults" found)
    (Expect.field "faults" again);
  Alcotest.(check string) "tier" "sampled" (Expect.field "tier" again)

let suite =
  ( "sampling",
    [
      Alcotest.test_case "download: correct" `Quick
        (Expect.passes (check (download ~body:19)));
      Alcotest.test_case
        "download: no timeout in the body, by the sampled tier, whatever the \
         period of its candidates"
        `Quick
        (Expect.caught ~tier:"sampled" (check (download ~bug:`Body ~body:19)));
      Alcotest.test_case "a failure found under seed 7 replays with no seed"
        `Quick test_seed_replay;
    ] )
