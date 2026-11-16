open Sampling

(* With 19 body lines the sampled tier has 300 candidates and 100 runs for
   them. Each position of a read carries a reset, an end of stream and a
   silence, in that order, so taking every third candidate would only ever
   reset the body. *)
let check w () = Eio_net_chaos.check w

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
    ] )
