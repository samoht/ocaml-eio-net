open Review3

let check w () = Eio_net_chaos.check w

(* The wrapper stops the spin of a loop that retries a failed accept at once:
   the accept raises [Failure] instead. *)
let spins () =
  match busy_accept () with
  | () -> Alcotest.fail "the busy loop was not caught"
  | exception Failure m ->
      if not (String.ends_with ~suffix:"while its clock stood still" m) then
        Alcotest.failf "failed with %S" m

let suite =
  ( "review3",
    [
      Alcotest.test_case "banner exchange: correct" `Quick
        (Expect.passes (check banner));
      Alcotest.test_case "reconnecting client: correct" `Quick
        (Expect.passes (check (connfail ?bug:None)));
      Alcotest.test_case
        "reconnecting client: retries connection failures only, by the linear \
         tier"
        `Quick
        (Expect.caught ~tier:"linear"
           (check (connfail ~bug:`Connection_failure)));
      Alcotest.test_case "accept loop that spins on EMFILE" `Quick spins;
      Alcotest.test_case "Eio_net_accept.run recovers from EMFILE" `Quick
        (Expect.passes accept_loop);
      Alcotest.test_case "pipelined batch: correct" `Quick
        (Expect.passes (check pipelined));
    ] )
