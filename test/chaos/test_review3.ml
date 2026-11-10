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
      Alcotest.test_case "200 lines of varying length fit the default budget"
        `Quick
        (Expect.passes (check (echo 200)));
      Alcotest.test_case "load-shedding server: correct" `Quick
        (Expect.passes (check (shed ?bug:None)));
      Alcotest.test_case
        "load-shedding server: leaked count, by the sampled tier" `Quick
        (Expect.caught ~tier:"sampled" (check (shed ~bug:`Leak)));
      Alcotest.test_case "framed requests: correct" `Quick
        (Expect.passes (check (rpc ?bug:None)));
      Alcotest.test_case
        "framed requests: truncated last frame taken, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (check (rpc ~bug:`Lenient)));
      Alcotest.test_case "large greetings both ways: correct" `Quick
        (Expect.passes (check greetings));
      Alcotest.test_case "Unix-domain stream ping-pong: correct" `Quick
        (Expect.passes (check unix_stream));
      Alcotest.test_case "Unix-domain datagram ping-pong: correct" `Quick
        (Expect.passes (check unix_datagram));
      Alcotest.test_case "leader lease: correct" `Quick
        (Expect.passes (check (lease ?bug:None)));
      Alcotest.test_case
        "leader lease: counted from the grant's arrival, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (check (lease ~bug:`Receipt)));
      Alcotest.test_case "pipelined batch: correct" `Quick
        (Expect.passes (check pipelined));
    ] )
