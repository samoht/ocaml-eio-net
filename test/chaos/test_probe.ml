open Probe

(* [coalesce] gives up on any error, and its bug shows only as a timeout, so
   no fault can fail it: [coalesce_retry] demands success and catches it. *)
let suite =
  ( "probe",
    [
      Alcotest.test_case "one-sided reset: no idle timeout, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" one_sided);
      Alcotest.test_case "one-sided reset: idle timeout" `Quick
        (Expect.passes one_sided_timeout);
      Alcotest.test_case "coalesced: correct" `Quick
        (Expect.passes (coalesce ~bug:false));
      Alcotest.test_case "coalesced: a client that gives up cannot tell" `Quick
        (Expect.passes (coalesce ~bug:true));
      Alcotest.test_case "coalesced, retried: correct" `Quick
        (Expect.passes (coalesce_retry ~bug:false));
      Alcotest.test_case "coalesced, retried: by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (coalesce_retry ~bug:true));
      Alcotest.test_case "stream of 20 chunks" `Quick
        (Expect.passes (stream 20));
      Alcotest.test_case "stream of 60 chunks" `Quick
        (Expect.passes (stream 60));
      Alcotest.test_case "stream of 80 chunks" `Quick
        (Expect.passes (stream 80));
      Alcotest.test_case "stream of 200 chunks" `Quick
        (Expect.passes (stream 200));
    ] )
