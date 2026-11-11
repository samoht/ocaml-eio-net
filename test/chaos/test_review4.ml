open Review4

let check w () = Eio_net_chaos.check w

(* A report collapses the runs of an operation it repeats: thousands of
   one-byte operations cost a few lines, and its replay still runs that
   scenario. *)
let bounded_report () =
  let m = Expect.report (check (echo ~bug:`Ignore_short)) in
  let lines = List.length (String.split_on_char '\n' m) in
  if lines > 200 then Alcotest.failf "the report has %d lines" lines;
  let replay = Scanf.sscanf (Expect.field "replay" m) "~replay:%S" Fun.id in
  let again =
    Expect.report (fun () ->
        Eio_net_chaos.check ~replay (echo ~bug:`Ignore_short))
  in
  Alcotest.(check string) "the replay fails the same way" m again

let suite =
  ( "review4",
    [
      Alcotest.test_case "AF_UNIX lines read before a cut: correct" `Quick
        (Expect.passes (check unix_lines));
      Alcotest.test_case "echo: correct" `Quick (Expect.passes (check echo));
      Alcotest.test_case "echo: ignored count, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (check (echo ~bug:`Ignore_short)));
      Alcotest.test_case "a long report is bounded" `Quick bounded_report;
    ] )
