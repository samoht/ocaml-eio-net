(* What a check is expected to do with a workload. *)

let field name report =
  let prefix = "  " ^ name ^ ":" in
  String.split_on_char '\n' report
  |> List.find_map (fun l ->
      if String.starts_with ~prefix l then
        let n = String.length prefix in
        Some (String.trim (String.sub l n (String.length l - n)))
      else None)
  |> Option.value ~default:""

let report f =
  match f () with
  | () -> Alcotest.fail "the check passed: it did not catch the bug"
  | exception Failure m -> m

let caught ~tier f () =
  let m = report f in
  if not (String.starts_with ~prefix:"Eio_net_chaos.check: the workload" m) then
    Alcotest.failf "the check failed for another reason: %s" m;
  Alcotest.(check string) "tier" tier (field "tier" m)

let pinned f () =
  match f () with
  | () -> Alcotest.fail "the pinned test passed: it did not catch the bug"
  | exception (Failure _ | Eio.Time.Timeout) -> ()

let passes f () = f ()
