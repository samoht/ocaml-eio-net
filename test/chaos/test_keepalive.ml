open Keepalive

let check ?os w () = Eio_net_chaos.check ?os w

(* The linear tier finds the run hangs. *)
let hangs f () =
  let m = Expect.report f in
  Alcotest.(check string) "tier" "linear" (Expect.field "tier" m);
  let error = Expect.field "error" m in
  if not (String.starts_with ~prefix:"the run hangs" error) then
    Alcotest.failf "not a hang: %s" error

let suite =
  ( "keepalive",
    [
      Alcotest.test_case "keepalive: correct" `Quick
        (Expect.passes (check keepalive));
      Alcotest.test_case "keepalive: correct under macOS" `Quick
        (Expect.passes (check ~os:`Macos keepalive));
      Alcotest.test_case "TCP_USER_TIMEOUT: correct" `Quick
        (Expect.passes (check user_timeout));
      Alcotest.test_case "no option: a silent server hangs it" `Quick
        (hangs (check nothing));
    ] )
