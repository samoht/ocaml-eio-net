type Eio.Exn.Backend.t += Refused_by_peer

let refused = Eio.Net.err (Connection_failure (Refused Refused_by_peer))
let unreachable = Eio_unix.Err.v Unix.ENETUNREACH "connect" ""

let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Refused -> Fmt.string ppf "refused"
      | `Unreachable -> Fmt.string ppf "unreachable")
    ( = )

(* This executable links no network library, so nothing is registered until
   the next case registers: the cases run in order. *)
let test_portable_without_a_registration () =
  Alcotest.(check (option answer))
    "a refusal" (Some `Refused)
    (Eio_net_connect.classify refused);
  Alcotest.(check (option answer))
    "an unreachable network is a backend error" None
    (Eio_net_connect.classify unreachable);
  Alcotest.(check (option answer))
    "a timeout carries no answer" None
    (Eio_net_connect.classify Eio.Time.Timeout)

let test_the_registered_classifier_answers () =
  Eio_net_connect.register ~name:"first" (function
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENETUNREACH, _, _)), _) ->
        Some `Unreachable
    | _ -> None);
  Alcotest.(check (option answer))
    "an unreachable network" (Some `Unreachable)
    (Eio_net_connect.classify unreachable);
  Alcotest.(check (option answer))
    "a refusal it does not name" None
    (Eio_net_connect.classify refused);
  Alcotest.check_raises "a second registration is refused by name"
    (Invalid_argument
       "Eio_net_connect.register: first is registered, second refused")
    (fun () -> Eio_net_connect.register ~name:"second" (fun _ -> None));
  Alcotest.(check (option answer))
    "the first classifier stays" (Some `Unreachable)
    (Eio_net_connect.classify unreachable)

let suite =
  ( "eio_net_connect",
    [
      Alcotest.test_case "portable without a registration" `Quick
        test_portable_without_a_registration;
      Alcotest.test_case "the registered classifier answers" `Quick
        test_the_registered_classifier_answers;
    ] )
