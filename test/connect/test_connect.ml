type Eio.Exn.Backend.t += Refused_by_peer

let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Refused -> Fmt.string ppf "refused"
      | `Unreachable -> Fmt.string ppf "unreachable")
    ( = )

let check name want e =
  Alcotest.(check (option answer))
    name want
    (Eio_net.Connect.classify (Eio_unix.Err.v e "connect" ""))

(* A refused peer and an unreachable network or host are answers from the
   network; a timeout is none. *)
let test_classify () =
  Alcotest.(check (option answer))
    "a refusal" (Some `Refused)
    (Eio_net.Connect.classify
       (Eio.Net.err (Connection_failure (Refused Refused_by_peer))));
  check "ECONNREFUSED" (Some `Refused) Unix.ECONNREFUSED;
  check "an unreachable network" (Some `Unreachable) Unix.ENETUNREACH;
  check "an unreachable host" (Some `Unreachable) Unix.EHOSTUNREACH;
  check "ETIMEDOUT" None Unix.ETIMEDOUT;
  Alcotest.(check (option answer))
    "a timeout carries no answer" None
    (Eio_net.Connect.classify Eio.Time.Timeout)

let suite = ("connect", [ Alcotest.test_case "classify" `Quick test_classify ])
