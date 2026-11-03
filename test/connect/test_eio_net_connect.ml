type Eio.Exn.Backend.t += Refused_by_peer

let refused = Eio.Net.err (Connection_failure (Refused Refused_by_peer))
let unreachable = Eio_unix.Err.v Unix.ENETUNREACH "connect" ""

let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Refused -> Fmt.string ppf "refused"
      | `Unreachable -> Fmt.string ppf "unreachable")
    ( = )

(* The default classifier reads the errors Eio names portably, and no
   backend's. *)
let test_portable () =
  Alcotest.(check (option answer))
    "a refusal" (Some `Refused)
    (Eio_net_connect.classify refused);
  Alcotest.(check (option answer))
    "an unreachable network is a backend error" None
    (Eio_net_connect.classify unreachable);
  Alcotest.(check (option answer))
    "a timeout carries no answer" None
    (Eio_net_connect.classify Eio.Time.Timeout)

let suite =
  ( "eio_net_connect",
    [ Alcotest.test_case "the portable answers" `Quick test_portable ] )
