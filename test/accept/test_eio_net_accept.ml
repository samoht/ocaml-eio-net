let no_descriptor = Eio_unix.Err.v Unix.EMFILE "accept" ""
let aborted = Eio_unix.Err.v Unix.ECONNABORTED "accept" ""

let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Aborted -> Fmt.string ppf "aborted"
      | `Descriptors -> Fmt.string ppf "descriptors")
    ( = )

(* The default classifier reads the errors Eio names portably, and no
   backend's. *)
let test_portable () =
  Alcotest.(check (option answer))
    "an aborted connection" (Some `Aborted)
    (Eio_net_accept.classify aborted);
  Alcotest.(check (option answer))
    "no descriptor is a backend error" None
    (Eio_net_accept.classify no_descriptor);
  Alcotest.(check (option answer))
    "an exception it does not name" None
    (Eio_net_accept.classify Exit)

let suite =
  ( "eio_net_accept",
    [ Alcotest.test_case "the portable answers" `Quick test_portable ] )
