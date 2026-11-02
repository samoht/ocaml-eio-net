let no_descriptor = Eio_unix.Err.v Unix.EMFILE "accept" ""
let aborted = Eio_unix.Err.v Unix.ECONNABORTED "accept" ""

let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Aborted -> Fmt.string ppf "aborted"
      | `Descriptors -> Fmt.string ppf "descriptors")
    ( = )

(* This executable links no network library, so nothing is registered until
   the next case registers: the cases run in order. *)
let test_portable_without_a_registration () =
  Alcotest.(check (option answer))
    "an aborted connection" (Some `Aborted)
    (Eio_net_accept.classify aborted);
  Alcotest.(check (option answer))
    "no descriptor is a backend error" None
    (Eio_net_accept.classify no_descriptor)

let test_the_registered_classifier_answers () =
  Eio_net_accept.register ~name:"first" (function
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, _, _)), _) ->
        Some `Descriptors
    | _ -> None);
  Alcotest.(check (option answer))
    "no descriptor" (Some `Descriptors)
    (Eio_net_accept.classify no_descriptor);
  Alcotest.(check (option answer))
    "an exception it does not name" None
    (Eio_net_accept.classify Exit);
  ()

(* A second classifier, as a program that links two network libraries
   registers, is asked after the first: the first answer wins. *)
let test_classifiers_are_asked_in_registration_order () =
  Eio_net_accept.register ~name:"second" (function
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, _, _)), _) | Exit ->
        Some `Aborted
    | _ -> None);
  Alcotest.(check (option answer))
    "the first classifier's answer wins" (Some `Descriptors)
    (Eio_net_accept.classify no_descriptor);
  Alcotest.(check (option answer))
    "the second answers what the first does not" (Some `Aborted)
    (Eio_net_accept.classify Exit)

let suite =
  ( "eio_net_accept",
    [
      Alcotest.test_case "portable without a registration" `Quick
        test_portable_without_a_registration;
      Alcotest.test_case "the registered classifier answers" `Quick
        test_the_registered_classifier_answers;
      Alcotest.test_case "classifiers are asked in registration order" `Quick
        test_classifiers_are_asked_in_registration_order;
    ] )
