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
  Alcotest.check_raises "a second registration is refused by name"
    (Invalid_argument
       "Eio_net_accept.register: first is registered, second refused")
    (fun () -> Eio_net_accept.register ~name:"second" (fun _ -> None))

let suite =
  ( "eio_net_accept",
    [
      Alcotest.test_case "portable without a registration" `Quick
        test_portable_without_a_registration;
      Alcotest.test_case "the registered classifier answers" `Quick
        test_the_registered_classifier_answers;
    ] )
