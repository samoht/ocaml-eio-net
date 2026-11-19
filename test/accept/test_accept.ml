let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Aborted -> Fmt.string ppf "aborted"
      | `Descriptors -> Fmt.string ppf "descriptors")
    ( = )

let check name want e =
  Alcotest.(check (option answer))
    name want
    (Eio_net.Accept.classify (Eio_unix.Err.v e "accept" ""))

(* A peer that reset its connection before the accept, and a process or
   system out of descriptors, are failures that leave the listener whole. *)
let test_classify () =
  check "an aborted connection" (Some `Aborted) Unix.ECONNABORTED;
  check "no descriptor left in the process" (Some `Descriptors) Unix.EMFILE;
  check "no descriptor left in the system" (Some `Descriptors) Unix.ENFILE;
  check "a closed descriptor" None Unix.EBADF;
  Alcotest.(check (option answer))
    "an exception it does not name" None
    (Eio_net.Accept.classify Exit)

(* The system the cases run on, as uname -s names it. *)
let system =
  lazy
    (if Sys.os_type <> "Unix" then Sys.os_type
     else
       let ic = Unix.open_process_args_in "uname" [| "uname"; "-s" |] in
       let s = String.trim (In_channel.input_all ic) in
       ignore (Unix.close_process_in ic);
       s)

(* Codes OCaml's Unix names only as EUNKNOWNERR: EPROTO and ENONET, on the
   systems that define them. *)
let unknown_codes () =
  match Lazy.force system with
  | "Linux" -> [ ("EPROTO", 71); ("ENONET", 64) ]
  | "Darwin" -> [ ("EPROTO", 100) ]
  | _ -> []

(* accept(2) on Linux passes on the network error pending on the new
   connection, which consumes it, and a BSD's clears the listener's error it
   returns: each is an aborted accept, followed by the next. An error that
   persists on the listener, EOPNOTSUPP for a socket that is not a stream or
   EPERM from a security module, is no answer, so the loop raises it rather
   than spin. *)
let test_pending_errors () =
  List.iter
    (fun e -> check (Unix.error_message e) (Some `Aborted) e)
    Unix.[ ENETDOWN; ENOPROTOOPT; EHOSTDOWN; EHOSTUNREACH; ENETUNREACH ];
  List.iter
    (fun (name, n) -> check name (Some `Aborted) (Unix.EUNKNOWNERR n))
    (unknown_codes ());
  List.iter
    (fun e -> check (Unix.error_message e) None e)
    Unix.[ EOPNOTSUPP; EPERM; EINVAL ]

let suite =
  ( "accept",
    [
      Alcotest.test_case "classify" `Quick test_classify;
      Alcotest.test_case "errors pending on the connection" `Quick
        test_pending_errors;
    ] )
