let answer =
  Alcotest.testable
    (fun ppf -> function
      | `Descriptors -> Fmt.string ppf "descriptors"
      | `Memory -> Fmt.string ppf "memory"
      | `Unsupported -> Fmt.string ppf "unsupported"
      | `Denied -> Fmt.string ppf "denied")
    ( = )

(* eio_posix lets socket(2)'s error through as it is, a [Unix.Unix_error], and
   an error Eio does not name elsewhere is an [Eio_unix.Unix_error]: both carry
   the answer. *)
let check name want e =
  Alcotest.(check (option answer))
    (name ^ ", raw") want
    (Eio_net.Socket.classify (Unix.Unix_error (e, "socket", "")));
  Alcotest.(check (option answer))
    (name ^ ", wrapped") want
    (Eio_net.Socket.classify (Eio_unix.Err.v e "socket" ""))

(* The errors of socket() that POSIX names, with Linux's EINVAL for an unknown
   protocol or family, are answers from the system; a cancellation and a
   descriptor error are none. *)
let test_classify () =
  check "no descriptor left in the process" (Some `Descriptors) Unix.EMFILE;
  check "no descriptor left in the system" (Some `Descriptors) Unix.ENFILE;
  check "no buffer space" (Some `Memory) Unix.ENOBUFS;
  check "no memory" (Some `Memory) Unix.ENOMEM;
  check "an unsupported family" (Some `Unsupported) Unix.EAFNOSUPPORT;
  check "an unsupported protocol" (Some `Unsupported) Unix.EPROTONOSUPPORT;
  check "a protocol of another type" (Some `Unsupported) Unix.EPROTOTYPE;
  check "an unknown protocol or family" (Some `Unsupported) Unix.EINVAL;
  check "a denied creation" (Some `Denied) Unix.EACCES;
  check "a creation not permitted" (Some `Denied) Unix.EPERM;
  check "a closed descriptor" None Unix.EBADF;
  Alcotest.(check (option answer))
    "a cancellation carries no answer" None
    (Eio_net.Socket.classify (Eio.Cancel.Cancelled Exit));
  Alcotest.(check (option answer))
    "an exception it does not name" None
    (Eio_net.Socket.classify Exit)

let suite = ("socket", [ Alcotest.test_case "classify" `Quick test_classify ])
