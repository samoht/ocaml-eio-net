(* Eio_net_chaos.device replays the kernel's traces on a network. Traces
   recorded through eio_posix on Linux (Docker image ocaml/opam, pinned by
   digest in scripts/generate.sh) and on macOS (traces/provenance).
   Regenerate, on macOS: REGEN=1 dune build @test/interop/kernel/regen-traces *)

open Eio_net_kernel

type os = Eio_chaos.os

let os_name : os -> string = function
  | `Linux -> "Linux"
  | `Macos -> "macOS"
  | `Windows -> "Windows"

let oses : os list = [ `Linux; `Macos; `Windows ]

let first_column file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | ops :: _ -> ops
      | [] -> Fmt.failwith "%s: bad row %S" file line)

let split = function "" -> [] | s -> String.split_on_char ';' s

(* The traces hold exactly the sequences and scenarios eio-net runs, in its
   order, which the library's copy of them relies on. *)
let test_rows () =
  List.iter
    (fun t ->
      let file = Filename.concat "traces" (transport_name t ^ ".csv") in
      if List.map split (first_column file) <> sequences t then
        Alcotest.failf
          "the %s traces hold other sequences than Eio_net_kernel's: \
           regenerate them"
          (transport_name t))
    transports;
  if first_column (Filename.concat "traces" "scenarios.csv") <> scenarios then
    Alcotest.fail
      "the scenario traces hold other scenarios than Eio_net_kernel's: \
       regenerate them"

(* eio-net.mem holds a closed connection's port only on a clock. *)
let mem os ~sw (env : Eio_net_chaos.env) =
  Eio_net_mem.v ~os ~clock:env#mono_clock ~sw ()

(* eio-net.mem answers as the kernel it follows. *)
let test_mem os () = Eio_net_chaos.device ~os (mem os)

(* A wrapper that injects nothing answers as the network it wraps. *)
let test_quiet_wrap os () =
  Eio_net_chaos.device ~os (fun ~sw env ->
      Eio_net_chaos.wrap ~os ~clock:env#clock (fun _ -> None) (mem os ~sw env))

(* A network that passes the one under it through, on a faulty one: it
   answers as the kernel does, or with the errors the faults give. *)
let test_over_faulty_network () =
  Eio_net_chaos.device ~budget:2000
    ~under:[ Eio_net_chaos.network () ]
    (fun ~sw:_ env -> env#net)

(* Each allowance of the check is one eio-net.mem needs. *)
let test_allowances () =
  let used =
    List.concat_map
      (fun os ->
        List.map
          (fun why -> (os, why))
          (Eio_net_chaos.Private.device_allowed os (mem os)))
      oses
  in
  List.iter
    (fun (os, why) ->
      if not (List.mem (os, why) used) then
        Alcotest.failf "no %s answer of eio-net.mem needs the allowance %S"
          (os_name os) why)
    Eio_net_chaos.Private.device_allowances

(* Under Windows, each eio_windows defect allowed is one an answer of
   eio-net.mem, which keeps Eio's contract, needs. *)
let test_defects () =
  let used = Eio_net_chaos.Private.device_allowed `Windows (mem `Windows) in
  List.iter
    (fun finding ->
      if not (List.mem finding used) then
        Alcotest.failf "no Windows answer of eio-net.mem needs the defect %S"
          finding)
    Eio_net_chaos.Private.device_defects

(* A network whose connects are all reset fails, and the report says
   where. *)
let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let reset (op : Eio_net_chaos.Op.t) =
  if op.kind = `Connect then Some Eio_net_chaos.Fault.Reset else None

let test_broken () =
  let net ~sw (env : Eio_net_chaos.env) =
    Eio_net_chaos.wrap ~clock:env#clock reset (Eio_net_mem.v ~sw ())
  in
  match Eio_net_chaos.device net with
  | () -> Alcotest.fail "a network that resets every connect passed"
  | exception Failure m ->
      let lines = String.split_on_char '\n' m in
      if
        not
          (String.starts_with ~prefix:"Eio_net_chaos.device: " (List.hd lines))
      then Alcotest.failf "the report starts %S" (List.hd lines);
      (* Sequences that differ at the same step with the same answers are
         one line, with an example. *)
      let grouped =
        List.exists
          (fun l ->
            String.starts_with
              ~prefix:"  connect: expected ok, got Refused, in " l
            && contains l " sequences, e.g. tcp4 "
            && String.ends_with ~suffix:"connect" l)
          lines
      in
      if not grouped then Alcotest.failf "no grouped connect line in:@.%s" m;
      (* Answers are named, not given as the traces' tokens. *)
      List.iter
        (fun l ->
          List.iter
            (fun raw ->
              if contains l raw then Alcotest.failf "a raw token %S in %S" raw l)
            [ " blk"; "{blk"; "got Fs"; "got Net"; "got r1"; " r1 " ])
        lines;
      let last = List.nth lines (List.length lines - 1) in
      if
        not
          (String.ends_with
             ~suffix:
               "left out; 13 that wait 60 seconds for a port to be free again; \
                5 that use up every port; 114 that send off the loopback \
                interface"
             last)
      then Alcotest.failf "the summary is %S" last

(* Under macOS, the TCP sequences that read or write after an end shut its
   receive side with bytes or an end of stream on their way to it, or after
   bytes went to an end that shut it or closed, are left out with the scenarios that
   depend on timing: whether the reset that follows has arrived depends on
   when the operation runs. *)
let test_macos_timing () =
  let net ~sw (env : Eio_net_chaos.env) =
    Eio_net_chaos.wrap ~os:`Macos ~clock:env#clock reset
      (Eio_net_mem.v ~os:`Macos ~sw ())
  in
  match Eio_net_chaos.device ~os:`Macos net with
  | () -> Alcotest.fail "a network that resets every connect passed"
  | exception Failure m ->
      let lines = String.split_on_char '\n' m in
      let last = List.nth lines (List.length lines - 1) in
      if
        not
          (String.ends_with
             ~suffix:"; 733 that have a macOS answer that depends on timing"
             last)
      then Alcotest.failf "the summary is %S" last

let suite =
  ( "kernel",
    [
      Alcotest.test_case "the traces hold eio-net's sequences" `Quick test_rows;
      Alcotest.test_case "a broken network fails, with a report" `Quick
        test_broken;
      Alcotest.test_case "every allowance is needed" `Quick test_allowances;
      Alcotest.test_case "every eio_windows defect is needed" `Quick
        test_defects;
      Alcotest.test_case "a network over a faulty network" `Quick
        test_over_faulty_network;
      Alcotest.test_case "macOS leaves out what depends on timing" `Quick
        test_macos_timing;
    ]
    @ List.map
        (fun os ->
          Alcotest.test_case ("eio-net.mem " ^ os_name os) `Quick (test_mem os))
        oses
    @ List.map
        (fun os ->
          Alcotest.test_case
            ("eio-net.mem wrapped with no fault " ^ os_name os)
            `Quick (test_quiet_wrap os))
        oses )
