(* eio-net.mem replays every sequence of the kernel's traces and must give
   Linux's outcomes, except where an exception below says why not.

   Traces recorded through eio_posix on Linux (Docker image ocaml/opam, pinned
   by digest in scripts/generate.sh) and on macOS. Linux's outcomes are the
   same from one recording to the next; a few of macOS's TCP ones depend on
   timing, as its loopback delivers asynchronously, so they are shown only to
   compare with.
   Regenerate, on macOS: REGEN=1 dune build @test/interop/kernel/regen-traces *)

open Kernel_seq

(* A step where mem may differ from Linux: the transports, the operation (its
   first words), and the two outcomes. *)
type exception_ = {
  why : string;
  transports : transport list;
  op : string;
  mem : string;
  linux : string;
}

(* Choices the mli documents as following macOS, each listed here: none,
   as eio-net.mem follows Linux on every sequence these traces hold. *)
let exceptions : exception_ list = []

type row = { ops : string list; linux : string list; macos : string list }

let split sep = function "" -> [] | s -> String.split_on_char sep s

let rows t =
  let file = Filename.concat "traces" (transport_name t ^ ".csv") in
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; linux; macos ] ->
          {
            ops = split ';' ops;
            linux = split '|' linux;
            macos = split '|' macos;
          }
      | _ -> Fmt.failwith "%s: bad row %S" file line)

let replay t ops =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw -> Kernel_seq.run (Eio_net_mem.v ~sw ()) t ops

let rec first_diff i a b =
  match (a, b) with
  | x :: a, y :: b ->
      if String.equal x y then first_diff (i + 1) a b else Some i
  | [], [] -> None
  | _ -> Some i

let nth l i = Option.value (List.nth_opt l i) ~default:"none"

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix

let matches t ~op ~mem ~linux e =
  List.mem t e.transports
  && starts_with ~prefix:e.op op
  && e.mem = mem && e.linux = linux

(* The exceptions some sequence needed, across transports. *)
let used = Hashtbl.create 16

let test_transport t () =
  let rows = rows t in
  if List.map (fun r -> r.ops) rows <> sequences t then
    Alcotest.failf
      "the %s traces hold other sequences than Kernel_seq's: regenerate them"
      (transport_name t);
  let unexpected =
    List.filter_map
      (fun r ->
        let mem = replay t r.ops in
        match first_diff 0 mem r.linux with
        | None -> None
        | Some i -> (
            let op = nth r.ops i and m = nth mem i and l = nth r.linux i in
            match List.find_opt (matches t ~op ~mem:m ~linux:l) exceptions with
            | Some e ->
                Hashtbl.replace used e.why ();
                None
            | None ->
                Fmt.kstr Option.some
                  "%s@.    mem   %s@.    Linux %s@.    macOS %s"
                  (String.concat "; " r.ops) (String.concat " | " mem)
                  (String.concat " | " r.linux)
                  (String.concat " | " r.macos)))
      rows
  in
  match unexpected with
  | [] -> ()
  | l ->
      List.iteri (fun i s -> if i < 20 then Fmt.epr "%s@." s) l;
      Alcotest.failf "%d of %d %s sequences differ from Linux" (List.length l)
        (List.length rows) (transport_name t)

let test_exceptions_needed () =
  List.iter
    (fun e ->
      if not (Hashtbl.mem used e.why) then
        Alcotest.failf "no sequence needs the exception %S any more" e.why)
    exceptions

let suite =
  ( "kernel",
    List.map
      (fun t -> Alcotest.test_case (transport_name t) `Quick (test_transport t))
      transports
    @ [
        Alcotest.test_case "every exception is needed" `Quick
          test_exceptions_needed;
      ] )
