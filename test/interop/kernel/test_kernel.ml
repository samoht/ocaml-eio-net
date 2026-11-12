(* eio-net.mem replays every sequence and every scenario of the kernel's
   traces and must give Linux's outcomes, except where an exception below
   says why not.

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

let matches t ~op ~mem ~linux (e : exception_) =
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

(* A scenario where mem may differ from Linux, and when: given the
   scenario's name, mem's outcome and Linux's. *)
type scenario_exception = {
  why : string;
  holds : string -> mem:string -> linux:string -> bool;
}

let split_last c s =
  match String.rindex_opt s c with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

(* The addresses glibc gives on a host with no IPv6 address but loopback,
   for those mem gives on a dual-stack one: IPv6 ones left out, IPv4-mapped
   ones turned into IPv4, and the ::1 that /etc/hosts gives localhost turned
   into 127.0.0.1; "localhost." is not in /etc/hosts. *)
let without_ipv6 name mem =
  let localhost =
    List.exists
      (fun n -> starts_with ~prefix:(Fmt.str "getaddrinfo %S " n) name)
      [ "localhost"; "LOCALHOST" ]
  in
  if not (starts_with ~prefix:"tcp:" mem || starts_with ~prefix:"udp:" mem) then
    mem
  else
    let v4 addr =
      let kind_ip, port = split_last ':' addr in
      let kind, ip =
        ( String.sub kind_ip 0 3,
          String.sub kind_ip 4 (String.length kind_ip - 4) )
      in
      let ip =
        if ip = "::1" then if localhost then Some "127.0.0.1" else None
        else if starts_with ~prefix:"::ffff:" ip then
          Some (String.sub ip 7 (String.length ip - 7))
        else if String.contains ip ':' then None
        else Some ip
      in
      Option.map (fun ip -> Fmt.str "%s:%s:%s" kind ip port) ip
    in
    match List.filter_map v4 (String.split_on_char ' ' mem) with
    | [] -> "ADDRFAMILY"
    | l -> String.concat " " l

let is_number s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let scenario_exceptions : scenario_exception list =
  [
    {
      why =
        "the recording host has no IPv6 address but loopback, so glibc leaves \
         out IPv6 addresses; mem resolves as a dual-stack host";
      holds =
        (fun name ~mem ~linux ->
          starts_with ~prefix:"getaddrinfo" name
          && without_ipv6 name mem = linux);
    };
    {
      why = "mem has no services database: a service name is unknown to it";
      holds =
        (fun name ~mem ~linux ->
          starts_with ~prefix:"getaddrinfo" name
          && Filename.check_suffix name {|"http"|}
          && mem = "SERVICE"
          && (starts_with ~prefix:"tcp:" linux || linux = "ADDRFAMILY"));
    };
    {
      why = "mem has no services database: getnameinfo gives the port";
      holds =
        (fun name ~mem ~linux ->
          let mh, ms = split_last '/' mem and lh, ls = split_last '/' linux in
          starts_with ~prefix:"getnameinfo" name
          && mh = lh && is_number ms
          && not (is_number ls));
    };
    {
      why =
        "mem has no host name: getnameinfo gives a Unix address's host as \
         localhost";
      holds =
        (fun name ~mem ~linux ->
          let mh, ms = split_last '/' mem and lh, ls = split_last '/' linux in
          starts_with ~prefix:"getnameinfo unix" name
          && mh = "localhost" && lh = "HOSTNAME" && ms = ls);
    };
    {
      why =
        "mem has no name server: a name /etc/hosts does not hold, such as \
         localhost. with its final dot, maps to no address, where the \
         recording host's name server resolves it";
      holds =
        (fun name ~mem ~linux ->
          match String.split_on_char '"' name with
          | [ "getaddrinfo "; host; " "; _; "" ] ->
              String.ends_with ~suffix:"." host
              && mem = "NONAME"
              && (starts_with ~prefix:"tcp:" linux
                 || starts_with ~prefix:"udp:" linux)
          | _ -> false);
    };
  ]
  @ List.map
      (fun (why, prefixes) ->
        {
          why;
          holds =
            (fun name ~mem:_ ~linux:_ ->
              List.exists (fun prefix -> starts_with ~prefix name) prefixes);
        })
      [
        ( "pending: a failed connect frees its bound address at once",
          [ "failed connect: " ] );
        ( "pending: getnameinfo names IPv4-compatible and abstract addresses \
           otherwise",
          [
            "getnameinfo tcp ::7f00:1";
            "getnameinfo tcp ::1:0";
            "getnameinfo unix abstract";
          ] );
      ]

let scenario_rows () =
  let file = Filename.concat "traces" "scenarios.csv" in
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ name; linux; macos ] -> (name, linux, macos)
      | _ -> Fmt.failwith "%s: bad row %S" file line)

(* mem runs on a mock clock, which moves only when a timed scenario waits,
   as Linux's timers run on a scenario's real time. *)
let replay_scenario name =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let wait secs =
    let span = Mtime.Span.of_float_ns (secs *. 1e9) |> Option.get in
    Eio_mock.Clock.Mono.set_time clock
      (Mtime.add_span (Eio.Time.Mono.now clock) span |> Option.get)
  in
  Kernel_seq.scenario ~wait (Eio_net_mem.v ~clock ~sw ()) name

let test_scenarios () =
  let rows = scenario_rows () in
  if List.map (fun (n, _, _) -> n) rows <> scenarios then
    Alcotest.fail
      "the scenario traces hold other scenarios than Kernel_seq's: regenerate \
       them";
  let unexpected =
    List.filter_map
      (fun (name, linux, macos) ->
        let mem = replay_scenario name in
        if String.equal mem linux then None
        else
          match
            List.find_opt
              (fun (e : scenario_exception) -> e.holds name ~mem ~linux)
              scenario_exceptions
          with
          | Some e ->
              Hashtbl.replace used e.why ();
              None
          | None ->
              Fmt.kstr Option.some
                "%s@.    mem   %s@.    Linux %s@.    macOS %s" name mem linux
                macos)
      rows
  in
  match unexpected with
  | [] -> ()
  | l ->
      List.iteri (fun i s -> if i < 400 then Fmt.epr "%s@." s) l;
      Alcotest.failf "%d of %d scenarios differ from Linux" (List.length l)
        (List.length rows)

let test_exceptions_needed () =
  List.iter
    (fun (e : exception_) ->
      if not (Hashtbl.mem used e.why) then
        Alcotest.failf "no sequence needs the exception %S any more" e.why)
    exceptions;
  List.iter
    (fun (e : scenario_exception) ->
      if not (Hashtbl.mem used e.why) then
        Alcotest.failf "no scenario needs the exception %S any more" e.why)
    scenario_exceptions

let suite =
  ( "kernel",
    List.map
      (fun t -> Alcotest.test_case (transport_name t) `Quick (test_transport t))
      transports
    @ [
        Alcotest.test_case "scenarios" `Quick test_scenarios;
        Alcotest.test_case "every exception is needed" `Quick
          test_exceptions_needed;
      ] )
