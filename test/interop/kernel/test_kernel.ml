(* eio-net.mem replays every sequence and every scenario of the kernel's
   traces: with [~os:`Linux] it must give Linux's outcomes, and with
   [~os:`Macos] macOS's, except where an exception below says why not.

   Traces recorded through eio_posix on Linux (Docker image ocaml/opam, pinned
   by digest in scripts/generate.sh) and on macOS. Linux's outcomes are the
   same from one recording to the next; a few of macOS's TCP ones depend on
   timing, as its loopback delivers asynchronously, and are listed below by
   name.
   Regenerate, on macOS: REGEN=1 dune build @test/interop/kernel/regen-traces *)

open Kernel_seq

type os = [ `Linux | `Macos ]

let os_name : os -> string = function `Linux -> "Linux" | `Macos -> "macOS"

(* A step where mem may differ from a kernel: the transports, the operation
   (its first words), and the two outcomes. *)
type exception_ = {
  why : string;
  transports : transport list;
  op : string;
  mem : string;
  kernel : string;
}

(* None: eio-net.mem follows each kernel on every sequence these traces
   hold that does not depend on timing. *)
let exceptions : os -> exception_ list = function `Linux | `Macos -> []

(* macOS's loopback delivers a segment asynchronously, so an operation that
   follows one that sends a FIN, data or a reset to the peer can run before
   the segment arrives; mem delivers it at once. These sequences are not
   compared: on one Mac their recorded outcome differs from that of a
   recording that lets 20 ms pass after each operation, which mem gives. *)
let timing_dependent : (transport * string) list =
  [
    (Tcp4, "connect;accept;close C;shutdown S all;write S 0");
    (Tcp4, "connect;accept;close C;shutdown S all;write S 2");
    (Tcp4, "connect;accept;close S;shutdown C all;write C 0");
    (Tcp4, "connect;accept;close S;shutdown C all;write C 2");
    (Tcp4, "connect;accept;shutdown C all;shutdown S all;write S 0");
    (Tcp4, "connect;accept;shutdown C all;shutdown S all;write S 2");
    (Tcp4, "connect;accept;shutdown C send;shutdown S all;read C 1");
    (Tcp4, "connect;accept;shutdown C send;shutdown S all;read C 4");
    (Tcp4, "connect;accept;shutdown C send;shutdown S all;write S 2");
    (Tcp4, "connect;accept;shutdown S all;shutdown C all;write C 0");
    (Tcp4, "connect;accept;shutdown S send;shutdown C all;read S 1");
    (Tcp4, "connect;accept;shutdown S send;shutdown C all;read S 4");
    (Tcp4, "connect;accept;shutdown S send;shutdown C all;write C 0");
    (Tcp4, "connect;accept;shutdown S send;shutdown C all;write C 2");
    (Tcp4, "connect;accept;write C 2;shutdown S all;read C 1");
    (Tcp4, "connect;accept;write C 2;shutdown S all;read C 4");
    (Tcp4, "connect;accept;write C 2;shutdown S all;write C 0");
    (Tcp4, "connect;accept;write C 2;shutdown S all;write C 2");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;read C 1");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;read C 4");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;write C 0");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;write C 2");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;write S 0");
    (Tcp4, "connect;accept;write C 2;shutdown S recv;write S 2");
    (Tcp4, "connect;accept;write S 2;shutdown C all;read S 1");
    (Tcp4, "connect;accept;write S 2;shutdown C all;read S 4");
    (Tcp4, "connect;accept;write S 2;shutdown C all;write S 0");
    (Tcp4, "connect;accept;write S 2;shutdown C all;write S 2");
    (Tcp4, "connect;accept;write S 2;shutdown C recv;read S 1");
    (Tcp4, "connect;accept;write S 2;shutdown C recv;read S 4");
    (Tcp4, "connect;accept;write S 2;shutdown C recv;write C 0");
    (Tcp4, "connect;accept;write S 2;shutdown C recv;write C 2");
    (Tcp6, "connect;accept;close C;shutdown S all;write S 0");
    (Tcp6, "connect;accept;close C;shutdown S all;write S 2");
    (Tcp6, "connect;accept;close C;write S 2;write S 0");
    (Tcp6, "connect;accept;close S;shutdown C all;write C 0");
    (Tcp6, "connect;accept;close S;shutdown C all;write C 2");
    (Tcp6, "connect;accept;shutdown C all;shutdown S all;write S 0");
    (Tcp6, "connect;accept;shutdown C all;shutdown S all;write S 2");
    (Tcp6, "connect;accept;shutdown C send;shutdown S all;read C 1");
    (Tcp6, "connect;accept;shutdown C send;shutdown S all;read C 4");
    (Tcp6, "connect;accept;shutdown C send;shutdown S all;write S 0");
    (Tcp6, "connect;accept;shutdown C send;shutdown S all;write S 2");
    (Tcp6, "connect;accept;shutdown S all;shutdown C all;write C 0");
    (Tcp6, "connect;accept;shutdown S all;shutdown C all;write C 2");
    (Tcp6, "connect;accept;shutdown S send;shutdown C all;read S 1");
    (Tcp6, "connect;accept;shutdown S send;shutdown C all;read S 4");
    (Tcp6, "connect;accept;shutdown S send;shutdown C all;write C 0");
    (Tcp6, "connect;accept;shutdown S send;shutdown C all;write C 2");
    (Tcp6, "connect;accept;write C 2;shutdown S all;read C 1");
    (Tcp6, "connect;accept;write C 2;shutdown S all;read C 4");
    (Tcp6, "connect;accept;write C 2;shutdown S all;write C 0");
    (Tcp6, "connect;accept;write C 2;shutdown S all;write C 2");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;read C 1");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;read C 4");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;write C 0");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;write C 2");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;write S 0");
    (Tcp6, "connect;accept;write C 2;shutdown S recv;write S 2");
    (Tcp6, "connect;accept;write S 2;shutdown C all;read S 1");
    (Tcp6, "connect;accept;write S 2;shutdown C all;read S 4");
    (Tcp6, "connect;accept;write S 2;shutdown C all;write S 0");
    (Tcp6, "connect;accept;write S 2;shutdown C all;write S 2");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;read S 1");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;read S 4");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;write C 0");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;write C 2");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;write S 0");
    (Tcp6, "connect;accept;write S 2;shutdown C recv;write S 2");
  ]

type row = { ops : string list; linux : string list; macos : string list }

let column os r = match os with `Linux -> r.linux | `Macos -> r.macos
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

let replay ~os t ops =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw -> Kernel_seq.run (Eio_net_mem.v ~os ~sw ()) t ops

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

let matches t ~op ~mem ~kernel (e : exception_) =
  List.mem t e.transports
  && starts_with ~prefix:e.op op
  && e.mem = mem && e.kernel = kernel

(* The exceptions some sequence or scenario needed, by OS. *)
let used = Hashtbl.create 16

let test_transport os t () =
  let rows = rows t in
  if List.map (fun r -> r.ops) rows <> sequences t then
    Alcotest.failf
      "the %s traces hold other sequences than Kernel_seq's: regenerate them"
      (transport_name t);
  let timed r =
    os = `Macos && List.mem (t, String.concat ";" r.ops) timing_dependent
  in
  let unexpected =
    List.filter_map
      (fun r ->
        let mem = replay ~os t r.ops in
        let kernel = column os r in
        match first_diff 0 mem kernel with
        | None -> None
        | Some _ when timed r -> None
        | Some i -> (
            let op = nth r.ops i and m = nth mem i and k = nth kernel i in
            match
              List.find_opt (matches t ~op ~mem:m ~kernel:k) (exceptions os)
            with
            | Some e ->
                Hashtbl.replace used (os, e.why) ();
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
      Alcotest.failf "%d of %d %s sequences differ from %s:@.%s" (List.length l)
        (List.length rows) (transport_name t) (os_name os)
        (String.concat "\n" l)

(* A scenario where mem may differ from a kernel, and when: given the
   scenario's name, mem's outcome and the kernel's. *)
type scenario_exception = {
  why : string;
  oses : os list;
  holds : string -> mem:string -> kernel:string -> bool;
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

(* The text of [s] with each number cut out, and the numbers. *)
let numbers s =
  let text = Buffer.create 16 and ns = ref [] and n = Buffer.create 8 in
  let flush () =
    if Buffer.length n > 0 then begin
      ns := int_of_string (Buffer.contents n) :: !ns;
      Buffer.add_char text '#';
      Buffer.clear n
    end
  in
  String.iter
    (function
      | '0' .. '9' as c -> Buffer.add_char n c
      | c ->
          flush ();
          Buffer.add_char text c)
    s;
  flush ();
  (Buffer.contents text, List.rev !ns)

(* [mem] and [kernel] say the same but for their numbers, each of mem's
   within [factor] of the kernel's. *)
let within factor ~mem ~kernel =
  let tm, nm = numbers mem and tk, nk = numbers kernel in
  tm = tk
  && List.compare_lengths nm nk = 0
  && List.for_all2
       (fun m k ->
         let m = float m and k = float k in
         m <= factor *. k && k <= factor *. m)
       nm nk

let scenario_exceptions : scenario_exception list =
  [
    {
      why =
        "a stream queues what mem works out from the buffers set, within 15% \
         of what Linux holds in writes of 1 KiB, as the overhead of each \
         segment and write is Linux's own";
      oses = [ `Linux ];
      holds =
        (fun name ~mem ~kernel ->
          starts_with ~prefix:"sockopt fill: " name && within 1.15 ~mem ~kernel);
    };
    {
      why =
        "macOS writes nothing into less room than a write's length and the \
         low-water mark, so a Unix stream with a small send buffer queues up \
         to half of what mem does";
      oses = [ `Macos ];
      holds =
        (fun name ~mem ~kernel ->
          starts_with ~prefix:"sockopt fill: unix" name
          && within 2. ~mem ~kernel);
    };
    {
      why =
        "macOS sizes the buffers of a TCP socket set before it connects, or \
         taken from its listener, to whole segments as it connects; mem keeps \
         the size set";
      oses = [ `Macos ];
      holds =
        (fun name ~mem ~kernel ->
          List.mem name
            [
              "sockopt inherit: tcp4 server SO_SNDBUF";
              "sockopt inherit: tcp4 server SO_RCVBUF";
              "sockopt connect: tcp4 client SO_SNDBUF";
              "sockopt connect: tcp4 client SO_RCVBUF";
            ]
          &&
          let tm, nm = numbers mem and tk, nk = numbers kernel in
          tm = tk && List.for_all2 ( <= ) nm nk);
    };
    {
      why =
        "macOS counts a datagram's memory in mbufs, which mem approximates by \
         its length";
      oses = [ `Macos ];
      holds =
        (fun name ~mem ~kernel ->
          (starts_with ~prefix:"sockopt udp rcvbuf" name
          || starts_with ~prefix:"sockopt unix dgram rcvbuf" name)
          && within 2. ~mem ~kernel);
    };
    {
      why =
        "the recording host has no IPv6 address but loopback, so glibc leaves \
         out IPv6 addresses; mem resolves as a dual-stack host";
      oses = [ `Linux ];
      holds =
        (fun name ~mem ~kernel:linux ->
          starts_with ~prefix:"getaddrinfo" name
          && without_ipv6 name mem = linux);
    };
    {
      why = "mem has no services database: a service name is unknown to it";
      oses = [ `Linux; `Macos ];
      holds =
        (fun name ~mem ~kernel ->
          starts_with ~prefix:"getaddrinfo" name
          && Filename.check_suffix name {|"http"|}
          && (mem = "SERVICE" || mem = "NONAME")
          && (starts_with ~prefix:"tcp:" kernel
             || starts_with ~prefix:"udp:" kernel
             || kernel = "ADDRFAMILY"));
    };
    {
      why = "mem has no services database: getnameinfo gives the port";
      oses = [ `Linux; `Macos ];
      holds =
        (fun name ~mem ~kernel:linux ->
          let mh, ms = split_last '/' mem and lh, ls = split_last '/' linux in
          starts_with ~prefix:"getnameinfo" name
          && mh = lh && is_number ms
          && not (is_number ls));
    };
    {
      why =
        "mem has no host name: getnameinfo gives a Unix address's host as \
         localhost";
      oses = [ `Linux ];
      holds =
        (fun name ~mem ~kernel:linux ->
          let mh, ms = split_last '/' mem and lh, ls = split_last '/' linux in
          starts_with ~prefix:"getnameinfo unix" name
          && mh = "localhost" && lh = "HOSTNAME" && ms = ls);
    };
    {
      why =
        "mem has no name server: a name /etc/hosts does not hold, such as \
         localhost. with its final dot, maps to no address, where the \
         recording host's name server resolves it";
      oses = [ `Linux; `Macos ];
      holds =
        (fun name ~mem ~kernel:linux ->
          match String.split_on_char '"' name with
          | [ "getaddrinfo "; host; " "; _; "" ] ->
              String.ends_with ~suffix:"." host
              && mem = "NONAME"
              && (starts_with ~prefix:"tcp:" linux
                 || starts_with ~prefix:"udp:" linux)
          | _ -> false);
    };
    {
      why =
        "a scenario off the loopback interface, or one that runs out of \
         ephemeral ports, is recorded only in a container, so not on macOS";
      oses = [ `Macos ];
      holds = (fun _ ~mem:_ ~kernel -> kernel = "not recorded");
    };
    {
      why =
        "the Mac that records routes multicast through its own network \
         interface, which 127.0.0.1 is not on; mem's host has loopback only";
      oses = [ `Macos ];
      holds =
        (fun name ~mem ~kernel ->
          name = "dgram error v4->224.0.0.1 len=10"
          && mem = "sent" && kernel = "EADDRNOTAVAIL");
    };
  ]

let scenario_rows () =
  let file = Filename.concat "traces" "scenarios.csv" in
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ name; linux; macos ] -> (name, linux, macos)
      | _ -> Fmt.failwith "%s: bad row %S" file line)

(* The scenarios whose macOS outcome depends on timing, as its loopback
   delivers asynchronously, and why: they are not compared. *)
let timing_dependent_scenarios : (string * string) list =
  let last_ack =
    "the end that closes second holds its port until the peer's last ACK \
     reaches it, which a bind right after the close races: on one Mac a bind \
     succeeded within 6 ms of both closes"
  in
  [
    ("reuse client closes first", last_ack);
    ("reuse bound server closes first", last_ack);
    ("timed: client closes first", last_ack);
    ("timed: client shuts send then server closes", last_ack);
    ("timed: client shuts down then server closes", last_ack);
    ("dual-stack hold: listen :: client ::1 client closes first", last_ack);
    ("dual-stack hold: listen :: client 127.0.0.1 client closes first", last_ack);
    ("dual-stack hold: listen ::1 client ::1 client closes first", last_ack);
    ( "dual-stack hold: listen 0.0.0.0 client 127.0.0.1 client closes first",
      last_ack );
    ( "timed: reset by unread bytes",
      "the reset races the bind: two recordings on one Mac differ" );
    ("connect 127.0.0.1 from 0.0.0.0 to ::1", "two recordings on one Mac differ");
  ]
  @ List.filter_map
      (fun name ->
        if starts_with ~prefix:"sockopt fill: tcp4" name then
          Some
            ( name,
              "macOS grows a TCP receive buffer as data comes, so what a \
               writer queues differs from one recording to the next" )
        else None)
      scenarios

(* mem runs on a mock clock, which moves only when every fiber waits, as the
   kernel's timers run on a scenario's real time. An untimed scenario has 2
   seconds, as the recorder gives it. *)
let replay_scenario ~os name =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let net = Eio_net_mem.v ~os ~clock ~sw () in
  if Kernel_seq.timed name || Kernel_seq.narrow_ports name then
    Kernel_seq.scenario ~wait:(Eio.Time.Mono.sleep clock) net name
  else
    match
      Eio.Time.with_timeout_exn env#clock 2. (fun () ->
          Kernel_seq.scenario net name)
    with
    | r -> r
    | exception Eio.Time.Timeout -> "timeout"

let test_scenarios os () =
  let rows = scenario_rows () in
  if List.map (fun (n, _, _) -> n) rows <> scenarios then
    Alcotest.fail
      "the scenario traces hold other scenarios than Kernel_seq's: regenerate \
       them";
  let unexpected =
    List.filter_map
      (fun (name, linux, macos) ->
        let kernel = match os with `Linux -> linux | `Macos -> macos in
        let mem = replay_scenario ~os name in
        if String.equal mem kernel then None
        else if os = `Macos && List.mem_assoc name timing_dependent_scenarios
        then None
        else
          match
            List.find_opt
              (fun (e : scenario_exception) ->
                List.mem os e.oses && e.holds name ~mem ~kernel)
              scenario_exceptions
          with
          | Some e ->
              Hashtbl.replace used (os, e.why) ();
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
      Alcotest.failf "%d of %d scenarios differ from %s:@.%s" (List.length l)
        (List.length rows) (os_name os) (String.concat "\n" l)

let oses : os list = [ `Linux; `Macos ]

let test_exceptions_needed () =
  List.iter
    (fun os ->
      List.iter
        (fun (e : exception_) ->
          if not (Hashtbl.mem used (os, e.why)) then
            Alcotest.failf "no %s sequence needs the exception %S any more"
              (os_name os) e.why)
        (exceptions os))
    oses;
  List.iter
    (fun (e : scenario_exception) ->
      List.iter
        (fun os ->
          if not (Hashtbl.mem used (os, e.why)) then
            Alcotest.failf "no %s scenario needs the exception %S any more"
              (os_name os) e.why)
        e.oses)
    scenario_exceptions;
  List.iter
    (fun (t, ops) ->
      if not (List.mem (String.split_on_char ';' ops) (sequences t)) then
        Alcotest.failf "%s holds no sequence %S" (transport_name t) ops)
    timing_dependent;
  List.iter
    (fun (name, _) ->
      if not (List.mem name scenarios) then
        Alcotest.failf "there is no scenario %S" name)
    timing_dependent_scenarios

let suite =
  ( "kernel",
    List.concat_map
      (fun os ->
        List.map
          (fun t ->
            Alcotest.test_case
              (Fmt.str "%s %s" (transport_name t) (os_name os))
              `Quick (test_transport os t))
          transports
        @ [
            Alcotest.test_case
              ("scenarios " ^ os_name os)
              `Quick (test_scenarios os);
          ])
      oses
    @ [
        Alcotest.test_case "every exception is needed" `Quick
          test_exceptions_needed;
      ] )
