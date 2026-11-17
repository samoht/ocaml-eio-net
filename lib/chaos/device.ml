open! Hermetic
module K = Eio_net_kernel

type os = [ `Linux | `Macos ]

let name = "Eio_net_chaos.device"
let os_name : os -> string = function `Linux -> "Linux" | `Macos -> "macOS"
let split sep = function "" -> [] | s -> String.split_on_char sep s
let starts_with ~prefix s = String.starts_with ~prefix s

(* {1 Answers} *)

(* An errno, as the traces name one: [EADDRINUSE], not [EOF]. *)
let is_errno t =
  String.length t > 1
  && t.[0] = 'E'
  && t <> "EOF"
  && String.for_all
       (function 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       t

(* An error with no errno, of a backend the traces do not know, is any
   error. *)
let token ~expected given =
  String.equal given expected || (given = "error" && is_errno expected)

let same ~expected given =
  String.equal given expected
  ||
  let e = split ' ' expected and g = split ' ' given in
  List.compare_lengths e g = 0
  && List.for_all2 (fun e g -> token ~expected:e g) e g

(* {1 Allowances}

   Where a scenario's answer depends on the host's configuration (its
   addresses, name server, services database or host name) or on how the
   kernel accounts for a buffer, other answers than the recorded one are
   allowed. *)

type allowance = {
  why : string;
  oses : os list;
  label : string; (* what the report says it allows *)
  applies : string -> bool; (* to a scenario, by its name *)
  holds : string -> kernel:string -> string -> bool;
}

let split_last c s =
  match String.rindex_opt s c with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

(* The addresses glibc gives on a host with no IPv6 address but loopback,
   for those a dual-stack host gives: IPv6 ones left out, IPv4-mapped ones
   turned into IPv4, and the ::1 that /etc/hosts gives localhost turned into
   127.0.0.1; "localhost." is not in /etc/hosts. *)
let without_ipv6 name given =
  let localhost =
    List.exists
      (fun n -> starts_with ~prefix:(Fmt.str "getaddrinfo %S " n) name)
      [ "localhost"; "LOCALHOST" ]
  in
  if not (starts_with ~prefix:"tcp:" given || starts_with ~prefix:"udp:" given)
  then given
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
    match List.filter_map v4 (String.split_on_char ' ' given) with
    | [] -> "ADDRFAMILY"
    | l -> String.concat " " l

let is_number s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

(* A word of an answer as a report names it: the traces write [blk] for an
   operation that would wait, [rN] or [rN:ADDR] for a read of N bytes, and
   [Fs] or [Net] for an Eio error of that class. *)
let rec word w =
  match split_last '=' w with
  | key, value when value <> "" && not (String.contains key ':') ->
      key ^ "=" ^ word value
  | _ -> (
      match w with
      | "blk" -> "would wait"
      | "Fs" -> "an Eio.Fs error"
      | "Net" -> "an Eio.Net error"
      | "error" -> "an error"
      | _ when String.length w > 1 && w.[0] = 'r' -> (
          let rest = String.sub w 1 (String.length w - 1) in
          let n, from =
            match String.index_opt rest ':' with
            | Some i ->
                ( String.sub rest 0 i,
                  " from " ^ String.sub rest (i + 1) (String.length rest - i - 1)
                )
            | None -> (rest, "")
          in
          match int_of_string_opt n with
          | Some k when is_number n ->
              Fmt.str "%d byte%s%s" k (Text.plural k) from
          | _ -> w)
      | _ -> w)

let shown answer = String.concat " " (List.map word (split ' ' answer))

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

(* [given] and [kernel] say the same but for their numbers, each of
   [given]'s within [factor] of the kernel's. *)
let within factor ~kernel given =
  let tg, ng = numbers given and tk, nk = numbers kernel in
  tg = tk
  && List.compare_lengths ng nk = 0
  && List.for_all2
       (fun g k ->
         let g = float g and k = float k in
         g <= factor *. k && k <= factor *. g)
       ng nk

let is_address s = starts_with ~prefix:"tcp:" s || starts_with ~prefix:"udp:" s

let table : allowance list =
  [
    {
      why =
        "a stream queues what the buffers set allow, within 15% of what Linux \
         holds in writes of 1 KiB, as the overhead of each segment and write \
         is Linux's own";
      oses = [ `Linux ];
      label = "the same within 15% of each number";
      applies = starts_with ~prefix:"sockopt fill: ";
      holds = (fun _ -> within 1.15);
    };
    {
      why =
        "macOS writes nothing into less room than a write's length and the \
         low-water mark, so a Unix stream with a small send buffer queues up \
         to half as much";
      oses = [ `Macos ];
      label = "the same within a factor of 2 of each number";
      applies = starts_with ~prefix:"sockopt fill: unix";
      holds = (fun _ -> within 2.);
    };
    {
      why =
        "macOS sizes the buffers of a TCP socket set before it connects, or \
         taken from its listener, to whole segments as it connects";
      oses = [ `Macos ];
      label = "the same with numbers no larger";
      applies =
        (fun name ->
          List.mem name
            [
              "sockopt inherit: tcp4 server SO_SNDBUF";
              "sockopt inherit: tcp4 server SO_RCVBUF";
              "sockopt connect: tcp4 client SO_SNDBUF";
              "sockopt connect: tcp4 client SO_RCVBUF";
            ]);
      holds =
        (fun _ ~kernel given ->
          let tg, ng = numbers given and tk, nk = numbers kernel in
          tg = tk
          && List.compare_lengths ng nk = 0
          && List.for_all2 ( <= ) ng nk);
    };
    {
      why =
        "macOS counts a datagram's memory in mbufs, which a network may count \
         by its length";
      oses = [ `Macos ];
      label = "the same within a factor of 2 of each number";
      applies =
        (fun name ->
          starts_with ~prefix:"sockopt udp rcvbuf" name
          || starts_with ~prefix:"sockopt unix dgram rcvbuf" name);
      holds = (fun _ -> within 2.);
    };
    {
      why =
        "the recording host has no IPv6 address but loopback, so glibc leaves \
         out IPv6 addresses, which a dual-stack host gives";
      oses = [ `Linux ];
      label = "the same with IPv6 addresses, as a dual-stack host gives";
      applies = starts_with ~prefix:"getaddrinfo";
      holds = (fun name ~kernel given -> without_ipv6 name given = kernel);
    };
    {
      why = "a host with no services database knows no service name";
      oses = [ `Linux; `Macos ];
      label = "SERVICE or NONAME, as a host with no services database answers";
      applies =
        (fun name ->
          starts_with ~prefix:"getaddrinfo" name
          && Filename.check_suffix name {|"http"|});
      holds =
        (fun _ ~kernel given ->
          (given = "SERVICE" || given = "NONAME")
          && (is_address kernel || kernel = "ADDRFAMILY"));
    };
    {
      why = "a host with no services database gives getnameinfo the port";
      oses = [ `Linux; `Macos ];
      label = "the port in place of the service";
      applies = starts_with ~prefix:"getnameinfo";
      holds =
        (fun _ ~kernel given ->
          let gh, gs = split_last '/' given
          and kh, ks = split_last '/' kernel in
          gh = kh && is_number gs && not (is_number ks));
    };
    {
      why = "a host with no host name gives a Unix address's host as localhost";
      oses = [ `Linux ];
      label = "localhost in place of the host name";
      applies = starts_with ~prefix:"getnameinfo unix";
      holds =
        (fun _ ~kernel given ->
          let gh, gs = split_last '/' given
          and kh, ks = split_last '/' kernel in
          gh = "localhost" && kh = "HOSTNAME" && gs = ks);
    };
    {
      why =
        "a host with no name server maps a name /etc/hosts does not hold, such \
         as localhost. with its final dot, to no address";
      oses = [ `Linux; `Macos ];
      label = "NONAME, as a host with no name server answers";
      applies =
        (fun name ->
          match String.split_on_char '"' name with
          | [ "getaddrinfo "; host; " "; _; "" ] ->
              String.ends_with ~suffix:"." host
          | _ -> false);
      holds = (fun _ ~kernel given -> given = "NONAME" && is_address kernel);
    };
    {
      why =
        "the Mac that records routes multicast through its own network \
         interface, which 127.0.0.1 is not on, where a host with loopback only \
         sends it";
      oses = [ `Macos ];
      label = "sent, as a host with loopback only answers";
      applies = String.equal "dgram error v4->224.0.0.1 len=10";
      holds =
        (fun _ ~kernel given -> given = "sent" && kernel = "EADDRNOTAVAIL");
    };
  ]

let allowances =
  List.concat_map (fun a -> List.map (fun os -> (os, a.why)) a.oses) table

(* {1 Scenarios left out} *)

(* The macOS scenarios whose answer depends on timing, as its loopback
   delivers asynchronously: one recording holds one of their answers. *)
let timing_dependent name =
  List.mem name
    [
      "reuse client closes first";
      "reuse bound server closes first";
      "dual-stack hold: listen :: client ::1 client closes first";
      "dual-stack hold: listen :: client 127.0.0.1 client closes first";
      "dual-stack hold: listen ::1 client ::1 client closes first";
      "dual-stack hold: listen 0.0.0.0 client 127.0.0.1 client closes first";
    ]
  || starts_with ~prefix:"sockopt fill: tcp4" name

(* A TCP sequence that reads or writes after an end shut its receive side
   while bytes or an end of stream were on their way to it, or after bytes
   went to an end that had shut it. macOS then sends a reset, and whether the
   operation sees it depends on when it runs. A close of the listener before
   the accept shuts the server's end. *)
let racy ops =
  let other = function "C" -> "S" | _ -> "C" in
  let towards = Hashtbl.create 2 and shut_down = Hashtbl.create 2 in
  let sent x = Hashtbl.replace towards (other x) () in
  let pending x = Hashtbl.mem towards x in
  let rec go ~accepted ~shut = function
    | [] -> false
    | op :: rest -> (
        match String.split_on_char ' ' op with
        | ("read" | "write") :: _ when shut -> true
        | "write" :: x :: _ ->
            sent x;
            go ~accepted ~shut:(shut || Hashtbl.mem shut_down (other x)) rest
        | [ "shutdown"; x; how ] ->
            let shut = shut || (how <> "send" && pending x) in
            if how <> "send" then Hashtbl.replace shut_down x ();
            if how <> "recv" then sent x;
            go ~accepted ~shut rest
        | [ "close"; x ] ->
            let shut = shut || pending x in
            sent x;
            go ~accepted ~shut rest
        | [ "close_listener" ] ->
            go ~accepted ~shut:(shut || ((not accepted) && pending "S")) rest
        | [ "accept" ] -> go ~accepted:true ~shut rest
        | _ -> go ~accepted ~shut rest)
  in
  go ~accepted:false ~shut:false ops

let timing = "have a macOS answer that depends on timing"

(* Why a sequence of [t] is left out under [os], if it is. *)
let sequence_left_out os t ops =
  match (os, t) with
  | `Macos, (K.Tcp4 | K.Tcp6) when racy ops -> Some timing
  | _ -> None

(* Why a scenario is left out under [os], if it is. *)
let left_out os name =
  if K.timed name then Some "wait 60 seconds for a port to be free again"
  else if K.narrow_ports name then Some "use up every port"
  else if K.off_loopback name then Some "send off the loopback interface"
  else if os = `Macos && timing_dependent name then Some timing
  else None

let reasons =
  [
    "wait 60 seconds for a port to be free again";
    "use up every port";
    "send off the loopback interface";
    timing;
  ]

(* {1 Replay} *)

let column os (linux, macos) = match os with `Linux -> linux | `Macos -> macos

(* A sequence's answers, one per operation, for each kernel. *)
let sequence_rows t =
  let codes = List.assoc (K.transport_name t) Device_traces.sequences in
  List.init
    (String.length codes / 2)
    (fun i ->
      let linux, macos =
        Device_traces.answers.(String.get_uint16_be codes (2 * i))
      in
      (split '|' linux, split '|' macos))

(* A scenario that does not end within that many turns of the event loop
   answers "timeout", as the recorder gave up on it. *)
let max_yields = 10_000

let bounded ~timeout f =
  Eio.Fiber.first f (fun () ->
      for _ = 1 to max_yields do
        Eio.Fiber.yield ()
      done;
      timeout)

let generic net = (net :> [ `Generic ] Eio.Net.ty Eio.Resource.t)

(* How a sequence gets its network: [run g] is [g net] on a fresh one, or
   [None] if its event loop hung after [g] began. *)
type fresh = {
  run : 'a. ([ `Generic ] Eio.Net.ty Eio.Resource.t -> 'a) -> 'a option;
}

(* Each sequence in an event loop of its own, on a simulated environment
   whose network follows [os]. *)
let simulated os f =
  let run g =
    let result = ref None in
    (match
       Quiet_mock.run ~effects:{ handle = Posix.systhread } ~limit:3600.
       @@ fun clock mono_clock _ ->
       Eio.Switch.run @@ fun sw ->
       let out = Sim.output () in
       Sim.with_process ~seed:[| 0 |] out @@ fun () ->
       let net = lazy (Eio_net_mem.v ~os ~mono:mono_clock ~sw ()) in
       let env = Sim.v ~net ~clock ~mono_clock ~seed:[| 0; 1 |] out in
       result := Some (g (generic (f ~sw env)))
     with
    | () -> ()
    | exception
        Quiet_mock.(
          Deadlock_detected _ | Time_limit | Busy | Too_long | Stopped) ->
        ());
    !result
  in
  { run }

(* Each sequence in the caller's event loop, as the host's own sockets
   need. *)
let in_loop f =
  { run = (fun g -> Some (Eio.Switch.run @@ fun sw -> g (generic (f ~sw)))) }

type difference = {
  step : string;
  expected : string;
  got : string;
  sequence : string option;
}

type summary = {
  differed : difference list;
  passed : int;
  left_out : (string * int) list;
  allowed : string list;
}

(* A sequence of [t] on a network [fresh] makes: where its answers leave
   those of [os], if they do. *)
let sequence os fresh t ops row =
  let got =
    fresh.run (fun net ->
        bounded ~timeout:[ "timeout" ] (fun () -> K.run net t ops))
    |> Option.value ~default:[ "timeout" ]
  in
  let rec first i = function
    | [], [] -> None
    | e :: es, g :: gs when token ~expected:e g -> first (i + 1) (es, gs)
    | es, gs ->
        let op = List.nth_opt ops i |> Option.value ~default:"end" in
        let upto = List.filteri (fun j _ -> j < i) ops @ [ op ] in
        let one = function [] -> "nothing" | x :: _ -> shown x in
        Some
          {
            step = op;
            expected = one es;
            got = one gs;
            sequence =
              Fmt.kstr Option.some "%s %s" (K.transport_name t)
                (String.concat "; " upto);
          }
  in
  first 0 (column os row, got)

(* A scenario on a network [fresh] makes, against the answer [kernel] of
   [os]. *)
let scenario os fresh sc ~kernel =
  let given =
    fresh.run (fun net ->
        bounded ~timeout:"timeout" (fun () -> K.scenario net sc))
    |> Option.value ~default:"timeout"
  in
  let these = List.filter (fun a -> List.mem os a.oses && a.applies sc) table in
  if same ~expected:kernel given then `Passed
  else
    match List.find_opt (fun a -> a.holds sc ~kernel given) these with
    | Some a -> `Allowed a.why
    | None ->
        let expected =
          match these with
          | [] -> shown kernel
          | _ ->
              Fmt.str "one of {%s}"
                (String.concat ", "
                   (shown kernel :: List.map (fun a -> a.label) these))
        in
        `Differs { step = sc; expected; got = shown given; sequence = None }

let out_of_date what ~held ~runs =
  if held <> runs then
    Fmt.failwith
      "%s: the traces hold %d %s, and eio-net runs %d: its traces are out of \
       date"
      name held what runs

let replay_with os fresh =
  let differed = ref [] and passed = ref 0 in
  let skipped = Hashtbl.create 4 and allowed = Hashtbl.create 16 in
  let skip why =
    Hashtbl.replace skipped why
      (1 + Option.value ~default:0 (Hashtbl.find_opt skipped why))
  in
  List.iter
    (fun t ->
      let rows = sequence_rows t and sequences = K.sequences t in
      out_of_date
        (K.transport_name t ^ " sequences")
        ~held:(List.length rows) ~runs:(List.length sequences);
      List.iter2
        (fun ops row ->
          match sequence_left_out os t ops with
          | Some why -> skip why
          | None -> (
              match sequence os fresh t ops row with
              | None -> incr passed
              | Some d -> differed := d :: !differed))
        sequences rows)
    K.transports;
  let rows = Device_traces.scenarios in
  out_of_date "scenarios" ~held:(Array.length rows)
    ~runs:(List.length K.scenarios);
  List.iteri
    (fun i sc ->
      match left_out os sc with
      | Some why -> skip why
      | None -> (
          match scenario os fresh sc ~kernel:(column os rows.(i)) with
          | `Passed -> incr passed
          | `Allowed why ->
              Hashtbl.replace allowed why ();
              incr passed
          | `Differs d -> differed := d :: !differed))
    K.scenarios;
  {
    differed = List.rev !differed;
    passed = !passed;
    left_out =
      List.filter_map
        (fun why ->
          Option.map (fun n -> (why, n)) (Hashtbl.find_opt skipped why))
        reasons;
    allowed = List.of_seq (Hashtbl.to_seq_keys allowed);
  }

(* The lines of a report: the differences at the same step with the same
   answers once, in the order the first came, with how many sequences gave
   them and the first as an example. *)
let grouped differed =
  let key d = (d.step, d.expected, d.got) in
  let counts = Hashtbl.create 64 in
  List.iter
    (fun d ->
      Hashtbl.replace counts (key d)
        (1 + Option.value ~default:0 (Hashtbl.find_opt counts (key d))))
    differed;
  List.filter_map
    (fun d ->
      match Hashtbl.find_opt counts (key d) with
      | None -> None
      | Some k ->
          Hashtbl.remove counts (key d);
          let what =
            Fmt.str "%s: expected %s, got %s" d.step d.expected d.got
          in
          Some
            (match d.sequence with
            | None -> what
            | Some sq when k = 1 -> Fmt.str "%s, in %s" what sq
            | Some sq ->
                Fmt.str "%s, in %a sequences, e.g. %s" what Text.pp_count k sq))
    differed

(* A report shows the first [shown_lines] lines, then counts the others. *)
let shown_lines = 100

let os_of : [< os ] option -> os = function
  | Some `Macos -> `Macos
  | Some `Linux | None -> `Linux

(* The report of a replay, if anything differed: its first lines, then the
   summary. *)
let report os s =
  match s.differed with
  | [] -> ()
  | differed ->
      let n = List.length differed in
      let left = List.fold_left (fun acc (_, k) -> acc + k) 0 s.left_out in
      let lines = grouped differed in
      let m = List.length lines in
      let lines =
        List.filteri (fun i _ -> i < shown_lines) lines
        @
        if m > shown_lines then
          [ Fmt.str "... and %d more lines" (m - shown_lines) ]
        else []
      in
      Fmt.failwith
        "%s: %a of %a sequences differ from %s@\n\
         %a@\n\
        \  %a passed, %a differed, %a left out%a"
        name Text.pp_count n Text.pp_count (s.passed + n) (os_name os)
        Fmt.(list ~sep:(any "@\n") (any "  " ++ string))
        lines Text.pp_count s.passed Text.pp_count n Text.pp_count left
        Fmt.(
          list ~sep:nop (fun ppf (why, k) ->
              pf ppf "; %a that %s" Text.pp_count k why))
        s.left_out

let replay os f = replay_with os (simulated os f)

(* The caller's Random goes on where it was, whatever the networks drew. *)
let check ?os f =
  let os = os_of os in
  let random = Random.get_state () in
  Fun.protect ~finally:(fun () -> Random.set_state random) @@ fun () ->
  report os (replay os f)

let check_in_loop ?os f =
  let os = os_of os in
  report os (replay_with os (in_loop f))
