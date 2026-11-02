open! Hermetic

type Eio.Exn.Backend.t += Injected_fault

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Injected_fault ->
        Fmt.string ppf "Injected_fault (eio-net.chaos)";
        true
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Injected_fault)

let refused_exn () =
  Eio.Net.err (Eio.Net.Connection_failure (Eio.Net.Refused Injected_fault))

let timeout_exn () = Eio.Net.err (Eio.Net.Connection_failure Eio.Net.Timeout)

let rec takev len = function
  | [] -> []
  | x :: xs ->
      if len <= 0 then []
      else if Cstruct.length x >= len then [ Cstruct.sub x 0 len ]
      else x :: takev (len - Cstruct.length x) xs

(* Formatters of the arguments of a printed schedule, which reads as the call
   that builds it: a negative number is parenthesised, as an argument must be,
   and a float is printed with the fewest digits that read back as it. *)
let pp_int ppf n = if n < 0 then Fmt.pf ppf "(%d)" n else Fmt.int ppf n

let pp_float ppf x =
  let rec shortest digits =
    let s = Printf.sprintf "%.*g" digits x in
    if digits >= 17 || Float.equal (float_of_string s) x then s
    else shortest (digits + 1)
  in
  let s = shortest 1 in
  if x < 0. then Fmt.pf ppf "(%s)" s else Fmt.string ppf s

let pp_list pp_v ppf l = Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any "; ") pp_v) l
let read_rng seed = Random.State.make [| seed; 0x5eed_1 |]
let write_rng seed = Random.State.make [| seed; 0x5eed_2 |]
let dgram_rng seed = Random.State.make [| seed; 0x5eed_3 |]

module Flow = struct
  type read = Deliver of int | Coalesce of int | Stall of int | Reset | Eof
  type write = Accept of int | Fail

  module Schedule = struct
    type seeded = {
      seed : int;
      dribble : int;
      stall : int;
      short_write : int;
      reset_after : int option;
      reset_kind : [ `Reset | `Eof ];
      fail_write_after : int option;
    }

    type t =
      | Pass
      | Script of { reads : read list; writes : write list }
      | Seeded of seeded

    let none = Pass
    let script ?(reads = []) ?(writes = []) () = Script { reads; writes }

    let seeded ?(dribble = 0) ?(stall = 0) ?(short_write = 0) ?reset_after
        ?(reset_kind = `Reset) ?fail_write_after seed =
      Seeded
        {
          seed;
          dribble;
          stall;
          short_write;
          reset_after;
          reset_kind;
          fail_write_after;
        }

    let pp_read ppf = function
      | Deliver n -> Fmt.pf ppf "Deliver %a" pp_int n
      | Coalesce n -> Fmt.pf ppf "Coalesce %a" pp_int n
      | Stall n -> Fmt.pf ppf "Stall %a" pp_int n
      | Reset -> Fmt.string ppf "Reset"
      | Eof -> Fmt.string ppf "Eof"

    let pp_write ppf = function
      | Accept n -> Fmt.pf ppf "Accept %a" pp_int n
      | Fail -> Fmt.string ppf "Fail"

    let pp_after name ppf = function
      | None -> ()
      | Some n -> Fmt.pf ppf " ~%s:%a" name pp_int n

    let pp ppf = function
      | Pass -> Fmt.string ppf "none"
      | Script { reads; writes } ->
          Fmt.pf ppf "script ~reads:%a ~writes:%a ()" (pp_list pp_read) reads
            (pp_list pp_write) writes
      | Seeded s ->
          Fmt.pf ppf
            "seeded ~dribble:%a ~stall:%a ~short_write:%a%a ~reset_kind:%s%a %a"
            pp_int s.dribble pp_int s.stall pp_int s.short_write
            (pp_after "reset_after") s.reset_after
            (match s.reset_kind with `Reset -> "`Reset" | `Eof -> "`Eof")
            (pp_after "fail_write_after")
            s.fail_write_after pp_int s.seed
  end

  type driver = { read : delivered:int -> read; write : written:int -> write }

  let driver (s : Schedule.t) : driver =
    match s with
    | Pass ->
        {
          read = (fun ~delivered:_ -> Deliver 0);
          write = (fun ~written:_ -> Accept 0);
        }
    | Script { reads; writes } ->
        let rq = ref reads and wq = ref writes in
        let read ~delivered:_ =
          match !rq with
          | [] -> Deliver 0
          | x :: xs ->
              rq := xs;
              x
        in
        let write ~written:_ =
          match !wq with
          | [] -> Accept 0
          | x :: xs ->
              wq := xs;
              x
        in
        { read; write }
    | Seeded s ->
        let rng_r = read_rng s.seed and rng_w = write_rng s.seed in
        let read_fired = ref false and write_fired = ref false in
        let read ~delivered =
          match s.reset_after with
          | Some n when (not !read_fired) && delivered >= n -> (
              read_fired := true;
              match s.reset_kind with `Reset -> Reset | `Eof -> Eof)
          | _ ->
              if s.stall > 0 && Random.State.bool rng_r then
                Stall (1 + Random.State.int rng_r s.stall)
              else if s.dribble > 0 then
                Deliver (1 + Random.State.int rng_r s.dribble)
              else Deliver 0
        in
        let write ~written =
          match s.fail_write_after with
          | Some n when (not !write_fired) && written >= n ->
              write_fired := true;
              Fail
          | _ ->
              if s.short_write > 0 then
                Accept (1 + Random.State.int rng_w s.short_write)
              else Accept 0
        in
        { read; write }

  (* The base socket's operations captured as closures, so the wrapper's state
     need not name the base's (polymorphic) tag type. *)
  type base = {
    read : Cstruct.t -> int;
    write_all : Cstruct.t list -> unit;
    shutdown : Eio.Flow.shutdown_command -> unit;
    close : unit -> unit;
    setopt : 'a. 'a Eio.Net.Sockopt.t -> 'a -> unit;
    getopt : 'a. 'a Eio.Net.Sockopt.t -> 'a;
  }

  let base_of (sock : _ Eio.Net.stream_socket) =
    let setopt : type a. a Eio.Net.Sockopt.t -> a -> unit =
     fun opt v -> Eio.Net.setsockopt sock opt v
    in
    let getopt : type a. a Eio.Net.Sockopt.t -> a =
     fun opt -> Eio.Net.getsockopt sock opt
    in
    {
      read = Eio.Flow.single_read sock;
      write_all = Eio.Flow.write sock;
      shutdown = Eio.Flow.shutdown sock;
      close = (fun () -> Eio.Resource.close sock);
      setopt;
      getopt;
    }

  module Impl = struct
    type tag = [ `Generic ]

    type t = {
      base : base;
      driver : driver;
      mutable leftover : Cstruct.t;
      mutable delivered : int;
      mutable written : int;
      mutable recv_closed : bool;
    }

    let fill_one t =
      let scratch = Cstruct.create (max (Cstruct.length t.leftover) 0x4000) in
      let got = t.base.read scratch in
      t.leftover <- Cstruct.append t.leftover (Cstruct.sub scratch 0 got)

    let ensure_buffer t = if Cstruct.length t.leftover = 0 then fill_one t

    let deliver t dst ~cap =
      ensure_buffer t;
      let avail = Cstruct.length t.leftover in
      let want = if cap <= 0 then avail else min cap avail in
      let n = min want (Cstruct.length dst) in
      Cstruct.blit t.leftover 0 dst 0 n;
      t.leftover <- Cstruct.sub t.leftover n (avail - n);
      t.delivered <- t.delivered + n;
      n

    let single_read t dst =
      if t.recv_closed then raise End_of_file;
      match t.driver.read ~delivered:t.delivered with
      | Reset -> raise (reset_exn ())
      | Eof ->
          t.recv_closed <- true;
          raise End_of_file
      | Stall n ->
          for _ = 1 to n do
            Eio.Fiber.yield ()
          done;
          deliver t dst ~cap:0
      | Deliver cap -> deliver t dst ~cap
      | Coalesce n ->
          (* One base read is mandatory (propagate its EOF); further reads are
             best-effort and stop at EOF. *)
          fill_one t;
          (try
             for _ = 2 to n do
               fill_one t
             done
           with End_of_file -> ());
          deliver t dst ~cap:0

    let single_write t bufs =
      let total = Cstruct.lenv bufs in
      if total = 0 then 0
      else
        match t.driver.write ~written:t.written with
        | Fail -> raise (reset_exn ())
        | Accept cap ->
            let n = if cap <= 0 then total else max 1 (min cap total) in
            t.base.write_all (takev n bufs);
            t.written <- t.written + n;
            n

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
    let read_methods = []
    let shutdown t cmd = t.base.shutdown cmd
    let close t = t.base.close ()
    let setsockopt t opt v = t.base.setopt opt v
    let getsockopt t opt = t.base.getopt opt
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t

  let wrap schedule (sock : _ Eio.Net.stream_socket) : flow =
    let state =
      {
        Impl.base = base_of sock;
        driver = driver schedule;
        leftover = Cstruct.empty;
        delivered = 0;
        written = 0;
        recv_closed = false;
      }
    in
    Eio.Resource.T (state, handler)

  let pair ?(client = Schedule.none) ?(server = Schedule.none) () =
    let c, s = Eio_net_mem.pipe () in
    (wrap client c, wrap server s)
end

module Datagram = struct
  type fault =
    | Pass
    | Drop of int
    | Dup
    | Reorder
    | Truncate of int
    | Garble of int
    | Delay of int

  module Schedule = struct
    type seeded = {
      seed : int;
      loss : float;
      dup : float;
      reorder : float;
      truncate : float;
      garble : float;
      delay : int;
    }

    type t = Pass | Script of fault list | Seeded of seeded

    let none = Pass

    let script events =
      List.iter
        (function
          | Drop n when n < 0 ->
              Fmt.invalid_arg
                "Eio_net_chaos.Datagram.Schedule.script: Drop %d is negative" n
          | _ -> ())
        events;
      Script events

    let seeded ?(loss = 0.) ?(dup = 0.) ?(reorder = 0.) ?(truncate = 0.)
        ?(garble = 0.) ?(delay = 0) seed =
      let sum = loss +. dup +. reorder +. truncate +. garble in
      if sum > 1. then
        Fmt.invalid_arg
          "Eio_net_chaos.Datagram.Schedule.seeded: the probabilities sum to %g"
          sum;
      Seeded { seed; loss; dup; reorder; truncate; garble; delay }

    let pp_fault ppf (fault : fault) =
      match fault with
      | Pass -> Fmt.string ppf "Pass"
      | Drop n -> Fmt.pf ppf "Drop %a" pp_int n
      | Dup -> Fmt.string ppf "Dup"
      | Reorder -> Fmt.string ppf "Reorder"
      | Truncate n -> Fmt.pf ppf "Truncate %a" pp_int n
      | Garble n -> Fmt.pf ppf "Garble %a" pp_int n
      | Delay n -> Fmt.pf ppf "Delay %a" pp_int n

    let pp ppf = function
      | Pass -> Fmt.string ppf "none"
      | Script events -> Fmt.pf ppf "script %a" (pp_list pp_fault) events
      | Seeded s ->
          Fmt.pf ppf
            "seeded ~loss:%a ~dup:%a ~reorder:%a ~truncate:%a ~garble:%a \
             ~delay:%a %a"
            pp_float s.loss pp_float s.dup pp_float s.reorder pp_float
            s.truncate pp_float s.garble pp_int s.delay pp_int s.seed
  end

  type driver = { next : unit -> fault; rng : Random.State.t }

  let driver (s : Schedule.t) : driver =
    match s with
    | Pass -> { next = (fun () -> Pass); rng = dgram_rng 0 }
    | Script events ->
        let q = ref events in
        let next () =
          match !q with
          | [] -> Pass
          | x :: xs ->
              q := xs;
              x
        in
        { next; rng = dgram_rng 0 }
    | Seeded s ->
        let rng = dgram_rng s.seed in
        let next () =
          let r = Random.State.float rng 1.0 in
          let l = s.loss in
          let d = l +. s.dup in
          let o = d +. s.reorder in
          let t = o +. s.truncate in
          let g = t +. s.garble in
          if r < l then Drop 1
          else if r < d then Dup
          else if r < o then Reorder
          else if r < t then Truncate (Random.State.int rng 8)
          else if r < g then Garble (Random.State.int rng 16)
          else if s.delay > 0 && Random.State.bool rng then
            Delay (1 + Random.State.int rng s.delay)
          else Pass
        in
        { next; rng }

  type base = {
    recv : Cstruct.t -> Eio.Net.Sockaddr.datagram * int;
    send : ?dst:Eio.Net.Sockaddr.datagram -> Cstruct.t list -> unit;
    shutdown : Eio.Flow.shutdown_command -> unit;
    close : unit -> unit;
    setopt : 'a. 'a Eio.Net.Sockopt.t -> 'a -> unit;
    getopt : 'a. 'a Eio.Net.Sockopt.t -> 'a;
  }

  (* A datagram socket is not a two-way flow, so {!Eio.Flow.shutdown} does not
     apply; dispatch the base's own [Shutdown] provider directly. *)
  let shutdown_of (Eio.Resource.T (t, ops) : _ Eio.Net.datagram_socket) =
    let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
    fun cmd -> X.shutdown t cmd

  let base_of (sock : _ Eio.Net.datagram_socket) =
    let setopt : type a. a Eio.Net.Sockopt.t -> a -> unit =
     fun opt v -> Eio.Net.setsockopt sock opt v
    in
    let getopt : type a. a Eio.Net.Sockopt.t -> a =
     fun opt -> Eio.Net.getsockopt sock opt
    in
    {
      recv = Eio.Net.recv sock;
      send = (fun ?dst bufs -> Eio.Net.send sock ?dst bufs);
      shutdown = shutdown_of sock;
      close = (fun () -> Eio.Resource.close sock);
      setopt;
      getopt;
    }

  module Impl = struct
    type tag = [ `Generic ]

    type t = {
      base : base;
      driver : driver;
      mutable deferred : (Eio.Net.Sockaddr.datagram * string) option;
    }

    let recv_raw t buf =
      let addr, len = t.base.recv buf in
      (addr, Cstruct.to_string (Cstruct.sub buf 0 len))

    let put buf addr data =
      let n = min (String.length data) (Cstruct.length buf) in
      Cstruct.blit_from_string data 0 buf 0 n;
      (addr, n)

    let rec apply t buf fault =
      match fault with
      | Pass ->
          let a, d = recv_raw t buf in
          put buf a d
      | Drop 0 -> apply t buf Pass
      | Drop n ->
          ignore (recv_raw t buf);
          apply t buf (Drop (n - 1))
      | Dup ->
          let a, d = recv_raw t buf in
          t.deferred <- Some (a, d);
          put buf a d
      | Reorder ->
          let a1, d1 = recv_raw t buf in
          let a2, d2 = recv_raw t buf in
          t.deferred <- Some (a1, d1);
          put buf a2 d2
      | Truncate k ->
          let a, d = recv_raw t buf in
          let d = if k < String.length d then String.sub d 0 (max 0 k) else d in
          put buf a d
      | Garble k ->
          let a, _ = recv_raw t buf in
          let d =
            String.init (max 0 k) (fun _ ->
                Char.chr (Random.State.int t.driver.rng 256))
          in
          put buf a d
      | Delay n ->
          for _ = 1 to n do
            Eio.Fiber.yield ()
          done;
          let a, d = recv_raw t buf in
          put buf a d

    let recv t buf =
      match t.deferred with
      | Some (addr, data) ->
          t.deferred <- None;
          put buf addr data
      | None -> apply t buf (t.driver.next ())

    let send t ?dst bufs = t.base.send ?dst bufs
    let shutdown t cmd = t.base.shutdown cmd
    let close t = t.base.close ()
    let setsockopt t opt v = t.base.setopt opt v
    let getsockopt t opt = t.base.getopt opt
  end

  let handler = Eio.Net.Pi.datagram_socket (module Impl)

  type socket = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t

  let wrap schedule (sock : _ Eio.Net.datagram_socket) : socket =
    let state =
      { Impl.base = base_of sock; driver = driver schedule; deferred = None }
    in
    Eio.Resource.T (state, handler)
end

module Net = struct
  type connect =
    | Open (* Connect normally (the flow is wrapped by the flow schedule). *)
    | Refuse (* Raise a connection-refused error. *)
    | Timeout (* Raise a connection-timeout error. *)
    | Slow of int (* [Eio.Fiber.yield] [n] times, then connect. *)

  module Schedule = struct
    type t = {
      flow : Flow.Schedule.t;
      connect : connect list;
      max_accepts : int option;
    }

    let v ?(flow = Flow.Schedule.none) ?(connect = []) ?max_accepts () =
      { flow; connect; max_accepts }

    let seeded ?dribble ?stall ?short_write ?reset_after ?reset_kind
        ?fail_write_after ?(connect = []) ?max_accepts seed =
      {
        flow =
          Flow.Schedule.seeded ?dribble ?stall ?short_write ?reset_after
            ?reset_kind ?fail_write_after seed;
        connect;
        max_accepts;
      }

    let pp_connect ppf = function
      | Open -> Fmt.string ppf "Open"
      | Refuse -> Fmt.string ppf "Refuse"
      | Timeout -> Fmt.string ppf "Timeout"
      | Slow n -> Fmt.pf ppf "Slow %a" pp_int n

    let pp ppf t =
      Fmt.pf ppf "v ~flow:(%a) ~connect:%a%a ()" Flow.Schedule.pp t.flow
        (pp_list pp_connect) t.connect
        (Flow.Schedule.pp_after "max_accepts")
        t.max_accepts
  end

  (* The base network captured as closures that already produce wrapped,
     [`Generic]-tagged sockets, so the provider modules stay monomorphic. *)
  type base = {
    connect :
      bind_to:Eio.Net.Sockaddr.stream option ->
      options:Eio.Net.Sockopt.settings ->
      sw:Eio.Switch.t ->
      Eio.Net.Sockaddr.stream ->
      Flow.flow;
    listen :
      reuse_addr:bool ->
      reuse_port:bool ->
      backlog:int ->
      sw:Eio.Switch.t ->
      Eio.Net.Sockaddr.stream ->
      base_listener;
    datagram :
      reuse_addr:bool ->
      reuse_port:bool ->
      sw:Eio.Switch.t ->
      [ Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6 ] ->
      [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t;
    getaddrinfo : service:string -> string -> Eio.Net.Sockaddr.t list;
    getnameinfo : Eio.Net.Sockaddr.t -> string * string;
  }

  and base_listener = {
    accept : sw:Eio.Switch.t -> Flow.flow * Eio.Net.Sockaddr.stream;
    listener_close : unit -> unit;
    listener_addr : unit -> Eio.Net.Sockaddr.stream;
    listener_setopt : 'a. 'a Eio.Net.Sockopt.t -> 'a -> unit;
    listener_getopt : 'a. 'a Eio.Net.Sockopt.t -> 'a;
  }

  module Listening_socket = struct
    type tag = [ `Generic ]

    type t = {
      base : base_listener;
      max_accepts : int option;
      mutable accepts : int;
    }

    let accept t ~sw =
      (match t.max_accepts with
      | Some m when t.accepts >= m ->
          t.base.listener_close ();
          raise End_of_file
      | _ -> ());
      t.accepts <- t.accepts + 1;
      t.base.accept ~sw

    let close t = t.base.listener_close ()
    let listening_addr t = t.base.listener_addr ()
    let setsockopt t opt v = t.base.listener_setopt opt v
    let getsockopt t opt = t.base.listener_getopt opt
  end

  let listening_handler = Eio.Net.Pi.listening_socket (module Listening_socket)

  module Network = struct
    type tag = [ `Generic ]

    type t = {
      base : base;
      schedule : Schedule.t;
      connect_q : connect list ref;
    }

    let next_connect t =
      match !(t.connect_q) with
      | [] -> Open
      | x :: xs ->
          t.connect_q := xs;
          x

    let connect t ~bind_to ~options ~sw addr =
      match next_connect t with
      | Refuse -> raise (refused_exn ())
      | Timeout -> raise (timeout_exn ())
      | Slow n ->
          for _ = 1 to n do
            Eio.Fiber.yield ()
          done;
          t.base.connect ~bind_to ~options ~sw addr
      | Open -> t.base.connect ~bind_to ~options ~sw addr

    let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
      let base = t.base.listen ~reuse_addr ~reuse_port ~backlog ~sw addr in
      let state =
        {
          Listening_socket.base;
          max_accepts = t.schedule.max_accepts;
          accepts = 0;
        }
      in
      Eio.Resource.T (state, listening_handler)

    let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
      t.base.datagram ~reuse_addr ~reuse_port ~sw addr

    let getaddrinfo t ~service node = t.base.getaddrinfo ~service node
    let getnameinfo t addr = t.base.getnameinfo addr
  end

  let handler = Eio.Net.Pi.network (module Network)

  let base_of ~flow (net : _ Eio.Net.t) =
    let connect ~bind_to ~options ~sw addr =
      Flow.wrap flow (Eio.Net.connect ?bind_to ~options ~sw net addr)
    in
    let listen ~reuse_addr ~reuse_port ~backlog ~sw addr =
      let l = Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw net addr in
      let accept ~sw =
        let sock, peer = Eio.Net.accept ~sw l in
        (Flow.wrap flow sock, peer)
      in
      let listener_setopt : type a. a Eio.Net.Sockopt.t -> a -> unit =
       fun opt v -> Eio.Net.setsockopt l opt v
      in
      let listener_getopt : type a. a Eio.Net.Sockopt.t -> a =
       fun opt -> Eio.Net.getsockopt l opt
      in
      {
        accept;
        listener_close = (fun () -> Eio.Resource.close l);
        listener_addr = (fun () -> Eio.Net.listening_addr l);
        listener_setopt;
        listener_getopt;
      }
    in
    let datagram ~reuse_addr:_ ~reuse_port:_ ~sw:_ _addr =
      invalid_arg "Eio_net_chaos.Net: datagram sockets are not wrapped here"
    in
    {
      connect;
      listen;
      datagram;
      getaddrinfo = (fun ~service node -> Eio.Net.getaddrinfo ~service net node);
      getnameinfo = (fun addr -> Eio.Net.getnameinfo net addr);
    }

  type t = [ `Generic ] Eio.Net.ty Eio.Resource.t

  let wrap (schedule : Schedule.t) (net : _ Eio.Net.t) : t =
    let state =
      {
        Network.base = base_of ~flow:schedule.flow net;
        schedule;
        connect_q = ref schedule.connect;
      }
    in
    Eio.Resource.T (state, handler)

  let v ~sw schedule = wrap schedule (Eio_net_mem.v ~sw ())
end
