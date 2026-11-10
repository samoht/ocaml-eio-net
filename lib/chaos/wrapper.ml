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

let rec takev len = function
  | [] -> []
  | b :: bs ->
      let n = Cstruct.length b in
      if len <= 0 then []
      else if n <= len then b :: takev (len - n) bs
      else [ Cstruct.sub b 0 len ]

type stream = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t

(* A connection, shared by its two ends. A reset breaks both, as on TCP. An
   end of stream is the close of one end, [`Closed closer]: its peer reads
   what it wrote before, then end of file. [broken] changes once, under
   [lock], which also guards [ends]: an end added after the break is shut at
   once. *)
type conn = {
  lock : Mutex.t;
  broken : [ `Open | `Reset | `Closed of Op.side ] Atomic.t;
  mutable ends : (Op.side * stream) list;
}

let conn () = { lock = Mutex.create (); broken = Atomic.make `Open; ends = [] }

let other : Op.side -> Op.side = function
  | `Client -> `Server
  | `Server -> `Client

let shutdown s cmd =
  try Eio.Flow.shutdown s cmd with Eio.Io _ | Invalid_argument _ -> ()

(* What the socket of the end [side] does once [conn] broke: on a close, the
   closer's sends end, so its peer reads what it wrote, then end of file. *)
let shut broken (side, s) =
  match broken with
  | `Reset -> shutdown s `All
  | `Closed closer -> if side = closer then shutdown s `Send
  | `Open -> ()

(* [at] is the end whose operation met the fault. Over a network that pairs no
   ends, a closer that is not there is the real peer: [at]'s receive side is
   shut, so it reads what is queued, then end of file. *)
let break conn ~at how =
  let ends =
    Mutex.protect conn.lock (fun () ->
        if Atomic.get conn.broken = `Open then begin
          Atomic.set conn.broken how;
          conn.ends
        end
        else [])
  in
  List.iter (shut how) ends;
  match how with
  | `Closed closer when ends <> [] && not (List.mem_assoc closer ends) ->
      List.iter (fun (side, s) -> if side = at then shutdown s `Receive) ends
  | _ -> ()

let join conn side sock =
  let broken =
    Mutex.protect conn.lock (fun () ->
        conn.ends <- (side, sock) :: conn.ends;
        Atomic.get conn.broken)
  in
  shut broken (side, sock)

(* The client registers its socket number once its connect returns; an accept
   that sees the connection first waits for it. *)
type pending = { number : int Eio.Promise.t; set : int Eio.Promise.u; c : conn }

(* What the wrappers of one network share. [lock] guards the counters and
   [pending], and runs [f], so [f] sees one operation at a time, in the order
   of [index]. *)
type ctx = {
  f : Op.t -> Fault.t option;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  outage : bool;
  lock : Mutex.t;
  mutable index : int;
  mutable sockets : int;
  mutable connects : int;
  mutable accepts : int;
  mutable exhausted : bool;
  mutable streak : int;
  mutable streak_at : float;
  livelocked : bool Atomic.t;
  pending : (int, pending) Hashtbl.t;
}

let ctx ~clock ~outage f =
  {
    f;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    outage;
    lock = Mutex.create ();
    index = 0;
    sockets = 0;
    connects = 0;
    accepts = 0;
    exhausted = false;
    streak = 0;
    streak_at = 0.;
    livelocked = Atomic.make false;
    pending = Hashtbl.create 7;
  }

(* Under [ctx.lock]. *)
let ask_locked ctx kind ~socket ~side ~offset ~length =
  ctx.index <- ctx.index + 1;
  ctx.f { Op.index = ctx.index; kind; socket; side; offset; length }

let locked (ctx : ctx) f = Mutex.protect ctx.lock f

(* A program that retries a failed operation at once, with no time passing,
   spins for ever: an outage refuses each connect at once. After [threshold]
   failures in a row while the clock stands still, the operation raises
   [Failure] instead, and after twice as many it waits for ever, as nothing
   the program does can get it out. *)
let threshold = 10_000

let livelock_message =
  Fmt.str "%d network operations failed in a row while its clock stood still"
    threshold

let failed ctx =
  let n =
    locked ctx (fun () ->
        let now = Eio.Time.now ctx.clock in
        if now <> ctx.streak_at then begin
          ctx.streak_at <- now;
          ctx.streak <- 0
        end;
        ctx.streak <- ctx.streak + 1;
        ctx.streak)
  in
  if n >= threshold then begin
    Atomic.set ctx.livelocked true;
    if n >= 2 * threshold then
      Eio.Cancel.protect (fun () ->
          Eio.Promise.await (fst (Eio.Promise.create ())));
    failwith ("Eio_net_chaos: " ^ livelock_message)
  end

let guard ctx f =
  match f () with
  | v ->
      locked ctx (fun () -> ctx.streak <- 0);
      v
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      failed ctx;
      Printexc.raise_with_backtrace e bt

(* A close frees a descriptor, so the accepts that [Fault.Emfile] fails go
   through again. *)
let free ctx = locked ctx (fun () -> ctx.exhausted <- false)

let ask ctx kind ~socket ~side ~offset ~length =
  locked ctx (fun () -> ask_locked ctx kind ~socket ~side ~offset ~length)

let next_socket ctx =
  ctx.sockets <- ctx.sockets + 1;
  ctx.sockets

let pending ctx id =
  match Hashtbl.find_opt ctx.pending id with
  | Some p ->
      Hashtbl.remove ctx.pending id;
      p
  | None ->
      let number, set = Eio.Promise.create () in
      let p = { number; set; c = conn () } in
      Hashtbl.replace ctx.pending id p;
      p

(* The connection a connect made, numbered [socket]. *)
let client_conn ctx ~socket sock =
  match Eio_net_mem.connection sock with
  | None -> conn ()
  | Some id ->
      let p = locked ctx (fun () -> pending ctx id) in
      Eio.Promise.resolve p.set socket;
      p.c

(* The number and the connection of an accepted socket. *)
let server_conn ctx sock =
  match Eio_net_mem.connection sock with
  | None -> (locked ctx (fun () -> next_socket ctx), conn ())
  | Some id ->
      let p = locked ctx (fun () -> pending ctx id) in
      (Eio.Promise.await p.number, p.c)

module Flow = struct
  (* [leftover] holds the bytes a base read took and no read has handed up:
     they stand for the socket's own queue, so a close drops them and a
     receive shutdown leaves them to read, as the base does with its queue.
     [lock] guards them, [closed], which a close in another domain changes
     while a read runs, and the counts of bytes, which readers and writers in
     several domains share. It is never held across a read or a write of the
     base socket. *)
  type t = {
    ctx : ctx;
    socket : int;
    side : Op.side;
    sock : stream;
    conn : conn;
    lock : Mutex.t;
    mutable leftover : Cstruct.t;
    mutable delivered : int;
    mutable written : int;
    mutable closed : bool;
    mutable reset_reported : bool;
    mutable wrote_after_end : bool;
  }

  (* Under [t.lock]. A reset drops what is buffered, and is reported once, by
     the first read or write that meets it: later reads return end of file,
     as a socket clears its pending error. An end that closed reads end of
     file; its peer reads on, to the end its socket now reaches. *)
  let check_read t =
    if t.closed then invalid_arg "Eio_net_chaos: read on a closed flow";
    match Atomic.get t.conn.broken with
    | `Reset ->
        t.leftover <- Cstruct.empty;
        if t.reset_reported then raise End_of_file;
        t.reset_reported <- true;
        raise (reset_exn ())
    | `Closed closer when closer = t.side ->
        t.leftover <- Cstruct.empty;
        raise End_of_file
    | `Closed _ | `Open -> ()

  (* Under [t.lock]. A closed flow refuses a write before any fault. After its
     peer closed, an end's first write goes through, its bytes lost, and the
     next ones raise the reset the peer answers with, as on Linux. *)
  let check_write t =
    if t.closed then invalid_arg "Eio_net_chaos: write on a closed flow";
    let reset () =
      t.reset_reported <- true;
      raise (reset_exn ())
    in
    match Atomic.get t.conn.broken with
    | `Open -> `Write
    | `Reset -> reset ()
    | `Closed closer when closer = t.side -> reset ()
    | `Closed _ ->
        if t.wrote_after_end then reset ();
        t.wrote_after_end <- true;
        `Lost

  let write_reset t =
    Mutex.protect t.lock (fun () -> t.reset_reported <- true);
    raise (reset_exn ())

  let broken t = Atomic.get t.conn.broken <> `Open

  (* Whether this end's own reads are cut: a reset, or its own close. *)
  let cut t =
    match Atomic.get t.conn.broken with
    | `Reset -> true
    | `Closed closer -> closer = t.side
    | `Open -> false

  (* Appends one base read to the buffer. A base error after a break is the
     break's. *)
  let fill t =
    let scratch = Cstruct.create 0x4000 in
    let got =
      match Eio.Flow.single_read t.sock scratch with
      | n -> n
      | exception (End_of_file | Eio.Io _) when cut t -> 0
    in
    Mutex.protect t.lock @@ fun () ->
    check_read t;
    t.leftover <- Cstruct.append t.leftover (Cstruct.sub scratch 0 got)

  (* Hands up at most [cap] buffered bytes, or all of them if [cap <= 0]. A
     byte is taken from the buffer once, under the lock, so readers in several
     domains neither lose nor duplicate one. *)
  let rec deliver t dst ~cap =
    let empty =
      Mutex.protect t.lock (fun () ->
          check_read t;
          Cstruct.length t.leftover = 0)
    in
    if empty then fill t;
    let n =
      Mutex.protect t.lock @@ fun () ->
      check_read t;
      let avail = Cstruct.length t.leftover in
      if avail = 0 then None
      else
        let want = if cap <= 0 then avail else min cap avail in
        let n = min want (Cstruct.length dst) in
        Cstruct.blit t.leftover 0 dst 0 n;
        t.leftover <- Cstruct.sub t.leftover n (avail - n);
        t.delivered <- t.delivered + n;
        Some n
    in
    match n with Some n -> n | None -> deliver t dst ~cap

  let raise_broken t =
    Mutex.protect t.lock (fun () -> check_read t);
    raise (reset_exn ())

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let single_read_ t dst =
      let fault =
        Mutex.protect t.lock (fun () ->
            check_read t;
            if Cstruct.length dst = 0 then None
            else
              ask t.ctx `Read ~socket:t.socket ~side:t.side ~offset:t.delivered
                ~length:(Cstruct.length dst))
      in
      if Cstruct.length dst = 0 then 0
      else
        match fault with
        | Some (Fault.Short n) -> deliver t dst ~cap:(max 1 n)
        | Some (Stall d) ->
            Eio.Time.sleep t.ctx.clock d;
            deliver t dst ~cap:0
        | Some Hang -> Eio.Fiber.await_cancel ()
        | Some Reset ->
            break t.conn ~at:t.side `Reset;
            raise_broken t
        | Some Eof ->
            break t.conn ~at:t.side (`Closed (other t.side));
            deliver t dst ~cap:0
        | Some
            ( Refuse | Timeout | Unreachable | Emfile | Drop | Dup | Reorder
            | Garble _ )
        | None ->
            deliver t dst ~cap:0

    (* The bytes are counted before they are written, so a write in another
       domain meanwhile sees them. *)
    let single_write_ t bufs =
      let total = Cstruct.lenv bufs in
      let fault, n =
        Mutex.protect t.lock @@ fun () ->
        match check_write t with
        | `Lost -> (None, -1)
        | `Write when total = 0 -> (None, 0)
        | `Write ->
            let fault =
              ask t.ctx `Write ~socket:t.socket ~side:t.side ~offset:t.written
                ~length:total
            in
            let n =
              match fault with
              | Some (Fault.Short k) -> max 1 (min k total)
              | Some (Hang | Reset | Eof) -> 0
              | Some
                  ( Stall _ | Refuse | Timeout | Unreachable | Emfile | Drop
                  | Dup | Reorder | Garble _ )
              | None ->
                  total
            in
            t.written <- t.written + n;
            (fault, n)
      in
      let write () =
        match Eio.Flow.write t.sock (takev n bufs) with
        | () -> n
        | exception Eio.Io _ when broken t -> write_reset t
      in
      if n < 0 then total
      else if total = 0 then 0
      else
        match fault with
        | Some Fault.Hang -> Eio.Fiber.await_cancel ()
        | Some Reset ->
            break t.conn ~at:t.side `Reset;
            write_reset t
        | Some Eof ->
            break t.conn ~at:t.side (`Closed t.side);
            write_reset t
        | Some (Stall d) ->
            Eio.Time.sleep t.ctx.clock d;
            write ()
        | Some
            ( Short _ | Refuse | Timeout | Unreachable | Emfile | Drop | Dup
            | Reorder | Garble _ )
        | None ->
            write ()

    let single_read t dst = guard t.ctx (fun () -> single_read_ t dst)
    let single_write t bufs = guard t.ctx (fun () -> single_write_ t bufs)
    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
    let read_methods = []

    let shutdown t cmd =
      try Eio.Flow.shutdown t.sock cmd with Eio.Io _ when broken t -> ()

    let close t =
      let was_open =
        Mutex.protect t.lock (fun () ->
            let was_open = not t.closed in
            t.leftover <- Cstruct.empty;
            t.closed <- true;
            was_open)
      in
      Eio.Resource.close t.sock;
      if was_open then free t.ctx

    let setsockopt t opt v = Eio.Net.setsockopt t.sock opt v
    let getsockopt t opt = Eio.Net.getsockopt t.sock opt
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  let v ctx ~socket ~side ~conn sock : stream =
    join conn side sock;
    Eio.Resource.T
      ( {
          ctx;
          socket;
          side;
          sock;
          conn;
          lock = Mutex.create ();
          leftover = Cstruct.empty;
          delivered = 0;
          written = 0;
          closed = false;
          reset_reported = false;
          wrote_after_end = false;
        },
        handler )
end

module Datagram = struct
  type socket = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t
  type datagram = Eio.Net.Sockaddr.datagram * string

  (* [ready] holds the datagrams to deliver before the next one the base
     receives: a duplicate, a held datagram released, or one taken before a
     stall. [held] holds the datagrams to deliver after the next one. Both are
     kept in the state while a receive waits, so a cancelled receive loses
     none, and both go with a close. [lock] guards them, the count and
     [closed], and is never held across a receive of the base socket. *)
  type t = {
    ctx : ctx;
    socket : int;
    sock : socket;
    lock : Mutex.t;
    mutable ready : datagram list;
    mutable held : datagram list;
    mutable received : int;
    mutable closed : bool;
  }

  let check t =
    if t.closed then
      invalid_arg "Eio_net_chaos: recv on a closed datagram socket"

  (* The whole datagram, up to 64 KiB or the caller's buffer if longer, so a
     copy kept for a later receive is cut only to the buffer of the receive
     that delivers it. *)
  let recv_raw t buf =
    let scratch = Cstruct.create_unsafe (max (Cstruct.length buf) 0x10000) in
    let addr, len = Eio.Net.recv t.sock scratch in
    (addr, Cstruct.to_string ~len scratch)

  let put buf (addr, data) =
    let n = min (String.length data) (Cstruct.length buf) in
    Cstruct.blit_from_string data 0 buf 0 n;
    (addr, n)

  let noise ~socket ~offset k =
    let rng = Random.State.make [| socket; offset |] in
    String.init (max 0 k) (fun _ -> Char.chr (Random.State.int rng 256))

  let shutdown_of (Eio.Resource.T (t, ops) : socket) =
    let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
    X.shutdown t

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    (* The fault is drawn once a datagram is taken, so a receive cancelled
       while it waits for one draws none. *)
    let rec recv_ t buf =
      let next =
        Mutex.protect t.lock (fun () ->
            check t;
            match t.ready with
            | d :: rest ->
                t.ready <- rest;
                Some d
            | [] -> None)
      in
      match next with
      | Some d -> put buf d
      | None -> (
          let ((addr, _) as d) = recv_raw t buf in
          let action =
            Mutex.protect t.lock @@ fun () ->
            check t;
            let offset = t.received in
            t.received <- offset + 1;
            match t.held with
            | _ :: _ ->
                (* The datagram after a held one passes, and the held one
                   follows it. *)
                t.ready <- t.ready @ List.rev t.held;
                t.held <- [];
                `Pass
            | [] -> (
                let fault =
                  ask t.ctx `Recv ~socket:t.socket ~side:`Client ~offset
                    ~length:(Cstruct.length buf)
                in
                match fault with
                | Some Fault.Drop -> `Again
                | Some Dup ->
                    t.ready <- t.ready @ [ d ];
                    `Pass
                | Some Reorder ->
                    t.held <- d :: t.held;
                    `Again
                | Some (Garble k) ->
                    `Deliver (addr, noise ~socket:t.socket ~offset k)
                | Some (Stall s) ->
                    t.ready <- d :: t.ready;
                    `Wait (Some s)
                | Some Hang ->
                    t.ready <- d :: t.ready;
                    `Wait None
                | Some
                    ( Short _ | Reset | Eof | Refuse | Timeout | Unreachable
                    | Emfile )
                | None ->
                    `Pass)
          in
          match action with
          | `Pass -> put buf d
          | `Deliver d -> put buf d
          | `Again -> recv_ t buf
          | `Wait (Some s) ->
              Eio.Time.sleep t.ctx.clock s;
              recv_ t buf
          | `Wait None -> Eio.Fiber.await_cancel ())

    let recv t buf = guard t.ctx (fun () -> recv_ t buf)
    let send t ?dst bufs = Eio.Net.send t.sock ?dst bufs
    let shutdown t cmd = shutdown_of t.sock cmd

    (* A datagram kept back goes with the socket. *)
    let close t =
      Mutex.protect t.lock (fun () ->
          t.ready <- [];
          t.held <- [];
          t.closed <- true);
      Eio.Resource.close t.sock;
      free t.ctx

    let setsockopt t opt v = Eio.Net.setsockopt t.sock opt v
    let getsockopt t opt = Eio.Net.getsockopt t.sock opt
  end

  let handler = Eio.Net.Pi.datagram_socket (module Impl)

  let v ctx (sock : socket) : socket =
    let socket = locked ctx (fun () -> next_socket ctx) in
    Eio.Resource.T
      ( {
          ctx;
          socket;
          sock;
          lock = Mutex.create ();
          ready = [];
          held = [];
          received = 0;
          closed = false;
        },
        handler )
end

module Listener = struct
  (* [held] is the connection an accept failed with EMFILE took, which stays
     queued until a descriptor is freed. *)
  type t = {
    ctx : ctx;
    l : [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
    mutable held : (stream * Eio.Net.Sockaddr.stream) option;
  }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let rec accept_ t ~sw =
      let ctx = t.ctx in
      let queued =
        locked ctx (fun () ->
            if ctx.exhausted then `Exhausted
            else
              match t.held with
              | Some c ->
                  t.held <- None;
                  `Held c
              | None -> `Accept)
      in
      match queued with
      | `Exhausted -> raise (Posix.emfile ())
      | `Held c -> c
      | `Accept -> accept_base t ~sw

    and accept_base t ~sw =
      let ctx = t.ctx in
      let sock, peer = Eio.Net.accept ~sw t.l in
      let sock = (sock :> stream) in
      let socket, conn = server_conn ctx sock in
      let flow = Flow.v ctx ~socket ~side:`Server ~conn sock in
      let fault =
        locked ctx (fun () ->
            let offset = ctx.accepts in
            ctx.accepts <- offset + 1;
            ask_locked ctx `Accept ~socket ~side:`Server ~offset ~length:0)
      in
      match fault with
      | Some Fault.Reset ->
          (* The client left first: the accept succeeds and the connection
             is reset at both ends. *)
          break conn ~at:`Server `Reset;
          (flow, peer)
      | Some Eof ->
          break conn ~at:`Server (`Closed `Client);
          (flow, peer)
      | Some Emfile ->
          locked ctx (fun () ->
              ctx.exhausted <- true;
              t.held <- Some (flow, peer));
          raise (Posix.emfile ())
      | Some Hang -> Eio.Fiber.await_cancel ()
      | Some (Stall d) ->
          Eio.Time.sleep ctx.clock d;
          (flow, peer)
      | Some
          ( Short _ | Refuse | Timeout | Unreachable | Drop | Dup | Reorder
          | Garble _ )
      | None ->
          (flow, peer)

    let accept t ~sw = guard t.ctx (fun () -> accept_ t ~sw)

    let close t =
      Eio.Resource.close t.l;
      free t.ctx

    let listening_addr t = Eio.Net.listening_addr t.l
    let setsockopt t opt v = Eio.Net.setsockopt t.l opt v
    let getsockopt t opt = Eio.Net.getsockopt t.l opt
  end

  let handler = Eio.Net.Pi.listening_socket (module Impl)
end

type (_, _, _) Eio.Resource.pi +=
  | Outage : ('t, 't -> bool, [> `Generic ]) Eio.Resource.pi
  | Livelocked : ('t, 't -> bool, [> `Generic ]) Eio.Resource.pi

module Network = struct
  type t = { ctx : ctx; net : [ `Generic ] Eio.Net.ty Eio.Resource.t }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let connect_ t ~bind_to ~options ~sw addr =
      let ctx = t.ctx in
      let socket, fault =
        locked ctx (fun () ->
            let offset = ctx.connects in
            ctx.connects <- offset + 1;
            let socket = next_socket ctx in
            ( socket,
              ask_locked ctx `Connect ~socket ~side:`Client ~offset ~length:0 ))
      in
      let go () =
        let sock =
          (Eio.Net.connect ?bind_to ~options ~sw t.net addr :> stream)
        in
        let conn = client_conn ctx ~socket sock in
        Flow.v ctx ~socket ~side:`Client ~conn sock
      in
      match fault with
      | Some (Fault.Refuse | Reset) -> raise (refused_exn ())
      | Some Timeout ->
          Eio.Time.sleep ctx.clock 133.;
          raise (Posix.timeout ())
      | Some Unreachable -> raise (Posix.unreachable ())
      | Some Hang -> Eio.Fiber.await_cancel ()
      | Some (Stall d) ->
          Eio.Time.sleep ctx.clock d;
          go ()
      | Some (Short _ | Eof | Emfile | Drop | Dup | Reorder | Garble _) | None
        ->
          go ()

    let connect t ~bind_to ~options ~sw addr =
      guard t.ctx (fun () -> connect_ t ~bind_to ~options ~sw addr)

    let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
      let l = Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.net addr in
      Eio.Resource.T
        ( {
            Listener.ctx = t.ctx;
            l :> [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
            held = None;
          },
          Listener.handler )

    let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
      Datagram.v t.ctx
        (Eio.Net.datagram_socket ~reuse_addr ~reuse_port ~sw t.net addr
          :> Datagram.socket)

    let getaddrinfo t ~service node = Eio.Net.getaddrinfo ~service t.net node
    let getnameinfo t addr = Eio.Net.getnameinfo t.net addr
  end

  let handler =
    Eio.Resource.handler
      (H (Outage, fun t -> t.ctx.outage)
      :: H (Livelocked, fun t -> Atomic.get t.ctx.livelocked)
      :: Eio.Resource.bindings (Eio.Net.Pi.network (module Impl)))
end

let v ~clock ~outage f (net : _ Eio.Net.t) :
    [ `Generic ] Eio.Net.ty Eio.Resource.t =
  Eio.Resource.T
    ( {
        Network.ctx = ctx ~clock ~outage f;
        net :> [ `Generic ] Eio.Net.ty Eio.Resource.t;
      },
      Network.handler )

let pair ~clock f =
  let ctx = ctx ~clock ~outage:false f in
  let c, s = Eio_net_mem.pipe () in
  let socket = locked ctx (fun () -> next_socket ctx) in
  let conn = conn () in
  ( Flow.v ctx ~socket ~side:`Client ~conn (c :> stream),
    Flow.v ctx ~socket ~side:`Server ~conn (s :> stream) )

let outage (net : [ `Generic ] Eio.Net.ty Eio.Resource.t) =
  let (Eio.Resource.T (t, ops)) = net in
  match Eio.Resource.get_opt ops Outage with Some f -> f t | None -> false

let livelocked (net : [ `Generic ] Eio.Net.ty Eio.Resource.t) =
  let (Eio.Resource.T (t, ops)) = net in
  match Eio.Resource.get_opt ops Livelocked with Some f -> f t | None -> false
