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

(* A writer that stopped, while its reader's read hangs: [thaw] resolves
   [thawed], which its writes wait for. *)
type freeze = { thawed : unit Eio.Promise.t; thaw : unit Eio.Promise.u }

(* A connection, shared by its two ends. A reset breaks both, as on TCP. An
   end of stream is the close of one end, [`Closed closer]: its peer reads
   what it wrote before, then end of file. [broken] changes once, under
   [lock], which also guards [ends], [sent], [writing] and [frozen]: an end
   added after the break is shut at once. [paired] holds when both ends are
   wrapped here. [sent], [writing] and [frozen] are indexed by the writing
   side: the bytes whose write returned, the writes under way, and the freeze
   of a stopped writer. *)
type conn = {
  lock : Mutex.t;
  family : Op.family;
  paired : bool;
  broken : [ `Open | `Reset | `Closed of Op.side ] Atomic.t;
  mutable ends : (Op.side * stream) list;
  sent : int array;
  writing : int array;
  frozen : freeze option array;
}

let conn ~paired family =
  {
    lock = Mutex.create ();
    family;
    paired;
    broken = Atomic.make `Open;
    ends = [];
    sent = [| 0; 0 |];
    writing = [| 0; 0 |];
    frozen = [| None; None |];
  }

let ix : Op.side -> int = function `Client -> 0 | `Server -> 1

(* The writer [side] goes on. *)
let thaw conn side =
  let f =
    Mutex.protect conn.lock (fun () ->
        let f = conn.frozen.(ix side) in
        conn.frozen.(ix side) <- None;
        f)
  in
  Option.iter (fun f -> Eio.Promise.resolve f.thaw ()) f

let thaw_all conn =
  thaw conn `Client;
  thaw conn `Server

let other : Op.side -> Op.side = function
  | `Client -> `Server
  | `Server -> `Client

let shutdown s cmd =
  try Eio.Flow.shutdown s cmd with Eio.Io _ | Invalid_argument _ -> ()

(* What the socket of the end [side] does once [conn] broke: on a close, the
   closer's sends end, so its peer reads what it wrote, then end of file. A
   Unix-domain closer's receives end too: it reads what is queued, then end
   of file. *)
let shut conn broken (side, s) =
  match broken with
  | `Reset -> shutdown s `All
  | `Closed closer when side = closer ->
      shutdown s (match conn.family with `Inet -> `Send | `Unix -> `All)
  | `Closed _ | `Open -> ()

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
  thaw_all conn;
  List.iter (shut conn how) ends;
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
  shut conn broken (side, sock)

(* The client registers its socket number once its connect returns; an accept
   that sees the connection first waits for it. *)
type pending = { number : int Eio.Promise.t; set : int Eio.Promise.u; c : conn }

(* The end of a connection, for the livelock guard: the connections a
   client made to one address count as one, so a client that reconnects at
   once after each failure retries one target. *)
type peer = Remote of Eio.Net.Sockaddr.stream | End of int * Op.side

(* What an operation is tried on, for the livelock guard. *)
type target =
  | Connect of Eio.Net.Sockaddr.stream
  | Accept of int
  | Read of peer
  | Write of peer
  | Recv of int

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
  mutable listeners : int;
  streaks : (target, int) Hashtbl.t;
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
    listeners = 0;
    streaks = Hashtbl.create 7;
    streak_at = 0.;
    livelocked = Atomic.make false;
    pending = Hashtbl.create 7;
  }

(* Under [ctx.lock]. *)
let ask_locked ctx kind ~socket ~side ~family ~offset ~length =
  ctx.index <- ctx.index + 1;
  ctx.f { Op.index = ctx.index; kind; socket; side; family; offset; length }

let locked (ctx : ctx) f = Mutex.protect ctx.lock f

(* A program that retries a failed operation at once, with no time passing,
   spins for ever: an outage refuses each connect at once. After [threshold]
   failures of one operation on one target while the clock stands still, with
   no success there in between, the operation raises [Failure] instead, and
   after twice as many it waits for ever, as nothing the program does can get
   it out. Failures on distinct targets, as a scan of many ports makes, do not
   add up. *)
let threshold = 10_000

let livelock_message =
  Fmt.str
    "the same network operation failed %d times on the same target while its \
     clock stood still"
    threshold

let failed ctx target =
  let n =
    locked ctx (fun () ->
        let now = Eio.Time.now ctx.clock in
        if now <> ctx.streak_at then begin
          ctx.streak_at <- now;
          Hashtbl.reset ctx.streaks
        end;
        let n =
          1 + Option.value ~default:0 (Hashtbl.find_opt ctx.streaks target)
        in
        Hashtbl.replace ctx.streaks target n;
        n)
  in
  if n >= threshold then begin
    Atomic.set ctx.livelocked true;
    if n >= 2 * threshold then
      Eio.Cancel.protect (fun () ->
          Eio.Promise.await (fst (Eio.Promise.create ())));
    failwith ("Eio_net_chaos: " ^ livelock_message)
  end

let guard ctx target f =
  match f () with
  | v ->
      locked ctx (fun () ->
          if Hashtbl.length ctx.streaks > 0 then
            Hashtbl.remove ctx.streaks target);
      v
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      failed ctx target;
      Printexc.raise_with_backtrace e bt

(* A close frees a descriptor, so the accepts that [Fault.Emfile] fails go
   through again. *)
let free ctx = locked ctx (fun () -> ctx.exhausted <- false)

let ask ctx kind ~socket ~side ~family ~offset ~length =
  locked ctx (fun () ->
      ask_locked ctx kind ~socket ~side ~family ~offset ~length)

let next_socket ctx =
  ctx.sockets <- ctx.sockets + 1;
  ctx.sockets

let pending ctx ~family id =
  match Hashtbl.find_opt ctx.pending id with
  | Some p ->
      Hashtbl.remove ctx.pending id;
      p
  | None ->
      let number, set = Eio.Promise.create () in
      let p = { number; set; c = conn ~paired:true family } in
      Hashtbl.replace ctx.pending id p;
      p

(* The connection a connect made, numbered [socket]. *)
let client_conn ctx ~socket ~family sock =
  match Eio_net_mem.connection sock with
  | None -> conn ~paired:false family
  | Some id ->
      let p = locked ctx (fun () -> pending ctx ~family id) in
      Eio.Promise.resolve p.set socket;
      p.c

(* The number and the connection of an accepted socket. *)
let server_conn ctx ~family sock =
  match Eio_net_mem.connection sock with
  | None -> (locked ctx (fun () -> next_socket ctx), conn ~paired:false family)
  | Some id ->
      let p = locked ctx (fun () -> pending ctx ~family id) in
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
    family : Op.family;
    peer : peer;
    sock : stream;
    conn : conn;
    lock : Mutex.t;
    mutable leftover : Cstruct.t;
    mutable delivered : int;
    mutable written : int;
    mutable closed : bool;
    mutable reset_reported : bool;
    mutable wrote_after_end : bool;
    mutable cut_short : bool;
    mutable stop : bool;
  }

  (* Under [t.lock]. The end of a broken connection's reads: a reset is
     reported once, by the first read or write that meets it, and later reads
     return end of file, as a socket clears its pending error. *)
  let ended t =
    t.leftover <- Cstruct.empty;
    match Atomic.get t.conn.broken with
    | `Reset when not t.reset_reported ->
        t.reset_reported <- true;
        raise (reset_exn ())
    | `Reset | `Closed _ | `Open -> raise End_of_file

  (* Whether this end's own reads are cut: a reset, or its own close. *)
  let cut t =
    match Atomic.get t.conn.broken with
    | `Reset -> true
    | `Closed closer -> closer = t.side
    | `Open -> false

  (* Under [t.lock]. Over IP, a cut drops what is buffered at once. An
     AF_UNIX byte whose write returned is in the reader's queue, so the reader
     reads it first, then meets the cut (unix(7)). The peer of an end that
     closed reads on, to the end its socket now reaches. *)
  let check_read t =
    if t.closed then invalid_arg "Eio_net_chaos: read on a closed flow";
    if t.family = `Inet && cut t then ended t

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
    if got = 0 then ended t;
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

  (* On AF_UNIX a hang is a peer that stops. A returned write is in the
     reader's queue (unix(7)), so while bytes are queued or on their way the
     reads take them and [t.stop] holds. Once none are, the read waits, and
     the peer's writes wait with it, until the read is cancelled or the
     connection closes, shuts down or breaks. Over IP, or with a peer not
     wrapped here, a hang holds what is queued. *)
  let stops t = t.family = `Unix && t.conn.paired

  let stopped t dst =
    let peer = ix (other t.side) in
    let next =
      Mutex.protect t.lock @@ fun () ->
      check_read t;
      let c = t.conn in
      Mutex.protect c.lock @@ fun () ->
      if Atomic.get c.broken <> `Open then begin
        t.stop <- false;
        `Read
      end
      else if
        Cstruct.length t.leftover > 0
        || c.sent.(peer) > t.delivered
        || c.writing.(peer) > 0
      then `Read
      else begin
        t.stop <- false;
        let thawed, thaw = Eio.Promise.create () in
        c.frozen.(peer) <- Some { thawed; thaw };
        `Wait thawed
      end
    in
    match next with
    | `Read -> deliver t dst ~cap:0
    | `Wait thawed ->
        (match Eio.Promise.await thawed with
        | () -> ()
        | exception (Eio.Cancel.Cancelled _ as e) ->
            thaw t.conn (other t.side);
            raise e);
        deliver t dst ~cap:0

  (* The writes of a peer that stopped wait. [writing] counts this one until
     [leave]. *)
  let rec enter t =
    if stops t then
      let c = t.conn in
      let frozen =
        Mutex.protect c.lock (fun () ->
            match c.frozen.(ix t.side) with
            | Some f -> Some f.thawed
            | None ->
                c.writing.(ix t.side) <- c.writing.(ix t.side) + 1;
                None)
      in
      match frozen with
      | Some thawed ->
          Eio.Promise.await thawed;
          enter t
      | None -> ()

  let leave t ~sent =
    if stops t then
      let c = t.conn in
      Mutex.protect c.lock (fun () ->
          c.writing.(ix t.side) <- c.writing.(ix t.side) - 1;
          c.sent.(ix t.side) <- c.sent.(ix t.side) + sent)

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let single_read_ t dst =
      let fault, stop =
        Mutex.protect t.lock (fun () ->
            check_read t;
            if Cstruct.length dst = 0 then (None, false)
            else
              let fault =
                ask t.ctx `Read ~socket:t.socket ~side:t.side ~family:t.family
                  ~offset:t.delivered ~length:(Cstruct.length dst)
              in
              if fault = Some Fault.Hang && stops t then t.stop <- true;
              (fault, t.stop))
      in
      if Cstruct.length dst = 0 then 0
      else
        match fault with
        | (Some Hang | None) when stop -> stopped t dst
        | Some (Fault.Short n) -> deliver t dst ~cap:(max 1 n)
        | Some (Stall d) ->
            Eio.Time.sleep t.ctx.clock d;
            deliver t dst ~cap:0
        | Some Hang -> Eio.Fiber.await_cancel ()
        | Some Reset ->
            break t.conn ~at:t.side `Reset;
            if t.family = `Inet then raise_broken t else deliver t dst ~cap:0
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
    let write_ t bufs ~sent =
      (* A write cut short found the socket's buffer full: on eio_posix the
         next one waits until it has room, and other fibers run meanwhile. *)
      let waits =
        Mutex.protect t.lock (fun () ->
            let waits = t.cut_short in
            t.cut_short <- false;
            waits)
      in
      if waits then Eio.Fiber.yield ();
      let total = Cstruct.lenv bufs in
      let fault, n =
        Mutex.protect t.lock @@ fun () ->
        match check_write t with
        | `Lost -> (None, -1)
        | `Write when total = 0 -> (None, 0)
        | `Write ->
            let fault =
              ask t.ctx `Write ~socket:t.socket ~side:t.side ~family:t.family
                ~offset:t.written ~length:total
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
            t.cut_short <- n < total;
            (fault, n)
      in
      let write () =
        match Eio.Flow.write t.sock (takev n bufs) with
        | () ->
            sent := n;
            n
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

    let single_write_ t bufs =
      enter t;
      let sent = ref 0 in
      Fun.protect
        ~finally:(fun () -> leave t ~sent:!sent)
        (fun () -> write_ t bufs ~sent)

    let single_read t dst =
      guard t.ctx (Read t.peer) (fun () -> single_read_ t dst)

    let single_write t bufs =
      guard t.ctx (Write t.peer) (fun () -> single_write_ t bufs)

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
    let read_methods = []

    let shutdown t cmd =
      (match cmd with
      | `Send -> thaw t.conn t.side
      | `Receive -> thaw t.conn (other t.side)
      | `All -> thaw_all t.conn);
      try Eio.Flow.shutdown t.sock cmd with Eio.Io _ when broken t -> ()

    let close t =
      let was_open =
        Mutex.protect t.lock (fun () ->
            let was_open = not t.closed in
            t.leftover <- Cstruct.empty;
            t.closed <- true;
            was_open)
      in
      thaw_all t.conn;
      Eio.Resource.close t.sock;
      if was_open then free t.ctx

    let setsockopt t opt v = Eio.Net.setsockopt t.sock opt v
    let getsockopt t opt = Eio.Net.getsockopt t.sock opt
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  let v ?remote ctx ~socket ~side ~family ~conn sock : stream =
    join conn side sock;
    Eio.Resource.T
      ( {
          ctx;
          socket;
          side;
          family;
          peer =
            (match remote with
            | Some addr -> Remote addr
            | None -> End (socket, side));
          sock;
          conn;
          lock = Mutex.create ();
          leftover = Cstruct.empty;
          delivered = 0;
          written = 0;
          closed = false;
          reset_reported = false;
          wrote_after_end = false;
          cut_short = false;
          stop = false;
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
    family : Op.family;
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
       while it waits for one draws none. If [f] raises, the datagram goes
       back for the next receive. *)
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
                  match
                    ask t.ctx `Recv ~socket:t.socket ~side:`Client
                      ~family:t.family ~offset ~length:(Cstruct.length buf)
                  with
                  | fault -> fault
                  | exception e ->
                      t.ready <- d :: t.ready;
                      raise e
                in
                (* A Unix-domain socket loses, duplicates, reorders and
                   garbles nothing (unix(7)). *)
                let fault =
                  match (t.family, fault) with
                  | `Unix, Some (Fault.Drop | Dup | Reorder | Garble _) -> None
                  | _ -> fault
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

    let recv t buf = guard t.ctx (Recv t.socket) (fun () -> recv_ t buf)
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

  let v ctx ~family (sock : socket) : socket =
    let socket = locked ctx (fun () -> next_socket ctx) in
    Eio.Resource.T
      ( {
          ctx;
          socket;
          family;
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
  (* [held] holds the connections an accept took and did not return, oldest
     first: one failed with EMFILE, which stays queued until a descriptor is
     freed, or one whose accept [f] raised in or hung until a cancel. The next
     accepts return them, as the kernel's queue would. *)
  type t = {
    ctx : ctx;
    l : [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
    mutable held : (stream * Eio.Net.Sockaddr.stream) list;
    number : int;
    family : Op.family;
  }

  let hold t c = locked t.ctx (fun () -> t.held <- c :: t.held)

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
              | c :: rest ->
                  t.held <- rest;
                  `Held c
              | [] -> `Accept)
      in
      match queued with
      | `Exhausted -> raise (Posix.emfile ())
      | `Held c -> c
      | `Accept -> accept_base t ~sw

    and accept_base t ~sw =
      let ctx = t.ctx in
      let sock, peer = Eio.Net.accept ~sw t.l in
      let sock = (sock :> stream) in
      let family = t.family in
      let socket, conn = server_conn ctx ~family sock in
      let flow = Flow.v ctx ~socket ~side:`Server ~family ~conn sock in
      let fault =
        match
          locked ctx (fun () ->
              let offset = ctx.accepts in
              ctx.accepts <- offset + 1;
              ask_locked ctx `Accept ~socket ~side:`Server ~family ~offset
                ~length:0)
        with
        | fault -> fault
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            hold t (flow, peer);
            Printexc.raise_with_backtrace e bt
      in
      match fault with
      | Some Fault.Reset when family = `Inet ->
          (* The client left first: the accept succeeds and the connection
             is reset at both ends. *)
          break conn ~at:`Server `Reset;
          (flow, peer)
      | Some Reset ->
          (* A Unix-domain client that left first closed: no reset. *)
          break conn ~at:`Server (`Closed `Client);
          (flow, peer)
      | Some Eof ->
          break conn ~at:`Server (`Closed `Client);
          (flow, peer)
      | Some Emfile ->
          locked ctx (fun () ->
              ctx.exhausted <- true;
              t.held <- (flow, peer) :: t.held);
          raise (Posix.emfile ())
      | Some Hang ->
          hold t (flow, peer);
          Eio.Fiber.await_cancel ()
      | Some (Stall d) ->
          Eio.Time.sleep ctx.clock d;
          (flow, peer)
      | Some
          ( Short _ | Refuse | Timeout | Unreachable | Drop | Dup | Reorder
          | Garble _ )
      | None ->
          (flow, peer)

    let accept t ~sw = guard t.ctx (Accept t.number) (fun () -> accept_ t ~sw)

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
      let family = Op.family_of_stream addr in
      let socket, fault =
        locked ctx (fun () ->
            let offset = ctx.connects in
            ctx.connects <- offset + 1;
            let socket = next_socket ctx in
            ( socket,
              ask_locked ctx `Connect ~socket ~side:`Client ~family ~offset
                ~length:0 ))
      in
      let go () =
        let sock =
          (Eio.Net.connect ?bind_to ~options ~sw t.net addr :> stream)
        in
        let conn = client_conn ctx ~socket ~family sock in
        Flow.v ~remote:addr ctx ~socket ~side:`Client ~family ~conn sock
      in
      match fault with
      | Some (Fault.Refuse | Reset) -> raise (refused_exn ())
      | Some (Timeout | Unreachable) when family = `Unix ->
          (* A Unix-domain connect has no handshake to lose and no route to
             miss. *)
          go ()
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
      guard t.ctx (Connect addr) (fun () ->
          connect_ t ~bind_to ~options ~sw addr)

    let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
      let l = Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.net addr in
      Eio.Resource.T
        ( {
            Listener.ctx = t.ctx;
            family = Op.family_of_stream addr;
            l :> [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
            held = [];
            number =
              locked t.ctx (fun () ->
                  t.ctx.listeners <- t.ctx.listeners + 1;
                  t.ctx.listeners);
          },
          Listener.handler )

    let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
      Datagram.v t.ctx
        ~family:(Op.family_of_datagram addr)
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
  let conn = conn ~paired:true `Inet in
  ( Flow.v ctx ~socket ~side:`Client ~family:`Inet ~conn (c :> stream),
    Flow.v ctx ~socket ~side:`Server ~family:`Inet ~conn (s :> stream) )

let outage (net : [ `Generic ] Eio.Net.ty Eio.Resource.t) =
  let (Eio.Resource.T (t, ops)) = net in
  match Eio.Resource.get_opt ops Outage with Some f -> f t | None -> false

let livelocked (net : [ `Generic ] Eio.Net.ty Eio.Resource.t) =
  let (Eio.Resource.T (t, ops)) = net in
  match Eio.Resource.get_opt ops Livelocked with Some f -> f t | None -> false
