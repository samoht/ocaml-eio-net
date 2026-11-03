open! Hermetic

let log_src = Logs.Src.create "eio-net.accept"

module Log = (val Logs.src_log log_src)

type answer = [ `Aborted | `Descriptors ]

let classify = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> Some `Aborted
  | _ -> None

(* The head of a held connection. [received] reads the bytes of an incomplete
   head received so far and is [None] once the head is complete. A connection
   may run on another domain than its loop's (an executor pool), so what it
   sets is atomic, and shedding shuts its socket down, which ends its reads on
   any domain, rather than cancelling its fiber. [shed] marks a connection
   shed, so the failure its shut socket causes is not reported. *)
type head = {
  received : (unit -> int) option Atomic.t;
  stopping : bool Atomic.t;
  shed : bool Atomic.t;
  shut : unit -> unit;
}

let shed head =
  Atomic.set head.shed true;
  head.shut ()

let reading head received =
  Atomic.set head.received (Some received);
  if Atomic.get head.stopping then shed head

let complete head = Atomic.set head.received None

type held = { seq : int; loop : int; head : head }

module Seq_map = Map.Make (Int)

(* The connections every loop of a pool holds, by accept order. Its loops
   share their descriptors, so their pressure on any loop sheds from all of
   them. Loops may run on different domains, so the map is replaced by
   compare-and-set and read as a snapshot; [ends] counts the connections ended,
   so a loop waiting for one misses none that ends before it waits. *)
type pool = {
  all : held Seq_map.t Atomic.t;
  next : int Atomic.t;
  ends : int Atomic.t;
  any_ended : Eio.Condition.t;
}

let pool () =
  {
    all = Atomic.make Seq_map.empty;
    next = Atomic.make 0;
    ends = Atomic.make 0;
    any_ended = Eio.Condition.create ();
  }

let rec update pool f =
  let all = Atomic.get pool.all in
  if not (Atomic.compare_and_set pool.all all (f all)) then update pool f

(* A loop's state: its pool and its identity there, how many connections it
   holds, which only its own fibers change, the condition one of their ends
   signals, and whether [stop] has resolved. *)
type loop = {
  pool : pool;
  id : int;
  mutable count : int;
  ended : Eio.Condition.t;
  stopping : bool Atomic.t;
}

(* The connection held by a loop that [mine] accepts whose head is incomplete
   and has made the least progress, the older of two that made the same. *)
let least_progressed pool mine =
  Seq_map.fold
    (fun _ c best ->
      match Atomic.get c.head.received with
      | None -> best
      | Some _ when (not (mine c)) || Atomic.get c.head.shed -> best
      | Some received -> (
          let n = received () in
          match best with
          | Some (b, m) when m < n || (m = n && b.seq < c.seq) -> best
          | _ -> Some (c, n)))
    (Atomic.get pool.all) None

(* Every connection of [loop] is held: shed its least progressed incomplete
   head, if it holds one, and wait for one of its connections to end. *)
let free_a_slot loop =
  Option.iter
    (fun (c, _) -> shed c.head)
    (least_progressed loop.pool (fun c -> c.loop = loop.id));
  Eio.Condition.await_no_mutex loop.ended

(* No descriptor is left: shed the least progressed incomplete head of
   [pool], if one is held, and wait for a connection of any of its loops to
   end. [false] when none holds one. *)
let free_a_descriptor pool =
  let ends = Atomic.get pool.ends in
  if Seq_map.is_empty (Atomic.get pool.all) then false
  else begin
    Option.iter
      (fun (c, _) -> shed c.head)
      (least_progressed pool (fun _ -> true));
    Eio.Condition.loop_no_mutex pool.any_ended (fun () ->
        if Atomic.get pool.ends = ends then None else Some ());
    true
  end

(* Serve [flow] on a fiber of [sw], held in [loop] and its pool until it
   ends. *)
let fork loop ~sw ~on_error handler flow addr =
  let pool = loop.pool in
  let seq = Atomic.fetch_and_add pool.next 1 in
  let shut () = try Eio.Flow.shutdown flow `All with Eio.Io _ -> () in
  let head =
    {
      received = Atomic.make (Some (fun () -> 0));
      stopping = loop.stopping;
      shed = Atomic.make false;
      shut;
    }
  in
  let c = { seq; loop = loop.id; head } in
  let ended () =
    Eio.Flow.close flow;
    update pool (Seq_map.remove seq);
    loop.count <- loop.count - 1;
    Atomic.incr pool.ends;
    Eio.Condition.broadcast loop.ended;
    Eio.Condition.broadcast pool.any_ended
  in
  let started = ref false in
  update pool (Seq_map.add seq c);
  loop.count <- loop.count + 1;
  Fun.protect ~finally:(fun () -> if not !started then ended ()) @@ fun () ->
  Eio.Fiber.fork ~sw (fun () ->
      started := true;
      Fun.protect ~finally:ended @@ fun () ->
      match handler head flow addr with
      | () -> ()
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e ->
          if not (Atomic.get head.shed) then
            on_error
              (Eio.Exn.add_context e "handling connection from %a"
                 Eio.Net.Sockaddr.pp addr))

let log_error exn = Log.warn (fun m -> m "connection failed: %a" Fmt.exn exn)

let run ?stop ?(on_error = log_error) ?(pool = pool ()) ?(classify = classify)
    ~max_connections listening handler =
  if max_connections < 1 then
    Fmt.invalid_arg "Eio_net_accept.run: max_connections is %d" max_connections;
  let loop =
    {
      pool;
      id = Atomic.fetch_and_add pool.next 1;
      count = 0;
      ended = Eio.Condition.create ();
      stopping = Atomic.make false;
    }
  in
  (* The connections are forked on a switch of the loop's own, so they end
     with it. *)
  Eio.Switch.run ~name:"Eio_net_accept.run" @@ fun sw ->
  let rec accept () =
    match Eio.Net.accept ~sw listening with
    | flow, addr ->
        while loop.count >= max_connections do
          free_a_slot loop
        done;
        fork loop ~sw ~on_error handler flow addr;
        accept ()
    | exception (Eio.Io _ as e) -> (
        let bt = Printexc.get_raw_backtrace () in
        match classify e with
        | Some `Aborted -> accept ()
        | Some `Descriptors ->
            if free_a_descriptor pool then accept ()
            else
              Eio.Exn.reraise_with_context e bt
                "accepting, with no connection held by any loop of its pool to \
                 shed or wait for"
        | None -> Printexc.raise_with_backtrace e bt)
  in
  match stop with
  | None -> accept ()
  | Some stop ->
      let v = Eio.Fiber.first accept (fun () -> Eio.Promise.await stop) in
      Atomic.set loop.stopping true;
      Seq_map.iter
        (fun _ c ->
          if c.loop = loop.id && Option.is_some (Atomic.get c.head.received)
          then shed c.head)
        (Atomic.get pool.all);
      v
