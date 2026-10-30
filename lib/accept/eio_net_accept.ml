type answer = [ `Aborted | `Descriptors ]

let portable = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> Some `Aborted
  | _ -> None

let registered = ref None

let classify exn =
  match !registered with Some (_, f) -> f exn | None -> portable exn

let register ~name f =
  match !registered with
  | Some (held, _) ->
      Fmt.invalid_arg "Eio_net_accept.register: %s is registered, %s refused"
        held name
  | None -> registered := Some (name, f)

(* The head of a held connection. [received] reads the bytes of an incomplete
   head received so far and is [None] once the head is complete. A connection
   may run on another domain than its loop's (an executor pool), so what it
   sets is atomic, and shedding shuts its socket down, which ends its reads on
   any domain, rather than cancelling its fiber. *)
type head = {
  received : (unit -> int) option Atomic.t;
  stopping : bool Atomic.t;
  shut : unit -> unit;
}

let reading head received =
  Atomic.set head.received (Some received);
  if Atomic.get head.stopping then head.shut ()

let complete head = Atomic.set head.received None

type held = { seq : int; head : head; mutable shed : bool }

(* The loop's state: the connections it holds, by accept order, the condition
   a connection's end signals, and whether [stop] has resolved. *)
type loop = {
  held : (int, held) Hashtbl.t;
  ended : Eio.Condition.t;
  stopping : bool Atomic.t;
  mutable next : int;
}

let shed c =
  c.shed <- true;
  c.head.shut ()

(* The held connection whose head is incomplete and has made the least
   progress, the older of two that made the same. *)
let least_progressed loop =
  Hashtbl.fold
    (fun _ c best ->
      match Atomic.get c.head.received with
      | None -> best
      | Some _ when c.shed -> best
      | Some received -> (
          let n = received () in
          match best with
          | Some (b, m) when m < n || (m = n && b.seq < c.seq) -> best
          | _ -> Some (c, n)))
    loop.held None

(* Shed the least progressed incomplete head, if one is held, and wait for a
   held connection to end. *)
let free_a_slot loop =
  Option.iter (fun (c, _) -> shed c) (least_progressed loop);
  Eio.Condition.await_no_mutex loop.ended

(* Serve [flow] on a fiber of [sw], held in [loop] until it ends. *)
let fork loop ~sw ~on_error handler flow addr =
  let seq = loop.next in
  loop.next <- seq + 1;
  let shut () = try Eio.Flow.shutdown flow `All with Eio.Io _ -> () in
  let head =
    {
      received = Atomic.make (Some (fun () -> 0));
      stopping = loop.stopping;
      shut;
    }
  in
  let c = { seq; head; shed = false } in
  let ended () =
    Eio.Flow.close flow;
    Hashtbl.remove loop.held seq;
    Eio.Condition.broadcast loop.ended
  in
  let started = ref false in
  Hashtbl.replace loop.held seq c;
  Fun.protect ~finally:(fun () -> if not !started then ended ()) @@ fun () ->
  Eio.Fiber.fork ~sw (fun () ->
      started := true;
      Fun.protect ~finally:ended @@ fun () ->
      match handler head flow addr with
      | () -> ()
      | exception ((Eio.Io _ | End_of_file) as e) ->
          if not c.shed then
            on_error
              (Eio.Exn.add_context e "handling connection from %a"
                 Eio.Net.Sockaddr.pp addr))

let run ?stop ~max_connections ~on_error listening handler =
  if max_connections < 1 then
    Fmt.invalid_arg "Eio_net_accept.run: max_connections is %d" max_connections;
  let loop =
    {
      held = Hashtbl.create 16;
      ended = Eio.Condition.create ();
      stopping = Atomic.make false;
      next = 0;
    }
  in
  (* The connections are forked on a switch of the loop's own, so they end
     with it. *)
  Eio.Switch.run ~name:"Eio_net_accept.run" @@ fun sw ->
  let rec accept () =
    match Eio.Net.accept ~sw listening with
    | flow, addr ->
        while Hashtbl.length loop.held >= max_connections do
          free_a_slot loop
        done;
        fork loop ~sw ~on_error handler flow addr;
        accept ()
    | exception (Eio.Io _ as e) -> (
        let bt = Printexc.get_raw_backtrace () in
        match classify e with
        | Some `Aborted -> accept ()
        | Some `Descriptors when Hashtbl.length loop.held > 0 ->
            free_a_slot loop;
            accept ()
        | Some `Descriptors ->
            Eio.Exn.reraise_with_context e bt
              "accepting, with no connection held to shed or wait for"
        | None -> Printexc.raise_with_backtrace e bt)
  in
  match stop with
  | None -> accept ()
  | Some stop ->
      let v = Eio.Fiber.first accept (fun () -> Eio.Promise.await stop) in
      Atomic.set loop.stopping true;
      Hashtbl.fold (fun _ c acc -> c :: acc) loop.held []
      |> List.iter (fun c ->
          if Option.is_some (Atomic.get c.head.received) then shed c);
      v
