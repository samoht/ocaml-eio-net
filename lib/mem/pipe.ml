open! Hermetic
open Kernel
open Code

(* The clock a network was given: TCP's timers run on it. *)
type timer = { now : unit -> Mtime.t; sleep_until : Mtime.t -> unit }

let timer clock =
  {
    now = (fun () -> Eio.Time.Mono.now clock);
    sleep_until = (fun t -> Eio.Time.Mono.sleep_until clock t);
  }

(* Fibers waiting on a pipe, first come first served, in a doubly-linked
   list as {!Eio.Condition} keeps them, so a cancelled waiter leaves at once.
   A waiter is taken from the list under the pipe's mutex, at most once, and
   woken once the mutex is released. The first waiter is woken with the turn:
   until it has gone again, a newcomer queues behind the waiters rather than
   take the room or bytes that woke it, and a woken waiter that must wait again
   goes back to the head. A waiter cancelled after it was taken passes the turn
   on, so no wake-up is lost. *)
module Waiters = struct
  type node = {
    wake : unit Eio.Promise.u;
    mutable prev : node option;
    mutable next : node option;
    mutable waiting : bool;
  }

  type t = {
    mutable first : node option;
    mutable last : node option;
    mutable turn : bool;
  }

  let v () = { first = None; last = None; turn = false }

  let add q =
    let p, wake = Eio.Promise.create () in
    let n = { wake; prev = q.last; next = None; waiting = true } in
    (match q.last with None -> q.first <- Some n | Some l -> l.next <- Some n);
    q.last <- Some n;
    (p, n)

  let add_first q =
    let p, wake = Eio.Promise.create () in
    let n = { wake; prev = None; next = q.first; waiting = true } in
    (match q.first with None -> q.last <- Some n | Some f -> f.prev <- Some n);
    q.first <- Some n;
    (p, n)

  let unlink q n =
    (match n.prev with None -> q.first <- n.next | Some p -> p.next <- n.next);
    (match n.next with None -> q.last <- n.prev | Some x -> x.prev <- n.prev);
    n.prev <- None;
    n.next <- None;
    n.waiting <- false

  let take q =
    match q.first with
    | None -> []
    | Some n ->
        unlink q n;
        [ n ]

  let rec take_all q =
    q.turn <- false;
    match q.first with
    | None -> []
    | Some n ->
        unlink q n;
        n :: take_all q

  (* A caller that does not hold the turn waits while others wait or hold
     it. *)
  let must_wait q ~turn = (not turn) && (Option.is_some q.first || q.turn)

  (* Wakes the first waiter with the turn, unless a woken one holds it. *)
  let pass q =
    if q.turn then []
    else
      match take q with
      | [] -> []
      | woken ->
          q.turn <- true;
          woken

  (* A caller that held the turn and leaves without waiting again passes it
     on. *)
  let release q ~turn =
    if turn then begin
      q.turn <- false;
      pass q
    end
    else []

  (* Queues a caller, at the head if it held the turn, which it gives up. *)
  let queue q ~turn =
    if turn then begin
      q.turn <- false;
      add_first q
    end
    else add q

  (* Removes a waiter still in the list; [false] if it was taken. *)
  let remove q n =
    n.waiting
    && begin
      unlink q n;
      true
    end

  let wake = List.iter (fun n -> Eio.Promise.resolve n.wake ())
end

(* One direction of a connection: the bytes written and not yet read. Its two
   ends may be used from two domains, so its state is guarded by a mutex. A
   read that finds nothing waits in [readers], and a write that finds the queue
   holding [capacity] bytes waits in [writers]. A write wakes one reader, and
   the next writer if room is left; a read wakes one writer, and the next
   reader if bytes are left. A change of state, a shutdown, a reset or a close,
   wakes every waiter.

   The ends follow TCP: [shut] is the writer's FIN, after which the reader
   reads end-of-file, or a reset when [reset] says one came. A reset is
   reported once, by a read or by a write that [refused] fails, as a socket
   clears its pending error; reads then return end-of-file.

   [reader] is the reader's end. [Gone] once it has closed: the first write
   to it is lost and answered with a reset, which [refused] then makes every
   later write raise; on a Unix-domain connection, [local], the first write
   raises it, as EPIPE. Once it shut its receive side it reads what is
   queued, then end-of-file, and as Linux then sends no window update, what
   it reads frees no room: [unacked] counts it. Once it has also shut its
   send side, data reaching a TCP reader resets the connection, as Linux
   resets it in FIN-WAIT; a write to a Unix-domain reader that shut its
   receive side raises the reset, as EPIPE.

   [silent] says nothing crosses any more, as on a partition: the reader
   reads the [n] bytes of [Some n], then waits until it shuts its receive
   side; the
   writer fills the queue, then waits until its own [shut]. What the reader
   reads frees no room, and neither end's close or shutdown reaches the
   other. A TCP pipe with a [timer] keeps when its writer last sent a byte
   while the connection was live, [last], when it went silent, [silenced],
   and when the oldest byte sent since then, or lost then, was written,
   [oldest]: when each end gives up follows. *)
type side = Open | Shut
type reader = Live of { receive : side; send : side } | Gone

type t = {
  mutex : Mutex.t;
  queued : Cstruct.t Queue.t;
  mutable head : int; (* Bytes of the first queued buffer already read. *)
  mutable shut : bool;
  mutable reset : bool;
  mutable refused : bool;
  mutable reader : reader;
  mutable silent : int option;
  timer : timer option;
  mutable last : Mtime.t;
  mutable silenced : Mtime.t;
  mutable oldest : Mtime.t option;
  os : os;
  local : bool;
  mutable capacity : int;
  mutable size : int; (* Bytes queued and not yet read. *)
  mutable unacked : int;
  readers : Waiters.t;
  writers : Waiters.t;
}

exception Expired

(* The reader's end changes here only: a shutdown of either side of a live
   reader, and its close ([leave]). *)
let shut_reader side t =
  match (t.reader, side) with
  | Live r, `Receive -> t.reader <- Live { r with receive = Shut }
  | Live r, `Send -> t.reader <- Live { r with send = Shut }
  | Gone, _ -> ()

let receive_shut t =
  match t.reader with
  | Live { receive = Shut; _ } -> true
  | Live _ | Gone -> false

let now t = match t.timer with Some c -> c.now () | None -> Mtime.min_stamp

let v ~os ~local ~timer ~capacity =
  let timer = if local then None else timer in
  let born = match timer with Some c -> c.now () | None -> Mtime.min_stamp in
  {
    os;
    local;
    timer;
    last = born;
    silenced = born;
    oldest = None;
    mutex = Mutex.create ();
    queued = Queue.create ();
    head = 0;
    shut = false;
    reset = false;
    refused = false;
    reader = Live { receive = Open; send = Open };
    silent = None;
    capacity;
    size = 0;
    unacked = 0;
    readers = Waiters.v ();
    writers = Waiters.v ();
  }

(* Runs [f] under the mutex; it returns its result and the waiters to wake
   once the mutex is released. *)
let locked t f =
  let result, woken = Mutex.protect t.mutex (fun () -> f t) in
  Waiters.wake woken;
  result

(* Waits for the wake-up of [waiter], registered in [q], and at most until
   [until ()], which a time already past makes raise [Expired]: [false] if
   that time came first, and the waiter left without the turn. *)
let await ?until t q (p, waiter) =
  let leave () =
    locked t (fun _ ->
        if Waiters.remove q waiter then ((), [])
        else begin
          q.turn <- false;
          ((), Waiters.pass q)
        end)
  in
  let deadline =
    match (t.timer, until) with
    | Some c, Some until -> Option.map (fun d -> (c, d)) (until ())
    | _ -> None
  in
  match deadline with
  | Some (c, d) when not (Mtime.is_later d ~than:(c.now ())) ->
      leave ();
      raise Expired
  | _ -> (
      let wait () =
        match deadline with
        | None ->
            Eio.Promise.await p;
            true
        | Some (c, d) ->
            Eio.Fiber.first
              (fun () ->
                Eio.Promise.await p;
                true)
              (fun () ->
                c.sleep_until d;
                false)
      in
      match wait () with
      | true -> true
      | false ->
          leave ();
          false
      | exception (Eio.Cancel.Cancelled _ as e) ->
          leave ();
          raise e)

(* Changes the state under the mutex and wakes every waiter. *)
let update t f =
  locked t (fun t ->
      f t;
      ((), Waiters.take_all t.readers @ Waiters.take_all t.writers))

(* A socket's buffer changed: the queue holds [capacity] bytes now. *)
let resize t capacity = update t (fun t -> t.capacity <- capacity)

(* On macOS a write that fails on a reset leaves it to the next read, as
   it raises EPIPE without clearing the socket's pending error. *)
let lasting t = t.os = Macos && not t.local

(* The reset reaches the reader once it has read what is queued. A TCP
   reader that has the writer's end-of-file keeps reading end-of-file on
   Linux, which reports the end of a stream before a reset that came after
   it; macOS reports the reset. A Unix-domain reader reads end-of-file on
   macOS. *)
let reset t =
  update t (fun t ->
      if Option.is_some t.silent then ()
      else if t.local && t.os = Macos then t.shut <- true
      else if t.local || lasting t || not t.shut then t.reset <- true)

(* The reset was reported: the stream has ended. *)
let consume_reset t =
  update t (fun t ->
      if t.reset && not (lasting t) then begin
        t.reset <- false;
        t.shut <- true
      end)

(* The bytes are copied: the caller owns its buffers again once this returns.
   A write to a reader that has gone is lost, and resets [back], the other
   direction of the connection. *)
let rec push t bufs n =
  match bufs with
  | b :: bufs when n > 0 ->
      let k = min n (Cstruct.length b) in
      if k > 0 then
        Queue.push (Cstruct.of_string (Cstruct.to_string ~len:k b)) t.queued;
      push t bufs (n - k)
  | _ -> ()

(* Under the mutex: queues what room is left of [n] bytes. *)
let queue t ~turn bufs n =
  if turn then t.writers.turn <- false;
  let k = min n (t.capacity - t.size - t.unacked) in
  push t bufs k;
  t.size <- t.size + k;
  k

(* Under the mutex: the next writer, if room is left. *)
let room t =
  if t.size + t.unacked < t.capacity then Waiters.pass t.writers else []

let rec write_turn ~turn ?until t ~back bufs =
  let n = Cstruct.lenv bufs in
  let result =
    locked t @@ fun t ->
    let leave outcome = (outcome, Waiters.release t.writers ~turn) in
    (* An empty write sends nothing, so on Linux it meets no shutdown or
       closed peer; on macOS it fails where a write would. *)
    if n = 0 && t.os = Linux then leave (`Done 0)
    else if t.shut then leave `Shut
    else if Option.is_some t.silent then
      if n = 0 then leave (`Done 0)
      else if
        Waiters.must_wait t.writers ~turn || t.size + t.unacked >= t.capacity
      then (`Wait (Waiters.queue t.writers ~turn), [])
      else
        let k = queue t ~turn bufs n in
        if k > 0 && Option.is_none t.oldest && Option.is_some t.timer then begin
          t.oldest <- Some (now t);
          (`Sent k, room t)
        end
        else (`Done k, room t)
    else if t.refused then leave `Refused
    else if n = 0 then
      if t.local && t.reader = Gone then leave (`Raise (reset_exn ()))
      else leave (`Done 0)
    else if t.reader = Gone then begin
      t.refused <- true;
      if t.local then leave (`Raise (reset_exn ())) else leave (`Lost n)
    end
    else if receive_shut t && t.local && t.os = Linux then
      leave (`Raise (reset_exn ()))
    else if
      Waiters.must_wait t.writers ~turn || t.size + t.unacked >= t.capacity
    then (`Wait (Waiters.queue t.writers ~turn), [])
    else if receive_shut t && t.os = Macos then begin
      (* macOS drops what reaches a receive side shut down, still counted
         in its buffer: a TCP one answers it with a reset. *)
      let k = min n (t.capacity - t.size - t.unacked) in
      t.unacked <- t.unacked + k;
      if t.local then leave (`Done k)
      else begin
        t.refused <- true;
        leave (`Dropped k)
      end
    end
    else if t.reader = Live { receive = Shut; send = Shut } then begin
      (* Data a full queue held back resets the connection once it is
         sent; a write that waited for room reports the reset. *)
      t.reset <- true;
      t.refused <- true;
      if turn then leave (`Raise (reset_exn ())) else leave (`Lost n)
    end
    else
      let k = queue t ~turn bufs n in
      if k > 0 && Option.is_some t.timer then t.last <- now t;
      (`Done k, Waiters.pass t.readers @ room t)
  in
  match result with
  | `Wait w ->
      let turn = await ?until t t.writers w in
      write_turn ~turn ?until t ~back bufs
  | `Sent n ->
      (* The writer's reads now wait for an acknowledgement too. *)
      update back ignore;
      n
  | `Raise e -> raise e
  | `Shut ->
      (* EPIPE after our own send shutdown; on TCP it reports a pending
         reset first, as Linux reports its pending error. *)
      if not t.local then consume_reset back;
      raise (reset_exn ())
  | `Refused ->
      (* A TCP write reports the pending reset; a Unix-domain one fails
         with EPIPE and leaves it to the next read. *)
      if not t.local then consume_reset back;
      raise (reset_exn ())
  | `Lost n ->
      reset back;
      n
  | `Dropped n ->
      (* The reader's reset: the writer's reads raise it, and so do the
         reader's writes. *)
      update back (fun b -> b.refused <- true);
      reset back;
      n
  | `Done n -> n

let write ?until t ~back bufs = write_turn ~turn:false ?until t ~back bufs

let rec take t dst got =
  match Queue.peek_opt t.queued with
  | Some src when got < Cstruct.length dst ->
      let avail = Cstruct.length src - t.head in
      let n = min avail (Cstruct.length dst - got) in
      Cstruct.blit src t.head dst got n;
      if n = avail then begin
        ignore (Queue.pop t.queued : Cstruct.t);
        t.head <- 0
      end
      else t.head <- t.head + n;
      take t dst (got + n)
  | _ -> got

(* What is queued drains before a reset or the end of the stream. *)
let rec read_turn ~turn ?until t dst =
  let result =
    locked t @@ fun t ->
    if Waiters.must_wait t.readers ~turn then
      (`Wait (Waiters.queue t.readers ~turn), [])
    else if Option.is_some t.silent then
      match t.silent with
      | Some left when left > 0 ->
          if turn then t.readers.turn <- false;
          let n =
            take t (Cstruct.sub dst 0 (min left (Cstruct.length dst))) 0
          in
          t.size <- t.size - n;
          t.unacked <- t.unacked + n;
          t.silent <- Some (left - n);
          (`Read n, if left > n then Waiters.pass t.readers else [])
      | _ when receive_shut t ->
          (`Raise End_of_file, Waiters.release t.readers ~turn)
      | _ -> (`Wait (Waiters.queue t.readers ~turn), [])
    else if not (Queue.is_empty t.queued) then begin
      if turn then t.readers.turn <- false;
      let n = take t dst 0 in
      t.size <- t.size - n;
      if receive_shut t then t.unacked <- t.unacked + n;
      let next =
        if Queue.is_empty t.queued then [] else Waiters.pass t.readers
      in
      (`Read n, (if receive_shut t then [] else Waiters.pass t.writers) @ next)
    end
    else if t.reset then begin
      t.reset <- false;
      t.shut <- true;
      (`Raise (reset_exn ()), Waiters.release t.readers ~turn)
    end
    else if t.shut || receive_shut t then
      (`Raise End_of_file, Waiters.release t.readers ~turn)
    else (`Wait (Waiters.queue t.readers ~turn), [])
  in
  match result with
  | `Read n -> n
  | `Raise e -> raise e
  | `Wait w ->
      let turn = await ?until t t.readers w in
      read_turn ~turn ?until t dst

let read ?until t dst = read_turn ~turn:false ?until t dst
let shut t = update t (fun t -> t.shut <- true)

(* The reader shuts its receive side: it still reads what is queued and
   what arrives, as on Linux, and end-of-file once nothing is; what it
   reads frees no room for the writer. macOS drops what is queued. *)
let shut_receive t =
  update t (fun t ->
      shut_reader `Receive t;
      if t.os = Macos then begin
        Queue.clear t.queued;
        t.head <- 0;
        t.unacked <- t.unacked + t.size;
        t.size <- 0;
        if Option.is_some t.silent then t.silent <- Some 0
      end)

(* A TCP reader's FIN advertises the room its dropped bytes took. *)
let open_window t =
  update t (fun t -> if Option.is_none t.silent then t.unacked <- 0)

(* The reader reads end-of-file once nothing is queued: macOS refuses to
   shut down its receive side again. *)
let receive_ended t = Mutex.protect t.mutex (fun () -> receive_shut t || t.shut)

(* The reader shut its send side. *)
let fin_sent t = update t (shut_reader `Send)

(* Nothing crosses from now on. Over TCP what is queued is lost, an end of
   stream or a reset included, as a path that drops every segment loses
   those in flight. AF_UNIX queues at the reader what a returned write
   sent, so a Unix-domain reader still reads it, and a direction that has
   ended keeps its end. *)
let silence t =
  update t (fun t ->
      if Option.is_none t.silent then
        if not t.local then begin
          t.silent <- Some 0;
          t.silenced <- now t;
          if t.size > 0 && Option.is_some t.timer then t.oldest <- Some (now t)
        end
        else if
          (not (t.shut || t.reset || t.refused))
          &&
          match t.reader with
          | Live { receive = Open; _ } -> true
          | _ -> false
        then t.silent <- Some t.size)

(* A silent TCP pipe with a timer: when it went silent, when its writer last
   sent a byte before, and when it wrote the oldest one not acknowledged. *)
let timing t =
  Mutex.protect t.mutex (fun () ->
      match (t.silent, t.timer) with
      | Some _, Some _ -> Some (t.silenced, t.last, t.oldest)
      | _ -> None)

(* The reader closes: bytes it left unread answer the writer with a reset,
   which refuses its writes at once. *)
let leave t =
  locked t @@ fun t ->
  if Option.is_some t.silent then (false, [])
  else
    let unread = not (Queue.is_empty t.queued) in
    Queue.clear t.queued;
    t.head <- 0;
    t.size <- 0;
    t.reader <- Gone;
    if unread then t.refused <- true;
    (unread, Waiters.take_all t.readers @ Waiters.take_all t.writers)

let os t = t.os
let local t = t.local
let timed t = Option.is_some t.timer
let wake t = update t ignore
let refuse t = update t (fun t -> t.refused <- true)
