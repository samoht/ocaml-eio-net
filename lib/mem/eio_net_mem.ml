open! Hermetic

type Eio.Exn.Backend.t +=
  | Closed
  | No_listener
  | Address_in_use
  | No_destination
  | Message_too_long
  | Invalid_port
  | Network_unreachable
  | Address_family
  | Wrong_socket_type
  | Not_connected

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Closed ->
        Format.pp_print_string ppf "Closed (eio-net.mem)";
        true
    | No_listener ->
        Format.pp_print_string ppf "No listener bound (eio-net.mem)";
        true
    | Address_in_use ->
        Format.pp_print_string ppf "Address in use (eio-net.mem)";
        true
    | No_destination ->
        Format.pp_print_string ppf "No destination (eio-net.mem)";
        true
    | Message_too_long ->
        Format.pp_print_string ppf "Message too long (eio-net.mem)";
        true
    | Invalid_port ->
        Format.pp_print_string ppf "Invalid port (eio-net.mem)";
        true
    | Network_unreachable ->
        Format.pp_print_string ppf "Network unreachable (eio-net.mem)";
        true
    | Address_family ->
        Format.pp_print_string ppf "Address family not supported (eio-net.mem)";
        true
    | Wrong_socket_type ->
        Format.pp_print_string ppf "Wrong socket type (eio-net.mem)";
        true
    | Not_connected ->
        Format.pp_print_string ppf "Not connected (eio-net.mem)";
        true
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)
let mapped_prefix = "\000\000\000\000\000\000\000\000\000\000\xff\xff"

(* The IPv4 address an IPv4-mapped IPv6 address stands for, or [ip]. *)
let unmap (ip : Eio.Net.Ipaddr.v4v6) =
  let raw = (ip :> string) in
  if String.length raw = 16 && String.equal (String.sub raw 0 12) mapped_prefix
  then Eio.Net.Ipaddr.of_raw (String.sub raw 12 4)
  else ip

let is_v4 ip = String.length (unmap ip :> string) = 4

let any ip =
  Eio.Net.Ipaddr.fold ip
    ~v4:(fun _ -> Eio.Net.Ipaddr.V4.any)
    ~v6:(fun _ -> Eio.Net.Ipaddr.V6.any)

(* The IPv4-mapped IPv6 form of an IPv4 address. *)
let map (ip : Eio.Net.Ipaddr.v4v6) =
  Eio.Net.Ipaddr.of_raw (mapped_prefix ^ (ip :> string))

let same (x : Eio.Net.Ipaddr.v4v6) (y : Eio.Net.Ipaddr.v4v6) =
  String.equal (x :> string) (y :> string)

(* The addresses, best first, at which a socket takes a connection or a
   datagram sent to [ip]: the address itself, in either form if it is IPv4,
   then the wildcard address of its family, and for IPv4 the IPv6 wildcard
   address of a dual-stack socket. A wildcard destination stands for the
   loopback address of its family, as both kernels route it. *)
let reaching ip =
  let ip = unmap ip in
  if is_v4 ip then
    let ip = if same ip (any ip) then Eio.Net.Ipaddr.V4.loopback else ip in
    let v4_any = Eio.Net.Ipaddr.V4.any in
    [ ip; map ip; v4_any; map v4_any; Eio.Net.Ipaddr.V6.any ]
  else
    let ip = if same ip (any ip) then Eio.Net.Ipaddr.V6.loopback else ip in
    [ ip; Eio.Net.Ipaddr.V6.any ]

(* [ip] as a socket bound at [own] sees it: IPv4 to an IPv4 socket, and
   IPv4-mapped to an IPv6 one. *)
let seen_by (own : Eio.Net.Ipaddr.v4v6) ip =
  if String.length (own :> string) = 4 then unmap ip
  else if is_v4 ip then map (unmap ip)
  else ip

(* Two IP addresses of one port overlap when they are equal, one is the
   wildcard address of the other's family, or one is the IPv6 wildcard address,
   which a dual-stack socket holds at IPv4 addresses too, as bind(2) on Linux
   has it. An IPv4-mapped address is the IPv4 address it maps. *)
let ip_overlaps (ip, port) (ip', port') =
  let dual x = same x Eio.Net.Ipaddr.V6.any in
  let x = unmap ip and y = unmap ip' in
  port = port'
  && (dual ip || dual ip'
     || (same (any x) (any y) && (same x y || same x (any x) || same y (any y)))
     )

let address_in_use () = Eio.Exn.create (Eio.Exn.X Address_in_use)

(* The error a socket at [src] meets reaching [dst] across address families,
   IPv6 against IPv4-mapped, as on Linux. *)
let cross_family (src : Eio.Net.Ipaddr.v4v6) (dst : Eio.Net.Ipaddr.v4v6) =
  if String.length (src :> string) <> 16 || String.length (dst :> string) <> 16
  then None
  else if is_v4 src && not (is_v4 dst) then Some Address_family
  else if
    (not (is_v4 src)) && (not (same src Eio.Net.Ipaddr.V6.any)) && is_v4 dst
  then Some Network_unreachable
  else None

(* The IP addresses at which sockets of one kind hold each port, kept as they
   bind and close, so a bind checks only the sockets of its port. [lowest] is a
   port of the dynamic range (RFC 6335, section 6) at or below the lowest one
   no socket holds: every port below it is held; [highest] is one at or above
   the highest free one. *)
module Ports = struct
  type t = {
    held : (int, Eio.Net.Ipaddr.v4v6 list) Hashtbl.t;
    mutable lowest : int;
    mutable highest : int;
  }

  let v () = { held = Hashtbl.create 64; lowest = 49152; highest = 65535 }
  let at t port = Option.value ~default:[] (Hashtbl.find_opt t.held port)
  let add t (ip, port) = Hashtbl.replace t.held port (ip :: at t port)

  let remove t (ip, port) =
    let rec drop = function
      | [] -> []
      | ip' :: rest -> if same ip ip' then rest else ip' :: drop rest
    in
    match drop (at t port) with
    | [] ->
        Hashtbl.remove t.held port;
        if port >= 49152 && port < t.lowest then t.lowest <- port;
        if port <= 65535 && port > t.highest then t.highest <- port
    | ips -> Hashtbl.replace t.held port ips

  let in_use t (ip, port) =
    List.exists (fun ip' -> ip_overlaps (ip, port) (ip', port)) (at t port)

  (* The lowest port of the dynamic range no socket holds, at any address. *)
  let free t =
    let rec find port =
      if port > 65535 then None
      else if Hashtbl.mem t.held port then find (port + 1)
      else begin
        t.lowest <- port;
        Some port
      end
    in
    find t.lowest

  (* The highest port of the dynamic range no socket holds. *)
  let free_down t =
    let rec find port =
      if port < 49152 then None
      else if Hashtbl.mem t.held port then find (port - 1)
      else begin
        t.highest <- port;
        Some port
      end
    in
    find t.highest
end

(* A Unix path no socket binds has no file, as Eio's backends remove a
   listener's file when the switch it listened under finishes. *)
let no_file () = Eio.Fs.err (Not_found No_listener)

(* The Unix-domain paths that hold a socket file, shared by stream and
   datagram sockets as a file system shares them. A socket's file stays once
   it closes, [Stale], as Eio's backends leave a datagram socket's file, until
   it is removed. *)
module Files = struct
  type kind = Listener | Datagram | Client | Stale
  type t = { mutex : Mutex.t; files : (string, kind) Hashtbl.t }

  let v () = { mutex = Mutex.create (); files = Hashtbl.create 8 }

  let find t path =
    Mutex.protect t.mutex (fun () -> Hashtbl.find_opt t.files path)

  (* Binds [path] unless a file is there; [take_over] binds over it. *)
  let bind t ?(take_over = false) path kind =
    Mutex.protect t.mutex @@ fun () ->
    (take_over || not (Hashtbl.mem t.files path))
    && begin
      Hashtbl.replace t.files path kind;
      true
    end

  (* The socket of [kind] at [path] closes and leaves its file, or with
     [remove] takes it away. *)
  let leave ?(remove = false) t path kind =
    Mutex.protect t.mutex @@ fun () ->
    match Hashtbl.find_opt t.files path with
    | Some k when k = kind ->
        if remove then Hashtbl.remove t.files path
        else Hashtbl.replace t.files path Stale
    | Some _ | None -> ()
end

(* A stream option is ignored only where the queues already provide what it
   asks for, so ignoring it yields the requested behaviour rather than
   withholding it: [TCP_NODELAY] and [TCP_QUICKACK]. Every other option would
   change what a caller observes and is refused by name, as is every option on
   a datagram socket. *)
module Sockopt = struct
  let refuse opt =
    Fmt.invalid_arg "eio-net.mem does not support socket option %a"
      Eio.Net.Sockopt.pp opt

  let stream : type a. a Eio.Net.Sockopt.t -> unit = function
    | Eio.Net.Sockopt.TCP_NODELAY -> ()
    | Eio.Net.Sockopt.TCP_QUICKACK -> ()
    | opt -> refuse opt

  let datagram : type a. a Eio.Net.Sockopt.t -> unit = refuse
end

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
   clears its pending error; reads then return end-of-file. [gone] says the
   reader has closed: the first write to it is lost and answered with a reset,
   which [refused] then makes every later write raise; on a Unix-domain
   connection, [local], the first write raises it, as EPIPE. [rcv_shut] says the reader
   shut its receive side: it reads what is queued, then end-of-file. Once it
   has also shut its send side, [fin_sent], data reaching a TCP reader resets
   the connection, as Linux resets it in FIN-WAIT; a write to a Unix-domain
   reader that shut its receive side raises the reset, as EPIPE. *)
module Pipe = struct
  type t = {
    mutex : Mutex.t;
    queued : Cstruct.t Queue.t;
    mutable head : int; (* Bytes of the first queued buffer already read. *)
    mutable shut : bool;
    mutable reset : bool;
    mutable gone : bool;
    mutable refused : bool;
    mutable rcv_shut : bool;
    mutable fin_sent : bool;
    local : bool;
    capacity : int;
    mutable size : int; (* Bytes queued and not yet read. *)
    readers : Waiters.t;
    writers : Waiters.t;
  }

  let v ~local ~capacity =
    {
      local;
      mutex = Mutex.create ();
      queued = Queue.create ();
      head = 0;
      shut = false;
      reset = false;
      gone = false;
      refused = false;
      rcv_shut = false;
      fin_sent = false;
      capacity;
      size = 0;
      readers = Waiters.v ();
      writers = Waiters.v ();
    }

  (* Runs [f] under the mutex; it returns its result and the waiters to wake
     once the mutex is released. *)
  let locked t f =
    let result, woken = Mutex.protect t.mutex (fun () -> f t) in
    Waiters.wake woken;
    result

  (* Waits for the wake-up of [waiter], registered in [q]. *)
  let await t q (p, waiter) =
    match Eio.Promise.await p with
    | () -> ()
    | exception (Eio.Cancel.Cancelled _ as e) ->
        locked t (fun _ ->
            if Waiters.remove q waiter then ((), [])
            else begin
              q.turn <- false;
              ((), Waiters.pass q)
            end);
        raise e

  (* Changes the state under the mutex and wakes every waiter. *)
  let update t f =
    locked t (fun t ->
        f t;
        ((), Waiters.take_all t.readers @ Waiters.take_all t.writers))

  (* The reset reaches the reader once it has read what is queued. A TCP
     reader that has the writer's end-of-file keeps reading end-of-file, as
     Linux reports the end of a stream before a reset that came after it. *)
  let reset t =
    update t (fun t -> if t.local || not t.shut then t.reset <- true)

  (* The reset was reported: the stream has ended. *)
  let consume_reset t =
    update t (fun t ->
        if t.reset then begin
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

  let rec write ?(turn = false) t ~back bufs =
    let n = Cstruct.lenv bufs in
    let result =
      locked t @@ fun t ->
      let leave outcome = (outcome, Waiters.release t.writers ~turn) in
      (* An empty write sends nothing, so it meets no shutdown or closed
         peer, as on Linux. *)
      if n = 0 then leave (`Done 0)
      else if t.shut then leave (`Raise (reset_exn ()))
      else if t.refused then leave `Refused
      else if t.gone then begin
        t.refused <- true;
        if t.local then leave (`Raise (reset_exn ())) else leave (`Lost n)
      end
      else if t.rcv_shut && t.local then leave (`Raise (reset_exn ()))
      else if t.rcv_shut && t.fin_sent then begin
        t.reset <- true;
        t.refused <- true;
        leave (`Lost n)
      end
      else if Waiters.must_wait t.writers ~turn || t.size >= t.capacity then
        (`Wait (Waiters.queue t.writers ~turn), [])
      else begin
        if turn then t.writers.turn <- false;
        let k = min n (t.capacity - t.size) in
        push t bufs k;
        t.size <- t.size + k;
        let next = if t.size < t.capacity then Waiters.pass t.writers else [] in
        (`Done k, Waiters.pass t.readers @ next)
      end
    in
    match result with
    | `Wait w ->
        await t t.writers w;
        write ~turn:true t ~back bufs
    | `Raise e -> raise e
    | `Refused ->
        (* A TCP write reports the pending reset; a Unix-domain one fails
           with EPIPE and leaves it to the next read. *)
        if not t.local then consume_reset back;
        raise (reset_exn ())
    | `Lost n ->
        reset back;
        n
    | `Done n -> n

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
  let rec read ?(turn = false) t dst =
    let result =
      locked t @@ fun t ->
      if Waiters.must_wait t.readers ~turn then
        (`Wait (Waiters.queue t.readers ~turn), [])
      else if not (Queue.is_empty t.queued) then begin
        if turn then t.readers.turn <- false;
        let n = take t dst 0 in
        t.size <- t.size - n;
        let next =
          if Queue.is_empty t.queued then [] else Waiters.pass t.readers
        in
        (`Read n, Waiters.pass t.writers @ next)
      end
      else if t.reset then begin
        t.reset <- false;
        t.shut <- true;
        (`Raise (reset_exn ()), Waiters.release t.readers ~turn)
      end
      else if t.shut || t.rcv_shut then
        (`Raise End_of_file, Waiters.release t.readers ~turn)
      else (`Wait (Waiters.queue t.readers ~turn), [])
    in
    match result with
    | `Read n -> n
    | `Raise e -> raise e
    | `Wait w ->
        await t t.readers w;
        read ~turn:true t dst

  let shut t = update t (fun t -> t.shut <- true)

  (* The reader shuts its receive side: it still reads what is queued and
     what arrives, as on Linux, and end-of-file once nothing is. *)
  let shut_receive t = update t (fun t -> t.rcv_shut <- true)

  (* The reader shut its send side. *)
  let fin_sent t = update t (fun t -> t.fin_sent <- true)

  (* The reader closes: bytes it left unread answer the writer with a reset,
     which refuses its writes at once. *)
  let leave t =
    locked t @@ fun t ->
    let unread = not (Queue.is_empty t.queued) in
    Queue.clear t.queued;
    t.head <- 0;
    t.size <- 0;
    t.gone <- true;
    if unread then t.refused <- true;
    (unread, Waiters.take_all t.readers @ Waiters.take_all t.writers)
end

module Flow = struct
  module Impl = struct
    type tag = [ `Generic ]

    (* [release] is the hook that closes the flow when the switch it was
       opened under finishes, removed once the flow is closed. *)
    type t = {
      to_peer : Pipe.t;
      from_peer : Pipe.t;
      mutable closed : bool;
      mutable release : Eio.Switch.hook;
      mutable on_close : unit -> unit;
    }

    (* As Eio's own sockets refuse a descriptor used after close. *)
    let check t op =
      if t.closed then Fmt.invalid_arg "eio-net.mem: %s on a closed flow" op

    let single_write t bufs =
      check t "write";
      Pipe.write t.to_peer ~back:t.from_peer bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    let single_read t buf =
      check t "read";
      if Cstruct.length buf = 0 then 0 else Pipe.read t.from_peer buf

    let shutdown t cmd =
      check t "shutdown";
      match cmd with
      | `Send ->
          Pipe.shut t.to_peer;
          Pipe.fin_sent t.from_peer
      | `Receive -> Pipe.shut_receive t.from_peer
      | `All ->
          Pipe.shut t.to_peer;
          Pipe.fin_sent t.from_peer;
          Pipe.shut_receive t.from_peer

    (* [abort] resets the peer, as a connection a listener drops unaccepted
       is; otherwise the peer reads end-of-file, or a reset if this end left
       bytes unread. *)
    let close_with ~abort t =
      if not t.closed then begin
        t.closed <- true;
        Eio.Switch.remove_hook t.release;
        t.release <- Eio.Switch.null_hook;
        let unread = Pipe.leave t.from_peer in
        if abort then Pipe.update t.from_peer (fun p -> p.refused <- true);
        if unread || abort then Pipe.reset t.to_peer;
        Pipe.shut t.to_peer;
        t.on_close ()
      end

    let close t = close_with ~abort:false t
    let read_methods = []
    let setsockopt _ opt _ = Sockopt.stream opt

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  type t = Impl.t * Impl.tag Eio.Net.stream_socket_ty Eio.Resource.t

  let v ~from_peer ~to_peer =
    let impl =
      Impl.
        {
          from_peer;
          to_peer;
          closed = false;
          release = Eio.Switch.null_hook;
          on_close = ignore;
        }
    in
    (impl, Eio.Resource.T (impl, handler))

  let pair ~local ~capacity =
    let to_client = Pipe.v ~local ~capacity in
    let to_server = Pipe.v ~local ~capacity in
    let client = v ~from_peer:to_client ~to_peer:to_server in
    let server = v ~from_peer:to_server ~to_peer:to_client in
    (client, server)

  (* Closes [flow] when [sw] finishes, unless it is closed first. *)
  let close_with sw ((impl, flow) : t) =
    impl.Impl.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Flow.close flow);
    flow

  let close ((_, flow) : t) = Eio.Flow.close flow
  let on_close ((impl, _) : t) f = impl.Impl.on_close <- f
  let abort ((impl, _) : t) = Impl.close_with ~abort:true impl
end

module Listening_socket = struct
  type tag = [ `Generic ]

  type hub = {
    listeners : (Eio.Net.Sockaddr.stream, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Eio.Mutex.t;
  }

  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.stream;
    incoming : (Flow.t * Eio.Net.Sockaddr.stream) Eio.Stream.t;
    closed : bool Atomic.t;
    mutable hooks : Eio.Switch.hook list;
  }

  (* A Unix-domain path is taken over with [reuse_addr], as Eio's POSIX backend
     removes the socket file there before it binds; the listener that held it
     stays open, unreachable. *)
  let bind hub ~reuse_addr addr =
    let addr =
      match addr with
      | `Tcp (ip, 0) ->
          Option.map (fun port -> `Tcp (ip, port)) (Ports.free hub.ports)
      | addr -> Some addr
    in
    match addr with
    | None -> Error `No_port
    | Some addr ->
        let bound =
          match addr with
          | `Unix path ->
              Files.bind hub.files ~take_over:reuse_addr path Files.Listener
          | `Tcp a -> not (Ports.in_use hub.ports a)
        in
        if not bound then Error `In_use
        else
          let t =
            {
              hub;
              addr;
              incoming = Eio.Stream.create 128;
              closed = Atomic.make false;
              hooks = [];
            }
          in
          Hashtbl.replace hub.listeners addr t;
          (match addr with `Tcp a -> Ports.add hub.ports a | `Unix _ -> ());
          Ok t

  (* The refusals are raised once the mutex is released, which an exception
     inside it would poison. *)
  let v hub ~reuse_addr addr =
    match
      Eio.Mutex.use_rw ~protect:true hub.mutex (fun () ->
          bind hub ~reuse_addr addr)
    with
    | Ok t -> t
    | Error `In_use -> raise (address_in_use ())
    | Error `No_port ->
        failwith "eio-net.mem: every port of 49152-65535 has a listener"

  let accept t ~sw =
    if Atomic.get t.closed then
      invalid_arg "accept on closed in-memory listener";
    let flow, client_addr = Eio.Stream.take t.incoming in
    (Flow.close_with sw flow, client_addr)

  (* The connections waiting to be accepted are reset with the listener, as
     a kernel resets them. A connect that queues one after this is
     refused ([Network.connect]). *)
  let close t =
    if not (Atomic.exchange t.closed true) then begin
      List.iter Eio.Switch.remove_hook t.hooks;
      t.hooks <- [];
      (* A path taken over is its new listener's to free. A close in a
         cancelled fiber frees the address all the same: taking the mutex
         is not cancelled. *)
      Eio.Cancel.protect @@ fun () ->
      Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
          match Hashtbl.find_opt t.hub.listeners t.addr with
          | Some held when held == t -> (
              Hashtbl.remove t.hub.listeners t.addr;
              match t.addr with
              | `Tcp a -> Ports.remove t.hub.ports a
              | `Unix path -> Files.leave t.hub.files path Listener)
          | Some _ | None -> ());
      let rec drain () =
        match Eio.Stream.take_nonblocking t.incoming with
        | Some (flow, _) ->
            Flow.abort flow;
            drain ()
        | None -> ()
      in
      drain ()
    end

  let listening_addr t = t.addr
  let setsockopt _ opt _ = Sockopt.stream opt

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
end

(* A datagram socket bound at [addr]: the datagrams sent to it and not yet
   received, each with the address it came from. Like a pipe, its queue is
   guarded by a mutex and holds whatever is sent, so a datagram is never
   dropped or reordered on the way, and a send never waits. *)
module Datagram_socket = struct
  type tag = [ `Generic ]

  type hub = {
    sockets : (Eio.Net.Sockaddr.datagram, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Mutex.t;
  }

  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : (Eio.Net.Sockaddr.datagram * string) Queue.t;
    readable : Eio.Condition.t;
    mutable closed : bool;
    mutable send_shut : bool;
    mutable recv_shut : bool;
    mutable release : Eio.Switch.hook;
  }

  let free_port hub =
    match Ports.free hub.ports with
    | Some port -> port
    | None ->
        failwith "eio-net.mem: every port of 49152-65535 has a datagram socket"

  let v hub addr =
    Mutex.protect hub.mutex @@ fun () ->
    let addr : Eio.Net.Sockaddr.datagram =
      match addr with
      (* An unbound socket is bound where a kernel binds it on its first
         send: the wildcard address of its family, at an ephemeral port. *)
      | `UdpV4 -> `Udp (Eio.Net.Ipaddr.V4.any, free_port hub)
      | `UdpV6 -> `Udp (Eio.Net.Ipaddr.V6.any, free_port hub)
      | `Udp (ip, 0) -> `Udp (ip, free_port hub)
      | (`Udp _ | `Unix _) as addr -> addr
    in
    let in_use =
      match addr with
      | `Unix path -> not (Files.bind hub.files path Files.Datagram)
      | `Udp a -> Ports.in_use hub.ports a
    in
    if in_use then raise (address_in_use ());
    let t =
      {
        hub;
        addr;
        lock = Mutex.create ();
        queued = Queue.create ();
        readable = Eio.Condition.create ();
        closed = false;
        send_shut = false;
        recv_shut = false;
        release = Eio.Switch.null_hook;
      }
    in
    Hashtbl.replace hub.sockets addr t;
    (match addr with `Udp a -> Ports.add hub.ports a | `Unix _ -> ());
    t

  (* The socket a datagram sent to [addr] reaches ([reaching]). *)
  let find hub (addr : Eio.Net.Sockaddr.datagram) =
    Mutex.protect hub.mutex @@ fun () ->
    match addr with
    | `Udp (ip, port) ->
        List.find_map
          (fun ip -> Hashtbl.find_opt hub.sockets (`Udp (ip, port)))
          (reaching ip)
    | `Unix _ -> Hashtbl.find_opt hub.sockets addr

  (* The longest datagram a socket sends to [dst]: UDP's payload over IPv4 or
     IPv6, or what Linux's default send buffer (212992 bytes, less 32) lets a
     Unix-domain socket send. *)
  let max_length : Eio.Net.Sockaddr.datagram -> int = function
    | `Unix _ -> 212960
    | `Udp (ip, _) -> if is_v4 ip then 65507 else 65527

  (* A send a socket refuses: too long, to another socket type or address
     family, or to port 0. An IPv6 socket sends to an IPv4 address the IPv4
     way, as Linux does: from an IPv6 address other than the wildcard, the
     network is unreachable. *)
  let refusal t dst bufs =
    if Cstruct.lenv bufs > max_length dst then Some Message_too_long
    else
      match (t.addr, dst) with
      | `Udp _, `Unix _ | `Unix _, `Udp _ -> Some Address_family
      | _, `Udp (_, 0) -> Some Invalid_port
      | `Udp (b, _), `Udp (d, _) -> (
          match (String.length (b :> string), String.length (d :> string)) with
          | 4, 16 -> Some Address_family
          | 16, 4 -> cross_family b (map d)
          | _ -> cross_family b d)
      | `Unix _, `Unix _ -> None

  (* The address a datagram from [t] to [dst] comes from, as [peer] sees it: a
     wildcard sender sends from the loopback address the destination
     reaches. *)
  let source t ~dst peer : Eio.Net.Sockaddr.datagram =
    match (t.addr, dst, peer.addr) with
    | `Udp (ip, port), `Udp (d, _), `Udp (own, _) ->
        let ip = unmap ip in
        let ip = if same ip (any ip) then List.hd (reaching d) else ip in
        `Udp (seen_by own ip, port)
    | addr, _, _ -> addr

  (* A datagram to an address no socket binds is lost, as UDP loses it, and
     the sender is not told; at a Unix path, the send finds no file, or one
     that no socket holds any more and refuses it. After a send shutdown a
     send fails with EPIPE, as does one to a Unix-domain socket that shut its
     receive side; Linux checks a Unix-domain socket's shutdown before its
     destination, and a UDP socket's after. *)
  let send t ?dst bufs =
    if t.closed then invalid_arg "eio-net.mem: send on a closed datagram socket";
    let local = match t.addr with `Unix _ -> true | `Udp _ -> false in
    let send_shut () = Mutex.protect t.lock (fun () -> t.send_shut) in
    if local && send_shut () then raise (reset_exn ());
    let dst =
      match dst with
      | Some dst -> dst
      | None ->
          let code = if local then Not_connected else No_destination in
          raise (Eio.Exn.create (Eio.Exn.X code))
    in
    if send_shut () then raise (reset_exn ());
    Option.iter
      (fun code -> raise (Eio.Exn.create (Eio.Exn.X code)))
      (refusal t dst bufs);
    let file =
      match dst with
      | `Unix path -> Files.find t.hub.files path
      | `Udp _ -> None
    in
    match (file, find t.hub dst) with
    | Some (Files.Listener | Client), _ ->
        raise (Eio.Exn.create (Eio.Exn.X Wrong_socket_type))
    | Some Stale, _ ->
        raise (Eio.Net.err (Connection_failure (Refused No_listener)))
    | None, None -> (
        match dst with `Unix _ -> raise (no_file ()) | `Udp _ -> ())
    | Some Datagram, None -> ()
    | (None | Some Datagram), Some peer -> (
        let delivered =
          Mutex.protect peer.lock @@ fun () ->
          if local && peer.recv_shut then `Refused
          else if peer.closed then `Lost
          else begin
            Queue.push
              (source t ~dst peer, Cstruct.to_string (Cstruct.concat bufs))
              peer.queued;
            `Delivered
          end
        in
        match delivered with
        | `Delivered -> Eio.Condition.broadcast peer.readable
        | `Lost -> ()
        | `Refused -> raise (reset_exn ()))

  (* A datagram longer than [buf] is cut to it, as a socket cuts it. *)
  let recv t buf =
    Eio.Condition.loop_no_mutex t.readable @@ fun () ->
    Mutex.protect t.lock @@ fun () ->
    if t.closed then invalid_arg "eio-net.mem: recv on a closed datagram socket"
    else
      match Queue.take_opt t.queued with
      | None -> None
      | Some (src, data) ->
          let n = min (String.length data) (Cstruct.length buf) in
          Cstruct.blit_from_string data 0 buf 0 n;
          Some (src, n)

  let close t =
    let was_open =
      Mutex.protect t.lock @@ fun () ->
      let was_open = not t.closed in
      t.closed <- true;
      was_open
    in
    if was_open then begin
      Eio.Switch.remove_hook t.release;
      t.release <- Eio.Switch.null_hook;
      Mutex.protect t.hub.mutex (fun () ->
          Hashtbl.remove t.hub.sockets t.addr;
          match t.addr with
          | `Unix path -> Files.leave t.hub.files path Datagram
          | `Udp a -> Ports.remove t.hub.ports a);
      Eio.Condition.broadcast t.readable
    end

  let shutdown t cmd =
    Mutex.protect t.lock @@ fun () ->
    if t.closed then
      invalid_arg "eio-net.mem: shutdown on a closed datagram socket";
    match cmd with
    | `Send -> t.send_shut <- true
    | `Receive -> t.recv_shut <- true
    | `All ->
        t.send_shut <- true;
        t.recv_shut <- true

  let setsockopt _ opt _ = Sockopt.datagram opt

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
end

module Network = struct
  type tag = [ `Generic ]

  type t = {
    sw : Eio.Switch.t;
    hub : Listening_socket.hub;
    datagrams : Datagram_socket.hub;
    capacity : int;
  }

  let listening_handler = Eio.Net.Pi.listening_socket (module Listening_socket)

  (* One listener per address: there is no kernel to spread connections
     across several, so [reuse_port] is refused. A listener is closed by
     whichever of [sw] and the network's switch finishes first. *)
  let listen t ~reuse_addr ~reuse_port ~backlog:_ ~sw addr =
    if reuse_port then
      invalid_arg "eio-net.mem does not support SO_REUSEPORT listeners";
    Eio.Switch.check sw;
    let listener = Listening_socket.v t.hub ~reuse_addr addr in
    (* The file a listener leaves goes when the listen's switch finishes, as
       eio_linux removes it then, closed by hand or not; eio_posix keeps the
       file of a listener closed by hand. This runs after the close the hooks
       below make. *)
    (match addr with
    | `Unix path ->
        Eio.Switch.on_release sw (fun () ->
            Files.leave ~remove:true t.hub.files path Stale)
    | `Tcp _ -> ());
    let socket = Eio.Resource.T (listener, listening_handler) in
    let close () = Eio.Resource.close socket in
    listener.hooks <-
      List.map
        (fun sw -> Eio.Switch.on_release_cancellable sw close)
        (if sw == t.sw then [ sw ] else [ sw; t.sw ]);
    socket

  let rec check_options : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (opt, _) :: rest ->
        Sockopt.stream opt;
        check_options rest

  (* Holds the address of a client that connects to [ip] in the listeners'
     ports, until [release]: the address its destination reaches, in the
     destination's family, or the one it is bound at, at a free port counted
     down from the top of the dynamic range as listeners count up from its
     bottom. *)
  let hold t ?bind ip =
    let reached = List.hd (reaching ip) in
    let mapped = String.length (ip :> string) = 16 && is_v4 ip in
    let reached = if mapped then map reached else reached in
    let ip, port =
      match bind with
      | None -> (reached, 0)
      | Some (b, port) ->
          ((if same (unmap b) (any (unmap b)) then reached else b), port)
    in
    Eio.Mutex.use_rw ~protect:true t.hub.mutex @@ fun () ->
    let ports = t.hub.Listening_socket.ports in
    if port <> 0 && Ports.in_use ports (ip, port) then Error `In_use
    else
      match if port = 0 then Ports.free_down ports else Some port with
      | None -> Error `No_port
      | Some port ->
          Ports.add ports (ip, port);
          Ok (ip, port)

  (* Not cancelled, even while it waits for the mutex: a release that gave up
     would hold the port for good. *)
  let release t local =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        Ports.remove t.hub.Listening_socket.ports local)

  (* A connect bound to an address of the other family fails, as macOS fails
     it with EAFNOSUPPORT where Linux says EINVAL; IPv6 against IPv4-mapped
     fails as on Linux. *)
  let check_bind (bind_to : Eio.Net.Sockaddr.stream option)
      (addr : Eio.Net.Sockaddr.stream) =
    let fail code = raise (Eio.Exn.create (Eio.Exn.X code)) in
    match (bind_to, addr) with
    | None, _ | Some (`Unix _), `Unix _ -> ()
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string) ->
        Option.iter fail (cross_family b d)
    | Some _, _ -> fail Address_family

  (* The client's address as the listener at [listener] sees it: an IPv4
     client of an IPv6 listener is IPv4-mapped. A Unix-domain client binds no
     path. *)
  let peer_addr (listener : Eio.Net.Sockaddr.stream) (ip, port) :
      Eio.Net.Sockaddr.stream =
    match listener with
    | `Unix _ -> `Unix ""
    | `Tcp (lip, _) -> `Tcp (seen_by lip ip, port)

  (* A connect for a client that holds [local], if it is a TCP one. *)
  let connect_held t ~sw ~bind_to ~release addr local =
    let listener =
      Eio.Mutex.use_ro t.hub.mutex (fun () ->
          match addr with
          | `Tcp (ip, port) ->
              List.find_map
                (fun ip -> Hashtbl.find_opt t.hub.listeners (`Tcp (ip, port)))
                (reaching ip)
          | `Unix _ -> Hashtbl.find_opt t.hub.listeners addr)
    in
    let refused () =
      raise (Eio.Net.err (Connection_failure (Refused No_listener)))
    in
    match (listener, addr) with
    | None, `Unix path -> (
        match Files.find t.hub.files path with
        | Some Datagram -> raise (Eio.Exn.create (Eio.Exn.X Wrong_socket_type))
        | Some (Stale | Client | Listener) -> refused ()
        | None -> raise (no_file ()))
    | None, `Tcp _ -> refused ()
    | Some listener, _ when Atomic.get listener.closed -> refused ()
    | Some listener, _ ->
        let unix = match addr with `Unix _ -> true | `Tcp _ -> false in
        let client, server = Flow.pair ~local:unix ~capacity:t.capacity in
        Flow.on_close client release;
        let peer =
          match (local, bind_to) with
          | Some a, _ -> peer_addr listener.addr a
          | None, Some (`Unix path) -> `Unix path
          | None, _ -> `Unix ""
        in
        Eio.Stream.add listener.incoming (server, peer);
        (* The listener closed while the connection waited for room: its
           queue was closed before this connection joined it. *)
        if Atomic.get listener.closed then begin
          Flow.close server;
          Flow.close client;
          refused ()
        end;
        Flow.close_with sw client

  (* A connection is a pair of queues. A TCP client holds its address until
     its flow closes; one bound at an address a socket holds finds it in
     use. *)
  let connect t ~bind_to ~options ~sw addr =
    check_options options;
    check_bind bind_to addr;
    let held =
      match (addr, bind_to) with
      | `Unix _, Some (`Unix path) ->
          if Files.bind t.hub.files path Client then `Path path
          else raise (address_in_use ())
      | `Unix _, _ -> `Nothing
      | `Tcp (ip, _), bind -> (
          let bind = match bind with Some (`Tcp b) -> Some b | _ -> None in
          match hold t ?bind ip with
          | Ok local -> `Port local
          | Error `In_use -> raise (address_in_use ())
          | Error `No_port ->
              failwith "eio-net.mem: every port of 49152-65535 is held")
    in
    let local =
      match held with `Port a -> Some a | `Path _ | `Nothing -> None
    in
    (* Released once, by the flow's close or by a failed connect; a bound
       path leaves its file. *)
    let released = Atomic.make false in
    let release () =
      if not (Atomic.exchange released true) then
        match held with
        | `Port a -> release t a
        | `Path path -> Files.leave t.hub.files path Client
        | `Nothing -> ()
    in
    match connect_held t ~sw ~bind_to ~release addr local with
    | flow -> flow
    | exception e ->
        release ();
        raise e

  let datagram_handler = Eio.Net.Pi.datagram_socket (module Datagram_socket)

  (* One socket per address: there is no kernel to spread datagrams across
     several, so [reuse_port] is refused. *)
  let datagram_socket t ~reuse_addr:_ ~reuse_port ~sw addr =
    if reuse_port then
      invalid_arg "eio-net.mem does not support SO_REUSEPORT datagram sockets";
    let datagram = Datagram_socket.v t.datagrams addr in
    let socket = Eio.Resource.T (datagram, datagram_handler) in
    datagram.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Resource.close socket);
    socket

  (* Each address once for streams and once for datagrams, as getaddrinfo(3)
     lists one per socket type; "localhost" IPv6 first, as glibc and macOS
     order it. *)
  let getaddrinfo _ ~service node =
    let v4 = Eio.Net.Ipaddr.V4.loopback and v6 = Eio.Net.Ipaddr.V6.loopback in
    let ips =
      match node with
      | "localhost" -> [ v6; v4 ]
      | "127.0.0.1" -> [ v4 ]
      | "::1" -> [ v6 ]
      | _ -> []
    in
    match int_of_string_opt service with
    | Some port when port >= 0 && port <= 65535 ->
        List.concat_map (fun ip -> [ `Tcp (ip, port); `Udp (ip, port) ]) ips
    | _ -> []

  let getnameinfo _ = function
    | `Tcp (_, port) | `Udp (_, port) -> ("localhost", string_of_int port)
    | `Unix path -> ("localhost", path)
end

let handler = Eio.Net.Pi.network (module Network)

let check_capacity = function
  | None -> max_int
  | Some c when c >= 1 -> c
  | Some c -> Fmt.invalid_arg "eio-net.mem: the capacity is %d" c

let pipe ?capacity () =
  let capacity = check_capacity capacity in
  let (_, client), (_, server) = Flow.pair ~local:false ~capacity in
  (client, server)

let v ?capacity ~sw () =
  let capacity = check_capacity capacity in
  let files = Files.v () in
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      ports = Ports.v ();
      files;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    {
      Datagram_socket.sockets = Hashtbl.create 8;
      ports = Ports.v ();
      files;
      mutex = Mutex.create ();
    }
  in
  Eio.Resource.T (Network.{ sw; hub; datagrams; capacity }, handler)
