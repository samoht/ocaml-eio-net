open! Hermetic
open Kernel
open Code
open Address

type Eio.Exn.Backend.t +=
  | Closed = Code.Closed
  | No_listener = Code.No_listener
  | Address_in_use = Code.Address_in_use
  | No_destination = Code.No_destination
  | Message_too_long = Code.Message_too_long
  | Invalid_port = Code.Invalid_port
  | Network_unreachable = Code.Network_unreachable
  | Address_family = Code.Address_family
  | Wrong_socket_type = Code.Wrong_socket_type
  | Not_connected = Code.Not_connected
  | Address_not_available = Code.Address_not_available
  | Name_too_long = Code.Name_too_long
  | Invalid_address = Code.Invalid_address
  | Broadcast = Code.Broadcast
  | Host_unreachable = Code.Host_unreachable

module Flow = struct
  module Impl = struct
    type tag = [ `Generic ]

    (* [release] is the hook that closes the flow when the switch it was
       opened under finishes, removed once the flow is closed. Its close
       defers to the operations in flight ([Life]). [given_up] says the end
       gave up on its silent peer, and whether it reported ETIMEDOUT yet. *)
    type t = {
      to_peer : Pipe.t;
      from_peer : Pipe.t;
      lock : Mutex.t;
      mutable life : Life.t;
      mutable abort : bool;
      mutable release : Eio.Switch.hook;
      mutable on_close : reset:bool -> abort:bool -> linger2:int option -> unit;
      mutable on_fin : unit -> unit;
      connection : int;
      opts : Sockopt.t;
      mutable given_up : [ `No | `Pending | `Reported ];
    }

    (* When this end gives up on its silent peer, if it does; the earliest
       time once it has. *)
    let until t () =
      if Mutex.protect t.lock (fun () -> t.given_up <> `No) then
        Some Mtime.min_stamp
      else
        match Pipe.timing t.from_peer with
        | None -> None
        | Some (silenced, sent, _) -> (
            match Pipe.timing t.to_peer with
            | None -> None
            | Some (_, acked, oldest) ->
                Sockopt.give_up t.opts ~silenced
                  ~last:
                    (if Mtime.is_later sent ~than:acked then sent else acked)
                  ~oldest)

    (* The error [op] meets once this end gave up: on Linux ETIMEDOUT once,
       for the first read or write, then end-of-file and EPIPE; on macOS
       EPIPE for each write, and ETIMEDOUT once for the first read, which
       a write leaves pending. The first wakes every waiter of this end. *)
    let gave_up t (op : [ `Read | `Write ]) =
      let first, report =
        Mutex.protect t.lock @@ fun () ->
        let first = t.given_up = `No in
        if first then t.given_up <- `Pending;
        let report =
          t.given_up = `Pending && (op = `Read || Pipe.os t.from_peer = Linux)
        in
        if report then t.given_up <- `Reported;
        (first, report)
      in
      if first then begin
        Pipe.wake t.from_peer;
        Pipe.wake t.to_peer
      end;
      if report then Posix.timed_out (op :> [ `Connect | `Read | `Write ])
      else match op with `Read -> End_of_file | `Write -> reset_exn ()

    (* Runs [f] with the deadline of a silent end, or raises what this end
       meets if it has given up. *)
    let watched t op f =
      if not (Pipe.timed t.from_peer) then f None
      else
        let until = until t in
        let expired =
          match until () with
          | Some d -> not (Mtime.is_later d ~than:(Pipe.now t.from_peer))
          | None -> false
        in
        if expired then raise (gave_up t op)
        else
          match f (Some until) with
          | v -> v
          | exception Pipe.Expired -> raise (gave_up t op)

    (* [abort] resets the peer, as a connection a listener drops unaccepted
       is; otherwise the peer reads end-of-file, or a reset if this end left
       bytes unread. *)
    let teardown t =
      let unread = Pipe.leave t.from_peer in
      if t.abort then Pipe.refuse t.from_peer;
      if unread || t.abort then Pipe.reset t.to_peer;
      Pipe.shut t.to_peer;
      t.on_close ~reset:(unread || t.abort) ~abort:t.abort
        ~linger2:(Sockopt.linger2 t.opts)

    let leave t =
      let last =
        Mutex.protect t.lock @@ fun () ->
        let life, last = Life.leave t.life in
        t.life <- life;
        last
      in
      if last then teardown t

    (* Runs [f] as a user of the open socket. As Eio's own sockets refuse a
       descriptor used after close. *)
    let use t op f =
      Mutex.protect t.lock (fun () ->
          match Life.enter t.life with
          | Some life -> t.life <- life
          | None -> Fmt.invalid_arg "%s: the flow is closed" op);
      match f () with
      | v ->
          leave t;
          v
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          leave t;
          Printexc.raise_with_backtrace e bt

    let single_write t bufs =
      use t "Eio.Flow.single_write" @@ fun () ->
      watched t `Write @@ fun until ->
      Pipe.write ?until t.to_peer ~back:t.from_peer bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    (* An empty read is end-of-file, as Eio's backends report the 0 that
       read(2) returns for it. *)
    let single_read t buf =
      use t "Eio.Flow.single_read" @@ fun () ->
      if Cstruct.length buf = 0 then raise End_of_file
      else watched t `Read @@ fun until -> Pipe.read ?until t.from_peer buf

    (* macOS refuses to shut down the receive side of a socket that reads
       end-of-file, and Eio ignores the ENOTCONN: nothing is shut down. *)
    let shutdown t cmd =
      use t "Eio.Flow.shutdown" @@ fun () ->
      (* An end that gave up is closed: Eio ignores the ENOTCONN. *)
      if Mutex.protect t.lock (fun () -> t.given_up = `No) then
        match cmd with
        | (`Receive | `All)
          when Pipe.os t.from_peer = Macos && Pipe.receive_ended t.from_peer ->
            ()
        | `Send ->
            Pipe.shut t.to_peer;
            Pipe.fin_sent t.from_peer;
            t.on_fin ()
        | `Receive -> Pipe.shut_receive t.from_peer
        | `All ->
            Pipe.shut t.to_peer;
            Pipe.fin_sent t.from_peer;
            Pipe.shut_receive t.from_peer;
            if Pipe.os t.from_peer = Macos && not (Pipe.local t.from_peer) then
              Pipe.open_window t.from_peer;
            t.on_fin ()

    let close_with ~abort t =
      let now =
        Mutex.protect t.lock @@ fun () ->
        let life, now = Life.close t.life in
        t.life <- life;
        if now <> `Again then t.abort <- abort;
        now
      in
      if now <> `Again then begin
        Eio.Switch.remove_hook t.release;
        t.release <- Eio.Switch.null_hook
      end;
      if now = `Now then teardown t

    (* A TCP socket that lingers for no time resets its peer as it closes,
       as both kernels abort the connection then. *)
    let close t =
      let abort =
        (not (Pipe.local t.from_peer)) && Sockopt.linger t.opts = Some 0
      in
      close_with ~abort t

    let read_methods = []

    (* An option set on a silent end moves when it gives up. *)
    let setsockopt t opt v =
      use t "Eio.Net.setsockopt" @@ fun () ->
      Sockopt.set t.opts opt v;
      if Option.is_some (Pipe.timing t.from_peer) then begin
        Pipe.wake t.from_peer;
        Pipe.wake t.to_peer
      end

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun t opt -> use t "Eio.Net.getsockopt" @@ fun () -> Sockopt.get t.opts opt
  end

  type (_, _, _) Eio.Resource.pi +=
    | Connection : ('t, 't -> int, [> `Generic ]) Eio.Resource.pi
    | Silence : ('t, 't -> unit, [> `Generic ]) Eio.Resource.pi

  let handler =
    Eio.Resource.handler
      (H (Connection, fun (t : Impl.t) -> t.connection)
      :: H
           ( Silence,
             fun (t : Impl.t) ->
               Pipe.silence t.to_peer;
               Pipe.silence t.from_peer )
      :: Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module Impl)))

  type t = Impl.t * Impl.tag Eio.Net.stream_socket_ty Eio.Resource.t

  let v ~connection ~opts ~from_peer ~to_peer =
    let impl =
      Impl.
        {
          from_peer;
          to_peer;
          lock = Mutex.create ();
          life = Life.v;
          abort = false;
          release = Eio.Switch.null_hook;
          on_close = (fun ~reset:_ ~abort:_ ~linger2:_ -> ());
          on_fin = ignore;
          connection;
          opts;
          given_up = `No;
        }
    in
    (impl, Eio.Resource.T (impl, handler))

  let connections = Atomic.make 0

  (* What a direction of a connection queues when the network sets no
     capacity: at least what Linux's default buffers hold on loopback. A TCP
     send buffer grows to 4 MiB (tcp_wmem), and a writer queues about
     2.6 MiB to a reader that does not read; an AF_UNIX one queues 208 or
     224 KiB of the writer's buffer, each write counted with its overhead,
     at most about 250 KiB. *)
  let default_capacity ~os ~local =
    match os with
    | Linux -> if local then 256 * 1024 else 4 * 1024 * 1024
    | Macos -> if local then 8 * 1024 else 1024 * 1024

  (* What a direction queues: [capacity] until a socket sets a buffer, and at
     most a [capacity] the test gave, [bound]. *)
  let queue ~local ~capacity ~bound ~writer ~reader =
    match Sockopt.queue ~local ~writer ~reader with
    | None -> capacity
    | Some q -> if bound then min q capacity else q

  (* A connection to itself: what it writes, it reads. *)
  let self ~os ~timer ~capacity ~bound ~(opts : Sockopt.t) =
    let connection = Atomic.fetch_and_add connections 1 in
    let size () =
      queue ~local:false ~capacity ~bound ~writer:opts ~reader:opts
    in
    let pipe = Pipe.v ~os ~local:false ~timer ~capacity:(size ()) in
    Sockopt.on_resize opts (fun () -> Pipe.resize pipe (size ()));
    v ~connection ~opts ~from_peer:pipe ~to_peer:pipe

  let pair ~os ~local ~timer ~capacity ~bound ~client:(c : Sockopt.t)
      ~server:(s : Sockopt.t) =
    let connection = Atomic.fetch_and_add connections 1 in
    let size writer reader = queue ~local ~capacity ~bound ~writer ~reader in
    let to_client = Pipe.v ~os ~local ~timer ~capacity:(size s c) in
    let to_server = Pipe.v ~os ~local ~timer ~capacity:(size c s) in
    let resized () =
      Pipe.resize to_client (size s c);
      Pipe.resize to_server (size c s)
    in
    Sockopt.on_resize c resized;
    Sockopt.on_resize s resized;
    let client =
      v ~connection ~opts:c ~from_peer:to_client ~to_peer:to_server
    in
    let server =
      v ~connection ~opts:s ~from_peer:to_server ~to_peer:to_client
    in
    (client, server)

  let connection (Eio.Resource.T (t, ops) : _ Eio.Net.stream_socket) =
    Option.map (fun f -> f t) (Eio.Resource.get_opt ops Connection)

  let silence (Eio.Resource.T (t, ops) : _ Eio.Net.stream_socket) =
    match Eio.Resource.get_opt ops Silence with
    | Some f -> f t
    | None ->
        invalid_arg
          "Eio_net_mem.Private.silence: the socket is not an in-memory \
           connection"

  (* Closes [flow] when [sw] finishes, unless it is closed first. *)
  let close_with sw ((impl, flow) : t) =
    impl.Impl.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Flow.close flow);
    flow

  let close ((_, flow) : t) = Eio.Flow.close flow
  let on_close ((impl, _) : t) f = impl.Impl.on_close <- f
  let on_fin ((impl, _) : t) f = impl.Impl.on_fin <- f
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

  (* The connections waiting to be accepted, at most [backlog] of them, and
     the listener's life, guarded by [lock] so a connect queues one only
     while the listener is open, and a close resets every one it queued. A
     close defers to the accepts in flight ([Life]). *)
  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.stream;
    lock : Mutex.t;
    incoming : (Flow.t * Eio.Net.Sockaddr.stream) Queue.t;
    backlog : int;
    acceptable : Eio.Condition.t;
    room : Eio.Condition.t;
    mutable life : Life.t;
    mutable hooks : Eio.Switch.hook list;
    reuse : bool;
    hold : Ports.hold option;
    opts : Sockopt.t;
  }

  (* Linux's accept queue takes one connection more than the backlog, which
     it caps at [net.core.somaxconn], 4096 by default, as it takes a negative
     one; macOS's takes the backlog, 128 ([kern.ipc.somaxconn]) for one of 0
     or less or past that. *)
  let queue_length ~os backlog =
    match os with
    | Linux -> if backlog < 0 || backlog > 4096 then 4097 else backlog + 1
    | Macos -> if backlog <= 0 || backlog > 128 then 128 else backlog

  let is_closed t = Mutex.protect t.lock (fun () -> Life.is_closed t.life)

  (* A Unix-domain path is taken over with [reuse_addr], as Eio's POSIX backend
     removes the socket file there before it binds; the listener that held it
     stays open, unreachable. *)
  let bind hub ~reuse_addr ~backlog addr =
    let addr =
      match addr with
      | `Tcp (ip, 0) ->
          let free =
            match Ports.os hub.ports with
            | Linux -> Ports.free hub.ports
            | Macos -> Ports.free_next hub.ports
          in
          Option.map (fun port -> `Tcp (ip, port)) free
      | `Unix path -> Some (`Unix (Files.bound hub.files path))
      | addr -> Some addr
    in
    match addr with
    | None -> Error `No_port
    | Some addr ->
        let bound =
          match addr with
          | `Unix path ->
              Files.bind hub.files ~take_over:reuse_addr path Files.Listener
          | `Tcp a -> not (Ports.in_use hub.ports ~reuse:reuse_addr a)
        in
        if not bound then Error `In_use
        else
          let hold =
            match addr with
            | `Tcp a ->
                Some (Ports.add hub.ports ~reuse:reuse_addr ~listening:true a)
            | `Unix _ -> None
          in
          let t =
            {
              hub;
              addr;
              lock = Mutex.create ();
              incoming = Queue.create ();
              backlog = queue_length ~os:(Ports.os hub.ports) backlog;
              acceptable = Eio.Condition.create ();
              room = Eio.Condition.create ();
              life = Life.v;
              hooks = [];
              reuse = reuse_addr;
              hold;
              opts = Sockopt.listener ~os:(Ports.os hub.ports) addr;
            }
          in
          Hashtbl.replace hub.listeners addr t;
          Ok t

  (* The refusals are raised once the mutex is released, which an exception
     inside it would poison. *)
  let v hub ~reuse_addr ~backlog addr =
    (match addr with
    | `Tcp (ip, _) ->
        check_local ~os:(Ports.os hub.ports) ~tcp:true ~fail:bind_fail ip
    | `Unix path -> check_path ~os:(Ports.os hub.ports) ~raw:true path);
    match
      Eio.Mutex.use_rw ~protect:true hub.mutex (fun () ->
          bind hub ~reuse_addr ~backlog addr)
    with
    | Ok t -> t
    (* A port of zero with none of the range free is bind(2)'s EADDRINUSE, as
       an address a socket holds is. *)
    | Error (`In_use | `No_port) -> bind_fail Address_in_use

  (* The connections waiting to be accepted are reset with the listener, as
     a kernel resets them. A connect that queues one after this is
     refused ([Network.connect]). *)
  let shut t =
    (* A path taken over is its new listener's to free. A close in a
       cancelled fiber frees the address all the same: taking the mutex is
       not cancelled. *)
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        match Hashtbl.find_opt t.hub.listeners t.addr with
        | Some held when held == t -> (
            Hashtbl.remove t.hub.listeners t.addr;
            match (t.addr, t.hold) with
            | `Tcp (_, port), Some h -> Ports.remove t.hub.ports port h
            | `Tcp _, None -> ()
            | `Unix path, _ -> Files.leave t.hub.files path Listener)
        | Some _ | None -> ());
    let queued =
      Mutex.protect t.lock @@ fun () ->
      let queued = List.of_seq (Queue.to_seq t.incoming) in
      Queue.clear t.incoming;
      queued
    in
    List.iter (fun (flow, _) -> Flow.abort flow) queued;
    Eio.Condition.broadcast t.room

  let leave t =
    let last =
      Mutex.protect t.lock @@ fun () ->
      let life, last = Life.leave t.life in
      t.life <- life;
      last
    in
    if last then shut t

  let accept t ~sw =
    Mutex.protect t.lock (fun () ->
        match Life.enter t.life with
        | Some life -> t.life <- life
        | None -> invalid_arg "Eio.Net.accept: the listener is closed");
    match
      Eio.Condition.loop_no_mutex t.acceptable @@ fun () ->
      Mutex.protect t.lock (fun () -> Queue.take_opt t.incoming)
    with
    | flow, client_addr ->
        leave t;
        Eio.Condition.broadcast t.room;
        (Flow.close_with sw flow, client_addr)
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        leave t;
        Printexc.raise_with_backtrace e bt

  (* Queues a connection, waiting while the queue is full. *)
  let offer t conn =
    let queued =
      Eio.Condition.loop_no_mutex t.room @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if Life.is_closed t.life then Some `Closed
      else if Queue.length t.incoming < t.backlog then begin
        Queue.push conn t.incoming;
        Some `Queued
      end
      else None
    in
    if queued = `Queued then Eio.Condition.broadcast t.acceptable;
    queued

  let close t =
    let now =
      Mutex.protect t.lock @@ fun () ->
      let life, now = Life.close t.life in
      t.life <- life;
      now
    in
    if now <> `Again then begin
      List.iter Eio.Switch.remove_hook t.hooks;
      t.hooks <- []
    end;
    if now = `Now then shut t

  (* As Eio's own sockets refuse a descriptor used after close. *)
  let listening_addr t =
    if Mutex.protect t.lock (fun () -> not (Life.is_open t.life)) then
      invalid_arg "Eio.Net.listening_addr: the listener is closed";
    t.addr

  let open_opts fn t =
    if Mutex.protect t.lock (fun () -> not (Life.is_open t.life)) then
      Fmt.invalid_arg "%s: the listener is closed" fn;
    t.opts

  let setsockopt t opt v = Sockopt.set (open_opts "Eio.Net.setsockopt" t) opt v

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt -> Sockopt.get (open_opts "Eio.Net.getsockopt" t) opt
end

(* A datagram socket bound at [addr]: the datagrams sent to it and not yet
   received, each with the address it came from. Like a pipe, its queue is
   guarded by a mutex and holds whatever is sent, so a datagram is never
   dropped or reordered on the way, and a send never waits. *)
module Datagram_socket = struct
  type tag = [ `Generic ]

  (* [room] is broadcast whenever a queued datagram goes, so a Unix-domain
     send waiting for room looks again. *)
  type hub = {
    os : os;
    sockets : (Eio.Net.Sockaddr.datagram, t) Hashtbl.t;
    ports : Ports.t;
    files : Files.t;
    mutex : Mutex.t;
    room : Eio.Condition.t;
  }

  (* [rmem] is the memory the datagrams of [queued] take, and those read but
     not yet freed, [unfreed], and [wmem] that of the Unix-domain datagrams
     this socket sent that are still queued, as Linux counts them; [batch] is
     the datagrams left of those queued when a receive last found none taken.
     All are guarded by [lock] but [wmem], which is atomic: no socket takes
     another's lock while it holds its own, as one sending to itself or two
     sending to each other would. *)
  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : datagram Queue.t;
    mutable rmem : int;
    mutable unfreed : int;
    mutable batch : int;
    wmem : int Atomic.t;
    readable : Eio.Condition.t;
    mutable closed : bool;
    mutable send_shut : bool;
    mutable recv_shut : bool;
    mutable release : Eio.Switch.hook;
    hold : Ports.hold option;
    opts : Sockopt.t;
  }

  and datagram = {
    src : Eio.Net.Sockaddr.datagram;
    data : string;
    size : int;
    sender : t option;
  }

  let rec pow2 head p = if p >= head then p else pow2 head (2 * p)

  (* The memory a UDP datagram of [n] bytes takes in a receive queue over
     loopback, as Linux allocates it: a head of 896 bytes or a power of two
     that holds the payload and its headers, up to 16 KiB, then a small head
     and the payload in pages, and 256 bytes of [sk_buff]. *)
  let udp_size ~v4 n =
    let head = n + if v4 then 444 else 456 in
    let size =
      if head <= 896 then 896
      else if head <= 16384 then pow2 head 1024
      else 896 + n
    in
    256 + size

  (* The memory a Unix-domain datagram of [n] bytes takes from its sender's
     buffer until it is received, as Linux allocates it: up to 16000 bytes in
     a head of a power of two that holds them, rounded up to 64, and 384
     bytes of shared information; past that, the rest in pages, 17 at most;
     and 256 bytes of [sk_buff]. *)
  let unix_size n =
    let paged =
      if n <= 16000 then 0
      else min ((n - 16000 + 4095) / 4096 * 4096) (17 * 4096)
    in
    let head = ((n - paged + 63) / 64 * 64) + 384 in
    256 + pow2 head 512 + paged

  (* A Unix-domain socket whose queue holds more than [max_dgram_qlen]
     datagrams makes a send to it wait, but its own send to itself. *)
  let max_dgram_qlen = 10

  (* What a datagram of [n] bytes takes of a macOS receive buffer: its
     length, and at least 128 bytes on a Unix path, which fits what one Mac
     holds of 1, 500, 1000 and 2000 bytes. *)
  let macos_size ~local n = if local then max n 128 else n

  (* A port of zero with none of the range free is bind(2)'s EADDRINUSE. *)
  let free_port hub =
    match Ports.free hub.ports with
    | Some port -> port
    | None -> bind_fail Address_in_use

  let v hub ~reuse addr =
    (match addr with
    | `Udp (ip, _) -> check_local ~os:hub.os ~fail:bind_fail ip
    | `Unix path -> check_path ~os:hub.os ~raw:true path
    | `UdpV4 | `UdpV6 -> ());
    Mutex.protect hub.mutex @@ fun () ->
    let addr : Eio.Net.Sockaddr.datagram =
      match addr with
      (* An unbound socket is bound where a kernel binds it on its first
         send: the wildcard address of its family, at an ephemeral port. *)
      | `UdpV4 -> `Udp (Eio.Net.Ipaddr.V4.any, free_port hub)
      | `UdpV6 -> `Udp (Eio.Net.Ipaddr.V6.any, free_port hub)
      | `Udp (ip, 0) -> `Udp (ip, free_port hub)
      | `Unix "" -> `Unix (Files.autobind hub.files)
      | (`Udp _ | `Unix _) as addr -> addr
    in
    let in_use =
      match addr with
      | `Unix path -> not (Files.bind hub.files path Files.Datagram)
      | `Udp a -> Ports.in_use hub.ports ~reuse a
    in
    if in_use then bind_fail Address_in_use;
    let hold =
      match addr with
      | `Udp a -> Some (Ports.add hub.ports ~reuse a)
      | `Unix _ -> None
    in
    let t =
      {
        hub;
        addr;
        lock = Mutex.create ();
        queued = Queue.create ();
        rmem = 0;
        unfreed = 0;
        batch = 0;
        wmem = Atomic.make 0;
        readable = Eio.Condition.create ();
        closed = false;
        send_shut = false;
        recv_shut = false;
        release = Eio.Switch.null_hook;
        hold;
        opts = Sockopt.datagram ~os:hub.os addr;
      }
    in
    (* The latest bound of sockets sharing an address takes its datagrams,
       as on Linux. *)
    Hashtbl.add hub.sockets addr t;
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

  (* The longest datagram [t] sends to [dst]: UDP's payload over IPv4 or
     IPv6, or on a Unix path 212960 bytes, what Linux sends with a send
     buffer of 212992 bytes, and less, its send buffer less 32, for a smaller
     one. *)
  let max_length t : Eio.Net.Sockaddr.datagram -> int = function
    | `Unix _ -> min 212960 (Sockopt.sndbuf t.opts - 32)
    | `Udp (ip, _) -> if is_v4 ip then 65507 else 65527

  (* macOS checks the length first: net.inet.udp.maxdgram and
     net.local.dgram.maxdgram. A Unix-domain socket refuses an IP address,
     an IPv4 one a path, and an IPv6 one an IPv4 address. A destination at a
     wildcard address has no route. An IPv4 socket bound at a wildcard
     address sends nothing to an IPv6 address, and one bound at 127.0.0.1
     finds no route to it. Between IPv6 and IPv4-mapped addresses, a socket
     bound at ::1 finds the IPv4 side not available, and one bound at an
     IPv4-mapped address other than the wildcard has no route to the IPv6
     side. *)
  let refusal_macos t (dst : Eio.Net.Sockaddr.datagram) len =
    let wild x = same (unmap x) (any (unmap x)) in
    if len > Sockopt.sndbuf t.opts then Some Message_too_long
    else
      match (t.addr, dst) with
      | `Unix _, `Udp _ -> Some Address_family
      | `Unix _, `Unix _ -> None
      | `Udp (b, _), `Unix _ ->
          Some
            (if String.length (b :> string) = 4 then Address_not_available
             else Invalid_address)
      | `Udp (b, _), `Udp (d, _) when String.length (b :> string) = 4 ->
          if String.length (d :> string) = 16 then
            Some (if wild b then Invalid_address else Host_unreachable)
          else if wild d then Some Host_unreachable
          else if off_loopback d then Some Network_unreachable
          else None
      | `Udp (b, _), `Udp (d, _) ->
          if String.length (d :> string) = 4 then Some Invalid_address
          else if wild d then Some Host_unreachable
          else if is_v4 d && not (is_v4 b || wild b) then
            Some Address_not_available
          else if (not (is_v4 d)) && is_v4 b && not (wild b) then
            Some Host_unreachable
          else if off_loopback d then Some Network_unreachable
          else None

  (* Without SO_BROADCAST a broadcast is refused, and a socket bound at a
     wildcard address has no route to send one, nor a multicast one. *)
  let routed ~too_long b d port =
    let b = unmap b and d4 = unmap d in
    let wildcard = same b (any b) in
    if port = 0 then Some Invalid_port
    else if lo_broadcast d4 || (broadcast d4 && not wildcard) then
      Some Broadcast
    else if wildcard && (broadcast d4 || multicast_v4 d4) then
      Some Network_unreachable
    else if off_loopback d then Some Network_unreachable
    else if too_long then Some Message_too_long
    else None

  (* What an IPv4 socket bound at [b] refuses. *)
  let refusal_v4 ~too_long ~len b : Eio.Net.Sockaddr.datagram -> _ = function
    | _ when len > 0xffff -> Some Message_too_long
    | `Unix path ->
        Some
          (if String.length path >= 14 then Address_family else Invalid_address)
    | `Udp (d, _) when String.length (d :> string) = 16 -> Some Address_family
    | `Udp (d, port) -> routed ~too_long b d port

  (* A send a socket refuses, in the order Linux checks: a Unix-domain
     socket refuses a UDP address or the empty path with EINVAL, then a
     datagram too long. An IPv4 socket refuses more than 65535 bytes, then a
     path (EINVAL, or EAFNOSUPPORT for one that fills a [sockaddr_in], 14
     bytes or more), an IPv6 address, port 0, an address its loopback address
     cannot route to, then a datagram too long for IPv4. An IPv6 socket
     refuses a path, then another family or an unreachable network, port 0, a
     loopback route, and a datagram too long; it sends to an IPv4 address the
     IPv4 way, from an IPv6 address other than the wildcard unreachable. *)
  let refusal t dst bufs =
    let len = Cstruct.lenv bufs in
    if t.hub.os = Macos then refusal_macos t dst len
    else
      let too_long = len > max_length t dst in
      match (t.addr, dst) with
      | `Unix _, `Udp _ -> Some Invalid_address
      | `Unix _, `Unix "" -> Some Invalid_address
      | `Unix _, `Unix _ -> if too_long then Some Message_too_long else None
      | `Udp (b, _), dst when String.length (b :> string) = 4 ->
          refusal_v4 ~too_long ~len b dst
      | `Udp _, `Unix _ -> Some Invalid_address
      | `Udp (b, _), `Udp (d, port) -> (
          let d16 = if String.length (d :> string) = 4 then map d else d in
          match cross_family ~os:Linux b d16 with
          (* No route reaches an IPv6 address off loopback: Linux says so
             before it finds the families apart. *)
          | Some Address_family when off_loopback d -> Some Network_unreachable
          | Some code -> Some code
          | None -> routed ~too_long b d port)

  (* The address a datagram from [t] to [dst] comes from, as [peer] sees it: a
     wildcard sender sends from the loopback address of the destination's
     family. *)
  let source t ~dst peer : Eio.Net.Sockaddr.datagram =
    match (t.addr, dst, peer.addr) with
    | `Udp (ip, port), `Udp (d, _), `Udp (own, _) ->
        let ip = unmap ip in
        let ip = if same ip (any ip) then source_to d else ip in
        (* On macOS a socket bound at an IPv4-mapped address takes IPv4
           datagrams as an IPv4 socket does. *)
        let own =
          if t.hub.os = Macos && String.length (own :> string) = 16 && is_v4 own
          then unmap own
          else own
        in
        `Udp (seen_by own ip, port)
    | addr, _, _ -> addr

  (* On macOS a datagram that finds its receiver's buffer full is lost over
     UDP, and refused with ENOBUFS on a Unix path, where Linux waits. *)
  let deliver_macos t ~local ~dst peer data =
    let size = macos_size ~local (String.length data) in
    let delivered =
      Mutex.protect peer.lock @@ fun () ->
      if peer.closed then `Lost
      else if peer.rmem + size > Sockopt.rcvbuf peer.opts then
        if local then `Full else `Lost
      else begin
        peer.rmem <- peer.rmem + size;
        Queue.push
          { src = source t ~dst peer; data; size; sender = None }
          peer.queued;
        `Delivered
      end
    in
    match delivered with
    | `Delivered ->
        Eio.Condition.broadcast peer.readable;
        `Done
    | `Lost -> `Done
    | `Full -> raise (Posix.no_buffer_space ())

  (* Queues the datagram at [peer], unless [peer] refuses it (a Unix-domain
     socket that shut its receive side) or has closed. A UDP datagram that
     finds the receive buffer full is lost, as Linux drops it. A Unix-domain
     one waits while the sender's buffer is full, or [peer] holds more than
     [max_dgram_qlen], as on Linux: [`Wait] says to look again. *)
  let deliver t ~local ~dst peer bufs =
    let data = Cstruct.to_string (Cstruct.concat bufs) in
    if t.hub.os = Macos then deliver_macos t ~local ~dst peer data
    else
      let n = String.length data in
      let delivered =
        if local && Atomic.get t.wmem >= Sockopt.sndbuf t.opts then `Wait
        else
          Mutex.protect peer.lock @@ fun () ->
          if local && peer.recv_shut then `Refused
          else if peer.closed then `Lost
          else if local then
            if peer != t && Queue.length peer.queued > max_dgram_qlen then `Wait
            else begin
              let size = unix_size n in
              ignore (Atomic.fetch_and_add t.wmem size);
              Queue.push
                { src = source t ~dst peer; data; size; sender = Some t }
                peer.queued;
              `Delivered
            end
          else
            let v4 =
              match dst with `Udp (d, _) -> is_v4 d | `Unix _ -> false
            in
            let size = udp_size ~v4 n in
            if peer.rmem + size > Sockopt.rcvbuf peer.opts then `Lost
            else begin
              peer.rmem <- peer.rmem + size;
              Queue.push
                { src = source t ~dst peer; data; size; sender = None }
                peer.queued;
              `Delivered
            end
      in
      match delivered with
      | `Delivered ->
          Eio.Condition.broadcast peer.readable;
          `Done
      | `Lost -> `Done
      | `Wait -> `Wait
      | `Refused -> raise (reset_exn ())

  (* A datagram leaves its socket's queue: its memory is free. *)
  let freed t d =
    t.rmem <- t.rmem - d.size;
    Option.iter
      (fun s -> ignore (Atomic.fetch_and_add s.wmem (-d.size)))
      d.sender

  (* A datagram a receive takes. A UDP one's memory is freed with that of
     the others read before, once they reach a quarter of the buffer or the
     receive takes the last of its batch, as Linux frees it. *)
  let taken t d =
    match d.sender with
    | Some _ -> freed t d
    | None when t.hub.os = Macos -> freed t d
    | None ->
        t.unfreed <- t.unfreed + d.size;
        if t.unfreed >= Sockopt.rcvbuf t.opts / 4 || t.batch = 0 then begin
          t.rmem <- t.rmem - t.unfreed;
          t.unfreed <- 0
        end

  (* The destination of a send, or the error a send without one raises; a
     send shutdown first for a Unix-domain socket, after for a UDP one. *)
  let destination t ~local dst =
    let shut () =
      if Mutex.protect t.lock (fun () -> t.send_shut) then raise (reset_exn ())
    in
    if local then shut ();
    let dst =
      match dst with
      | Some dst -> dst
      | None ->
          let code =
            if local && t.hub.os = Linux then Not_connected else No_destination
          in
          fail code
    in
    shut ();
    dst

  (* A datagram to an address no socket binds is lost, as UDP loses it, and
     the sender is not told; at a Unix path, the send finds no file, or one
     that no socket holds any more and refuses it. After a send shutdown a
     send fails with EPIPE, as does one to a Unix-domain socket that shut its
     receive side; Linux checks a Unix-domain socket's shutdown before its
     destination, and a UDP socket's after. *)
  let send t ?dst bufs =
    if t.closed then invalid_arg "Eio.Net.send: the datagram socket is closed";
    let os = t.hub.os in
    (match dst with Some (`Unix path) -> check_path ~os path | _ -> ());
    let local = match t.addr with `Unix _ -> true | `Udp _ -> false in
    let dst =
      match (destination t ~local dst, t.addr) with
      | `Udp (d, port), `Udp (bound, _) when os = Linux ->
          `Udp (toward ~os ~bound d, port)
      | dst, _ -> dst
    in
    Option.iter
      (function
        | Broadcast -> raise (Eio.Fs.err (Permission_denied Broadcast))
        | code -> fail code)
      (refusal t dst bufs);
    match dst with
    | `Udp (d, _) when os = Macos && unanswered d ->
        (* Sent, and lost: macOS's loopback interface takes 127.0.0.1 only. *)
        ()
    | _ -> (
        (* A send that waits looks for its destination again, as Linux does, and
           a Unix-domain one fails once its socket shuts its sending. *)
        Eio.Condition.loop_no_mutex t.hub.room
        @@ fun () ->
        if local && Mutex.protect t.lock (fun () -> t.send_shut) then
          raise (reset_exn ());
        let file =
          match dst with
          | `Unix path -> Files.find t.hub.files ~stream:false path
          | `Udp _ -> None
        in
        match (file, find t.hub dst) with
        | Some (Files.Listener | Client), _ -> fail Wrong_socket_type
        | Some Stale, _ -> refused ()
        | None, None -> (
            match dst with
            | `Unix path when Files.abstract path -> refused ()
            | `Unix _ -> raise (no_file ())
            | `Udp _ -> Some ())
        | Some Datagram, None -> Some ()
        | (None | Some Datagram), Some peer -> (
            match deliver t ~local ~dst peer bufs with
            | `Done -> Some ()
            | `Wait -> None))

  (* A datagram longer than [buf] is cut to it, as a socket cuts it. *)
  let recv t buf =
    let got =
      Eio.Condition.loop_no_mutex t.readable @@ fun () ->
      Mutex.protect t.lock @@ fun () ->
      if t.closed then invalid_arg "Eio.Net.recv: the datagram socket is closed"
      else if t.batch = 0 then t.batch <- Queue.length t.queued;
      match Queue.take_opt t.queued with
      | None -> None
      | Some d ->
          t.batch <- t.batch - 1;
          taken t d;
          let n = min (String.length d.data) (Cstruct.length buf) in
          Cstruct.blit_from_string d.data 0 buf 0 n;
          Some (d.src, n)
    in
    Eio.Condition.broadcast t.hub.room;
    got

  let close t =
    let was_open =
      Mutex.protect t.lock @@ fun () ->
      let was_open = not t.closed in
      t.closed <- true;
      Queue.iter (freed t) t.queued;
      Queue.clear t.queued;
      was_open
    in
    if was_open then begin
      Eio.Condition.broadcast t.hub.room;
      Eio.Switch.remove_hook t.release;
      t.release <- Eio.Switch.null_hook;
      Mutex.protect t.hub.mutex (fun () ->
          let others =
            List.filter
              (fun s -> s != t)
              (Hashtbl.find_all t.hub.sockets t.addr)
          in
          while Hashtbl.mem t.hub.sockets t.addr do
            Hashtbl.remove t.hub.sockets t.addr
          done;
          List.iter (Hashtbl.add t.hub.sockets t.addr) (List.rev others);
          match (t.addr, t.hold) with
          | `Unix path, _ -> Files.leave t.hub.files path Datagram
          | `Udp (_, port), Some h -> Ports.remove t.hub.ports port h
          | `Udp _, None -> ());
      Eio.Condition.broadcast t.readable
    end

  (* A send waiting for room on this socket, or from it, looks again. macOS
     refuses to shut down a socket that is not connected, and Eio ignores the
     ENOTCONN: nothing is shut down. *)
  let shutdown t cmd =
    ( Mutex.protect t.lock @@ fun () ->
      if t.closed then
        invalid_arg "Eio.Flow.shutdown: the datagram socket is closed";
      match cmd with
      | (`Send | `Receive | `All) when t.hub.os = Macos -> ()
      | `Send -> t.send_shut <- true
      | `Receive -> t.recv_shut <- true
      | `All ->
          t.send_shut <- true;
          t.recv_shut <- true );
    Eio.Condition.broadcast t.hub.room

  let open_opts fn t =
    if Mutex.protect t.lock (fun () -> t.closed) then
      Fmt.invalid_arg "%s: the datagram socket is closed" fn;
    t.opts

  let setsockopt t opt v = Sockopt.set (open_opts "Eio.Net.setsockopt" t) opt v

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun t opt -> Sockopt.get (open_opts "Eio.Net.getsockopt" t) opt
end

(* The two ends of a TCP connection, and when each lets go of its address.
   An end holds it while open. Once closed, the end whose FIN went first, by
   a shutdown of its send side or by its close, holds it in TIME_WAIT for 60
   seconds from the peer's FIN, as Linux does, and the other lets go at once.
   Closed with no FIN from its peer, it waits for one in FIN_WAIT2 for its
   TCP_LINGER2, or 60 seconds, Linux's tcp_fin_timeout, as tcp_fin_time has
   it. After a reset, and without a clock, each lets go once both have
   closed. An end its listener dropped unaccepted lets go at once, as Linux
   destroys it. *)
module Ends = struct
  type side = [ `Client | `Server ]

  type decision =
    [ `Held | `Release | `Until of Mtime.t | `Time_wait of Mtime.t ]

  (* An end is open, closed at a time, or dropped by its listener. [fin]
     orders the ends' FINs and says when each went. *)
  type state =
    | Open
    | Closed of { at : Mtime.t; fin_timeout : Mtime.Span.t }
    | Dropped

  type end_ = { mutable fin : (int * Mtime.t) option; mutable state : state }

  type t = {
    lock : Mutex.t;
    now : (unit -> Mtime.t) option;
    time_wait : Mtime.Span.t;
    mutable seq : int;
    ends : end_ array;
    mutable reset : bool;
  }

  let v (ports : Ports.t) =
    {
      lock = Mutex.create ();
      now = Ports.now ports;
      time_wait = Ports.time_wait ports;
      seq = 0;
      ends = Array.init 2 (fun _ -> { fin = None; state = Open });
      reset = false;
    }

  let index : side -> int = function `Client -> 0 | `Server -> 1
  let now t = match t.now with Some now -> now () | None -> Mtime.min_stamp

  let decide t i : decision =
    let e = t.ends.(i) and o = t.ends.(1 - i) in
    match e.state with
    | Open -> `Held
    | Dropped -> `Release
    | Closed { at = closed; fin_timeout } -> (
        let both =
          match o.state with Open -> false | Closed _ | Dropped -> true
        in
        if Option.is_none t.now || t.reset then if both then `Release else `Held
        else
          let after span t = Option.value ~default:t (Mtime.add_span t span) in
          let orphan = after fin_timeout closed in
          match (e.fin, o.fin) with
          | Some (mine, _), Some (theirs, _) when theirs < mine -> `Release
          | _, Some (_, peer) when Mtime.is_later peer ~than:orphan -> `Release
          | _, Some (_, peer) ->
              let u = after t.time_wait peer in
              if Mtime.is_later u ~than:closed then `Time_wait u else `Release
          | _, None -> `Until orphan)

  (* Changes the state, and gives what the client and the server now do. *)
  let update t f =
    Mutex.protect t.lock @@ fun () ->
    f ();
    (decide t 0, decide t 1)

  let send_fin t i =
    let e = t.ends.(i) in
    if Option.is_none e.fin then begin
      t.seq <- t.seq + 1;
      e.fin <- Some (t.seq, now t)
    end

  let fin t side = update t (fun () -> send_fin t (index side))

  let close t side ~reset ~abort ~linger2 =
    let fin_timeout =
      match linger2 with
      | Some n -> Mtime.Span.(n * s)
      | None -> Ports.fin_timeout
    in
    update t (fun () ->
        let i = index side in
        send_fin t i;
        t.ends.(i).state <-
          (if abort then Dropped else Closed { at = now t; fin_timeout });
        if reset then t.reset <- true)

  let reset t = Mutex.protect t.lock (fun () -> t.reset <- true)
end

module Network = struct
  type tag = [ `Generic ]

  type t = {
    sw : Eio.Switch.t;
    os : os;
    hub : Listening_socket.hub;
    datagrams : Datagram_socket.hub;
    capacity : int -> int;
        (* What a connection queues each way, from the kernel's default. *)
    bounded : bool;
        (* The test gave [capacity]: it bounds what buffers set give too. *)
    sleep : (float -> unit) option;
    timer : Pipe.timer option;
  }

  let listening_handler = Eio.Net.Pi.listening_socket (module Listening_socket)

  (* One listener per address: there is no kernel to spread connections
     across several, so [reuse_port] is refused. A listener is closed by
     whichever of [sw] and the network's switch finishes first. *)
  let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
    if reuse_port then
      invalid_arg
        "Eio.Net.listen: reuse_port is true, which Eio_net_mem does not support";
    Eio.Switch.check sw;
    (* eio_posix removes a listener's file when [sw] finishes, and fails to
       for a path with a NUL, as there is none. *)
    (match addr with
    | `Unix path when embedded_nul ~os:Linux path ->
        Eio.Switch.on_release sw (fun () ->
            raise (Posix.bind_error (`Unlink path)))
    | _ -> ());
    let listener = Listening_socket.v t.hub ~reuse_addr ~backlog addr in
    (* The file a listener leaves goes when the listen's switch finishes, as
       eio_linux removes it then, closed by hand or not; eio_posix keeps the
       file of a listener closed by hand. This runs after the close the hooks
       below make. *)
    (match listener.addr with
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

  (* The options of a [connect], set in order before it binds and connects,
     as eio_posix sets them. *)
  let rec set_options opts : Eio.Net.Sockopt.settings -> unit = function
    | [] -> ()
    | (opt, v) :: rest ->
        Sockopt.set opts opt v;
        set_options opts rest

  (* Holds the address of a client that connects to [ip] at [dport] in the
     listeners' ports, until [release]: the loopback address of its
     destination's family, in the destination's form, or the one it is bound
     at. Its port is, on Linux, one free toward the destination if it binds
     no address, and the highest port nothing holds if bound at port 0, as
     listeners count up from the bottom; on macOS, the next port nothing
     holds. *)
  let hold t ?bind ((ip : Eio.Net.Ipaddr.v4v6), dport) =
    let toward = (List.hd (reaching ip), dport) in
    let mapped = String.length (ip :> string) = 16 && is_v4 ip in
    let reached = if mapped then map (source_to ip) else source_to ip in
    let bound, wildcard, port =
      match bind with
      | None -> (reached, false, 0)
      | Some (b, port) -> (b, same (unmap b) (any (unmap b)), port)
    in
    let ip = if wildcard then reached else bound in
    Eio.Mutex.use_rw ~protect:true t.hub.mutex @@ fun () ->
    let ports = t.hub.Listening_socket.ports in
    (* The bind comes first, at the address it names. *)
    if port <> 0 && Ports.in_use ports (bound, port) then Error `In_use
    else
      let toward =
        match (bind, t.os) with None, Linux -> Some toward | _ -> None
      in
      let port =
        match (toward, t.os) with
        | _ when port <> 0 -> Some port
        | Some toward, _ -> Ports.free_toward ports toward
        | None, Linux -> Ports.free_down ports
        | None, Macos -> Ports.free_next ports
      in
      match port with
      | None -> Error `No_port
      | Some port ->
          Ok
            ( (ip, port),
              Ports.add ports ~wildcard ~connected:true ?toward (ip, port) )

  (* Lets go of [h] now, or at [until]. Not cancelled, even while it waits
     for the mutex: a release that gave up would hold the port for good. *)
  let release_port t ?until ?(time_wait = false) port h =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
        let ports = t.hub.Listening_socket.ports in
        match until with
        | Some until -> Ports.set_until h ~time_wait until
        | None -> Ports.remove ports port h)

  (* A connect's socket has its destination's family, and fails to bind an
     address of another as Linux's bind(2) does: an IPv4 socket with
     EAFNOSUPPORT, an IPv6 or Unix-domain one with EINVAL. IPv6 against
     IPv4-mapped fails as on Linux. *)
  let check_bind ~os (bind_to : Eio.Net.Sockaddr.stream option)
      (addr : Eio.Net.Sockaddr.stream) =
    match (bind_to, addr) with
    | None, _ | Some (`Unix _), `Unix _ -> ()
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string)
           && os = Macos ->
        Option.iter fail (cross_family ~os b d)
    | Some _, _ when os = Macos -> fail Address_family
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string) -> (
        match cross_family ~os b d with
        (* Linux finds no route, or no scope, before the families apart. *)
        | Some Address_family when first_byte d = 0xff ->
            fail Network_unreachable
        | Some Address_family when needs_scope d -> fail Invalid_address
        | Some Address_family when off_loopback d -> fail Network_unreachable
        | code -> Option.iter fail code)
    | Some (`Tcp _), `Tcp (d, _) when String.length (d :> string) = 16 ->
        fail Invalid_address
    | Some (`Tcp _), `Unix _ -> fail Invalid_address
    | Some _, `Tcp _ -> fail Address_family

  (* The client's address as the listener at [listener] sees it: an IPv4
     client of an IPv6 listener is IPv4-mapped. A Unix-domain client binds no
     path. *)
  let peer_addr (listener : Eio.Net.Sockaddr.stream) (ip, port) :
      Eio.Net.Sockaddr.stream =
    match listener with
    | `Unix _ -> `Unix ""
    | `Tcp (lip, _) -> `Tcp (seen_by lip ip, port)

  (* Lets go of a client's address, now or at [until]. *)
  type release = ?until:Mtime.t -> ?time_wait:bool -> unit -> unit

  (* The listener a connect to [addr] reaches, if any. *)
  let find_listener t addr =
    Eio.Mutex.use_ro t.hub.mutex (fun () ->
        match addr with
        | `Tcp (ip, port) ->
            List.find_map
              (fun ip -> Hashtbl.find_opt t.hub.listeners (`Tcp (ip, port)))
              (reaching ip)
        | `Unix _ -> Hashtbl.find_opt t.hub.listeners addr)

  (* A connect reaches no broadcast or multicast address, nor one off
     loopback, and a link-local one needs a scope, as on Linux. *)
  let check_destination ~os = function
    | `Unix _ -> ()
    | `Tcp (d, _) ->
        let d' = unmap d in
        let group =
          if is_v4 d' then multicast_v4 d' || broadcast d' || lo_broadcast d'
          else first_byte d' = 0xff
        in
        if group then
          fail (if os = Macos then Address_family else Network_unreachable);
        if needs_scope d' then
          fail (if os = Macos then Host_unreachable else Invalid_address);
        if off_loopback d then fail Network_unreachable

  (* A connect bound at its own destination opens to itself, as Linux's
     simultaneous open does. *)
  let to_itself ~bind_to local = function
    | `Unix _ -> false
    | `Tcp (d, port) -> (
        match (bind_to, local) with
        | Some (`Tcp _), Some (ip, port') ->
            port = port' && same (unmap ip) (unmap (List.hd (reaching d)))
        | _ -> false)

  (* macOS refuses a connect to itself, and its loopback interface takes
     127.0.0.1 only: a connect to another address of 127.0.0.0/8 waits for a
     handshake that never comes, and times out after net.inet.tcp.keepinit. *)
  let check_macos t ~to_itself = function
    | `Unix _ -> ()
    | `Tcp _ when to_itself -> fail Invalid_address
    | `Tcp (d, _) when unanswered d -> (
        match t.sleep with
        | Some sleep ->
            sleep 75.;
            raise (Posix.timed_out `Connect)
        | None -> Eio.Fiber.await_cancel ())
    | `Tcp _ -> ()

  (* Its FIN reaches itself: it holds its address in TIME_WAIT. *)
  let connect_self t ~sw ~opts ~(release : release) =
    let capacity = t.capacity (Flow.default_capacity ~os:t.os ~local:false) in
    Sockopt.connect opts Client;
    let flow =
      Flow.self ~os:t.os ~timer:t.timer ~capacity ~bound:t.bounded ~opts
    in
    let ends = Ends.v t.hub.Listening_socket.ports in
    Flow.on_close flow (fun ~reset ~abort:_ ~linger2 ->
        ignore (Ends.close ends `Client ~reset ~abort:false ~linger2);
        match Ends.close ends `Server ~reset ~abort:false ~linger2 with
        | `Time_wait u, _ -> release ~until:u ~time_wait:true ()
        | `Until u, _ -> release ~until:u ()
        | (`Release | `Held), _ -> release ());
    Flow.close_with sw flow

  (* A connect no listener takes. *)
  let unheard t ~sw ~opts ~release ~to_itself = function
    | `Tcp _ when to_itself -> connect_self t ~sw ~opts ~release
    | `Tcp _ -> refused ()
    | `Unix "" -> fail Invalid_address
    | `Unix path -> (
        match Files.find t.hub.files ~stream:true path with
        | Some Datagram -> fail Wrong_socket_type
        | Some (Stale | Client | Listener) -> refused ()
        | None when Files.abstract path -> refused ()
        | None -> raise (no_file ()))

  (* The accepted socket holds the listener's port at the address the client
     reached, with the listener's [SO_REUSEADDR], and as one bound at a
     wildcard address if the listener is, as Linux's does. macOS's accepted
     socket has its listener's family. *)
  let child_hold t (listener : Listening_socket.t) = function
    | `Unix _ -> None
    | `Tcp (ip, port) ->
        let reached = List.hd (reaching ip) in
        let reached, wildcard =
          match listener.addr with
          | `Tcp (l, _) ->
              ( (if t.os = Macos then seen_by l reached else reached),
                same (unmap l) (any (unmap l)) )
          | `Unix _ -> (reached, false)
        in
        Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
            Some
              ( port,
                Ports.add t.hub.Listening_socket.ports ~reuse:listener.reuse
                  ~connected:true ~wildcard (reached, port) ))

  (* Lets go of the client's address, and of the accepted end's, as the
     connection's ends say. *)
  let watch t ~(release : release) ~child client server =
    let ends = Ends.v t.hub.Listening_socket.ports in
    let settle (c, s) =
      let apply f = function
        | `Held -> ()
        | `Release -> f None false
        | `Until u -> f (Some u) false
        | `Time_wait u -> f (Some u) true
      in
      apply (fun until time_wait -> release ?until ~time_wait ()) c;
      Option.iter
        (fun (port, h) ->
          apply
            (fun until time_wait -> release_port t ?until ~time_wait port h)
            s)
        child
    in
    Flow.on_close client (fun ~reset ~abort ~linger2 ->
        settle (Ends.close ends `Client ~reset ~abort ~linger2));
    Flow.on_close server (fun ~reset ~abort ~linger2 ->
        settle (Ends.close ends `Server ~reset ~abort ~linger2));
    Flow.on_fin client (fun () -> settle (Ends.fin ends `Client));
    Flow.on_fin server (fun () -> settle (Ends.fin ends `Server));
    ends

  (* A connection the listener queues. The switch takes the client before
     the listener sees it, so a finished switch queues nothing. *)
  let connect_listener t ~sw ~bind_to ~opts ~release addr local
      (listener : Listening_socket.t) =
    let unix = match addr with `Unix _ -> true | `Tcp _ -> false in
    let capacity = t.capacity (Flow.default_capacity ~os:t.os ~local:unix) in
    Sockopt.connect opts Client;
    let client, server =
      Flow.pair ~os:t.os ~local:unix ~timer:t.timer ~capacity ~bound:t.bounded
        ~client:opts
        ~server:(Sockopt.accepted listener.opts)
    in
    let child = child_hold t listener addr in
    let ends = watch t ~release ~child client server in
    let fail () =
      Ends.reset ends;
      Flow.close server;
      Flow.close client
    in
    let peer =
      match (local, bind_to) with
      | Some a, _ -> peer_addr listener.addr a
      | None, Some (`Unix path) -> `Unix path
      | None, _ -> `Unix ""
    in
    let flow = Flow.close_with sw client in
    match Listening_socket.offer listener (server, peer) with
    | `Queued -> flow
    | `Closed ->
        (* The listener closed while the connection waited for room. *)
        fail ();
        refused ()
    | exception e ->
        fail ();
        raise e

  (* A connect for a client that holds [local], if it is a TCP one. *)
  let connect_held t ~sw ~bind_to ~opts ~(release : release) addr local =
    let listener = find_listener t addr in
    check_destination ~os:t.os addr;
    let to_itself = to_itself ~bind_to local addr in
    if t.os = Macos then check_macos t ~to_itself addr;
    match listener with
    | None -> unheard t ~sw ~opts ~release ~to_itself addr
    | Some listener when Listening_socket.is_closed listener -> refused ()
    | Some listener ->
        connect_listener t ~sw ~bind_to ~opts ~release addr local listener

  (* The checks a connect's addresses meet before it binds, in Linux's
     order, or macOS's, and the destination it then has. macOS binds before
     it looks at the destination. *)
  let check_connect t ~bind_to ~opts addr =
    let os = t.os in
    (match (os, bind_to) with
    | Macos, Some (`Unix path) -> check_path ~os path
    | _ -> ());
    (match addr with `Unix path -> check_path ~os path | `Tcp _ -> ());
    let addr =
      match (addr, bind_to) with
      | `Tcp (d, port), Some (`Tcp (bound, _)) ->
          `Tcp (toward ~os ~bound d, port)
      | addr, _ -> addr
    in
    check_bind ~os bind_to addr;
    (match bind_to with
    | Some (`Tcp (ip, _)) ->
        check_local ~os ~tcp:true ip;
        Sockopt.bound opts ip
    | Some (`Unix path) -> check_path ~os path
    | None -> ());
    addr

  (* What a client holds: a port, a path, or nothing. *)
  let hold_local t ~bind_to addr =
    match (addr, bind_to) with
    | `Unix _, Some (`Unix path) ->
        if Files.bind t.hub.files path Client then `Path path
        else raise (address_in_use ())
    | `Unix _, _ -> `Nothing
    | `Tcp (ip, dport), bind -> (
        let bind = match bind with Some (`Tcp b) -> Some b | _ -> None in
        match hold t ?bind (ip, dport) with
        | Ok (local, h) -> `Port (local, h)
        (* With no port left, connect(2) fails with EADDRNOTAVAIL, and a
           bind(2) at port 0 before it with EADDRINUSE. *)
        | Error `No_port when Option.is_none bind -> fail Address_not_available
        | Error (`In_use | `No_port) -> raise (address_in_use ()))

  (* eio_posix and eio_linux keep a failed connect's socket on [sw], so the
     address it was bound at stays bound until [sw] finishes, as on Linux;
     macOS frees a TCP one at once. *)
  let release_failed t ~sw ~bind_to release =
    match bind_to with
    | None -> release ()
    | Some (`Tcp _) when t.os = Macos -> release ()
    | Some _ -> (
        try Eio.Switch.on_release sw (fun () -> release ())
        with Invalid_argument _ -> ())

  (* A connection is a pair of queues. A TCP client holds its address until
     its flow closes; one bound at an address a socket holds finds it in
     use. *)
  let connect t ~bind_to ~options ~sw addr =
    let opts = Sockopt.client ~os:t.os addr in
    set_options opts options;
    Eio.Switch.check sw;
    let addr = check_connect t ~bind_to ~opts addr in
    let bind_to =
      match bind_to with
      | Some (`Unix path) -> Some (`Unix (Files.bound t.hub.files path))
      | bind_to -> bind_to
    in
    let held = hold_local t ~bind_to addr in
    let local =
      match held with `Port (a, _) -> Some a | `Path _ | `Nothing -> None
    in
    (* Released by the connection's ends; a bound path leaves its file. *)
    let release ?until ?time_wait () =
      match held with
      | `Port ((_, port), h) -> release_port t ?until ?time_wait port h
      | `Path path -> Files.leave t.hub.files path Client
      | `Nothing -> ()
    in
    match connect_held t ~sw ~bind_to ~opts ~release addr local with
    | flow -> flow
    | exception e ->
        release_failed t ~sw ~bind_to release;
        raise e

  let datagram_handler = Eio.Net.Pi.datagram_socket (module Datagram_socket)

  let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
    if reuse_port then
      invalid_arg
        "Eio.Net.datagram_socket: reuse_port is true, which Eio_net_mem does \
         not support";
    let datagram = Datagram_socket.v t.datagrams ~reuse:reuse_addr addr in
    let socket = Eio.Resource.T (datagram, datagram_handler) in
    datagram.release <-
      Eio.Switch.on_release_cancellable sw (fun () -> Eio.Resource.close socket);
    socket

  (* Each address once for streams and once for datagrams, as getaddrinfo(3)
     lists one per socket type. The service is read first, as glibc reads it,
     and then the host: a number, "localhost" or the loopback addresses.
     Neither host nor service names nothing. *)
  let getaddrinfo t ~service node =
    let port =
      match t.os with
      | Linux -> Resolve.port service
      | Macos -> Resolve.port_macos service
    in
    match port with
    | _ when node = "" && service = "" -> []
    | None when t.os = Macos ->
        raise (Eio.Net.err (Address_lookup_failed NONAME))
    | None -> raise (Eio.Net.err (Address_lookup_failed SERVICE))
    | Some port ->
        List.concat_map
          (fun ip ->
            match t.os with
            | Linux -> [ `Tcp (ip, port); `Udp (ip, port) ]
            | Macos -> [ `Udp (ip, port); `Tcp (ip, port) ])
          (Resolve.host ~os:t.os node)

  (* An IPv4-compatible address, ::a.b.c.d past ::1, which glibc's
     getnameinfo names as its IPv4 address, and inet_ntop(3) writes dotted
     unless its first 16 of those bits are zero. *)
  let compatible (ip : Eio.Net.Ipaddr.v4v6) =
    let s = (ip :> string) in
    if
      String.length s = 16
      && String.for_all (( = ) '\000') (String.sub s 0 12)
      && Int32.unsigned_compare (String.get_int32_be s 12) 1l > 0
    then
      Some
        ( Eio.Net.Ipaddr.of_raw (String.sub s 12 4),
          s.[12] <> '\000' || s.[13] <> '\000' )
    else None

  (* The loopback addresses are "localhost", as /etc/hosts names them, and
     any other a number, as glibc gives one no name server knows; a service
     is a number, as there is no services database. There is no host name to
     give a Unix address. *)
  let getnameinfo t = function
    | `Unix _ when t.os = Macos ->
        (* macOS's getnameinfo(3) names no Unix address, and OCaml's [Unix]
           raises [Not_found] then. *)
        raise Not_found
    | `Tcp (ip, port) | `Udp (ip, port) ->
        let lo = Eio.Net.Ipaddr.V4.loopback in
        let host =
          match compatible ip with
          | Some (v4, _) when same v4 lo -> "localhost"
          | Some (v4, true) -> Fmt.str "::%a" Eio.Net.Ipaddr.pp v4
          | _ when same (unmap ip) lo || same ip Eio.Net.Ipaddr.V6.loopback ->
              "localhost"
          | _ -> Fmt.str "%a" Eio.Net.Ipaddr.pp ip
        in
        (host, string_of_int port)
    | `Unix path ->
        (* A C string: an abstract address's ends at its first NUL. *)
        let service =
          match String.index_opt path '\000' with
          | Some i -> String.sub path 0 i
          | None -> path
        in
        ("localhost", service)
end

let handler = Eio.Net.Pi.network (module Network)

let capacity_rule fn = function
  | None -> Fun.id
  | Some c when c >= 1 -> Fun.const c
  | Some c ->
      Fmt.invalid_arg "Eio_net_mem.%s: capacity is %d, it must be positive" fn c

let pair ?os ?capacity () =
  let os = Kernel.of_arg os in
  let end_ role =
    let opts = Sockopt.v ~os ~family:V4 (Stream Unconnected) in
    Sockopt.connect opts role;
    opts
  in
  let (_, client), (_, server) =
    Flow.pair ~os ~local:false ~timer:None
      ~capacity:
        (capacity_rule "pair" capacity (Flow.default_capacity ~os ~local:false))
      ~bound:(Option.is_some capacity) ~client:(end_ Client)
      ~server:(end_ Server)
  in
  (client, server)

let network ?os ~capacity ~bounded ?mono ~sw () =
  let os = Kernel.of_arg os in
  let files = Files.v () in
  let now = Option.map (fun mono () -> Eio.Time.Mono.now mono) mono in
  let sleep = Option.map (fun mono d -> Eio.Time.Mono.sleep mono d) mono in
  let timer = Option.map Pipe.timer mono in
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      ports = Ports.v ~os ?now ();
      files;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    {
      Datagram_socket.os;
      sockets = Hashtbl.create 8;
      ports = Ports.v ~os ();
      files;
      mutex = Mutex.create ();
      room = Eio.Condition.create ();
    }
  in
  Eio.Resource.T
    ( Network.{ sw; os; hub; datagrams; capacity; bounded; sleep; timer },
      handler )

let v ?os ?capacity ?mono ~sw () =
  network ?os
    ~capacity:(capacity_rule "v" capacity)
    ~bounded:(Option.is_some capacity) ?mono ~sw ()

module Private = struct
  let v ?os ~capacity ?mono ~sw () =
    network ?os ~capacity ~bounded:true ?mono ~sw ()

  let silence = Flow.silence
  let connection_id = Flow.connection
end
