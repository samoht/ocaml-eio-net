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

(* A Unix path no socket binds has no file, as Eio's POSIX backend removes it
   when the socket's switch finishes. *)
let no_file () = Eio.Fs.err (Not_found No_listener)

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

(* One direction of a connection: the bytes written and not yet read. Its two
   ends may be used from two domains, so its state is guarded by a mutex and a
   read that finds nothing waits on a condition it registers for before it looks,
   which a write broadcasts after it has queued its bytes. A write takes what
   fits under [capacity] and waits on [writable] while nothing does, which a
   read broadcasts.

   The ends follow TCP: [shut] is the writer's FIN, after which the reader
   reads end-of-file, or a reset when [reset] says one came. A reset is
   reported once, by a read or by a write that [refused] fails, as a socket
   clears its pending error; reads then return end-of-file. [gone] says the
   reader has closed: the first write to it is lost and answered with a reset,
   which [refused] then makes every later write raise; on a Unix-domain
   connection, [local], the first write raises it, as EPIPE. [deaf] says the reader
   shut its receive side: writes are lost, as Linux takes and drops them. *)
module Pipe = struct
  type t = {
    mutex : Mutex.t;
    queued : Cstruct.t Queue.t;
    mutable head : int; (* Bytes of the first queued buffer already read. *)
    mutable shut : bool;
    mutable reset : bool;
    mutable gone : bool;
    mutable refused : bool;
    mutable deaf : bool;
    local : bool;
    capacity : int;
    mutable size : int; (* Bytes queued and not yet read. *)
    readable : Eio.Condition.t;
    writable : Eio.Condition.t;
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
      deaf = false;
      capacity;
      size = 0;
      readable = Eio.Condition.create ();
      writable = Eio.Condition.create ();
    }

  let update t f =
    Mutex.protect t.mutex (fun () -> f t);
    Eio.Condition.broadcast t.readable;
    Eio.Condition.broadcast t.writable

  (* The reset reaches the reader once it has read what is queued. *)
  let reset t = update t (fun t -> t.reset <- true)

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

  let write t ~back bufs =
    let n = Cstruct.lenv bufs in
    let result =
      Eio.Condition.loop_no_mutex t.writable @@ fun () ->
      Mutex.protect t.mutex @@ fun () ->
      if t.shut then raise (reset_exn ());
      if t.refused then Some `Refused
      else if t.gone then begin
        t.refused <- true;
        if t.local then raise (reset_exn ());
        Some (`Lost n)
      end
      else if t.deaf then Some (`Dropped n)
      else if n = 0 then Some (`Queued 0)
      else if t.size >= t.capacity then None
      else
        let k = min n (t.capacity - t.size) in
        push t bufs k;
        t.size <- t.size + k;
        Some (`Queued k)
    in
    match result with
    | `Refused ->
        consume_reset back;
        raise (reset_exn ())
    | `Lost n ->
        reset back;
        n
    | `Dropped n -> n
    | `Queued k ->
        Eio.Condition.broadcast t.readable;
        k

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
  let read t dst =
    let n =
      Eio.Condition.loop_no_mutex t.readable @@ fun () ->
      Mutex.protect t.mutex @@ fun () ->
      if not (Queue.is_empty t.queued) then begin
        let n = take t dst 0 in
        t.size <- t.size - n;
        Some n
      end
      else if t.reset then begin
        t.reset <- false;
        t.shut <- true;
        raise (reset_exn ())
      end
      else if t.shut || t.deaf then raise End_of_file
      else None
    in
    Eio.Condition.broadcast t.writable;
    n

  let shut t = update t (fun t -> t.shut <- true)

  (* The reader shuts its receive side: what is queued is dropped. *)
  let deafen t =
    update t (fun t ->
        Queue.clear t.queued;
        t.head <- 0;
        t.size <- 0;
        t.deaf <- true)

  (* The reader closes: bytes it left unread answer the writer with a reset,
     which refuses its writes at once. *)
  let leave t =
    let unread =
      Mutex.protect t.mutex @@ fun () ->
      let unread = not (Queue.is_empty t.queued) in
      Queue.clear t.queued;
      t.head <- 0;
      t.size <- 0;
      t.gone <- true;
      if unread then t.refused <- true;
      unread
    in
    Eio.Condition.broadcast t.writable;
    unread
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
      | `Send -> Pipe.shut t.to_peer
      | `Receive -> Pipe.deafen t.from_peer
      | `All ->
          Pipe.shut t.to_peer;
          Pipe.deafen t.from_peer

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
        Pipe.shut t.to_peer
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
        { from_peer; to_peer; closed = false; release = Eio.Switch.null_hook }
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
  let abort ((impl, _) : t) = Impl.close_with ~abort:true impl
end

module Listening_socket = struct
  type tag = [ `Generic ]

  type hub = {
    listeners : (Eio.Net.Sockaddr.stream, t) Hashtbl.t;
    mutex : Eio.Mutex.t;
  }

  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.stream;
    incoming : (Flow.t * Eio.Net.Sockaddr.stream) Eio.Stream.t;
    closed : bool Atomic.t;
    mutable hooks : Eio.Switch.hook list;
  }

  (* The lowest port of the dynamic range (RFC 6335, section 6) that no
     listener holds, at any address. *)
  let free_port hub =
    let held = Hashtbl.create (Hashtbl.length hub.listeners) in
    Hashtbl.iter
      (fun addr _ ->
        match addr with
        | `Tcp (_, port) -> Hashtbl.replace held port ()
        | `Unix _ -> ())
      hub.listeners;
    let rec find port =
      if port > 65535 then None
      else if Hashtbl.mem held port then find (port + 1)
      else Some port
    in
    find 49152

  let overlaps (a : Eio.Net.Sockaddr.stream) (b : Eio.Net.Sockaddr.stream) =
    match (a, b) with
    | `Tcp a, `Tcp b -> ip_overlaps a b
    | `Unix path, `Unix path' -> String.equal path path'
    | `Tcp _, `Unix _ | `Unix _, `Tcp _ -> false

  let in_use hub addr =
    Hashtbl.fold
      (fun held _ in_use -> in_use || overlaps addr held)
      hub.listeners false

  (* A Unix-domain path is taken over with [reuse_addr], as Eio's POSIX backend
     removes the socket file there before it binds; the listener that held it
     stays open, unreachable. *)
  let bind hub ~reuse_addr addr =
    let addr =
      match addr with
      | `Tcp (ip, 0) -> Option.map (fun port -> `Tcp (ip, port)) (free_port hub)
      | addr -> Some addr
    in
    match addr with
    | None -> Error `No_port
    | Some addr ->
        let taken_over =
          match addr with `Unix _ -> reuse_addr | `Tcp _ -> false
        in
        if (not taken_over) && in_use hub addr then Error `In_use
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
      (* A path taken over is its new listener's to free. *)
      Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
          match Hashtbl.find_opt t.hub.listeners t.addr with
          | Some held when held == t -> Hashtbl.remove t.hub.listeners t.addr
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

  (* [files] holds the paths of closed Unix-domain sockets, whose files Eio's
     POSIX backend leaves behind. *)
  type hub = {
    sockets : (Eio.Net.Sockaddr.datagram, t) Hashtbl.t;
    files : (string, unit) Hashtbl.t;
    mutex : Mutex.t;
  }

  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : (Eio.Net.Sockaddr.datagram * string) Queue.t;
    readable : Eio.Condition.t;
    mutable closed : bool;
    mutable release : Eio.Switch.hook;
  }

  (* The lowest port of the dynamic range (RFC 6335, section 6) that no
     datagram socket holds, at any address. *)
  let free_port hub =
    let held port =
      Hashtbl.fold
        (fun addr _ held ->
          held || match addr with `Udp (_, p) -> p = port | `Unix _ -> false)
        hub.sockets false
    in
    let rec find port =
      if port > 65535 then
        failwith "eio-net.mem: every port of 49152-65535 has a datagram socket"
      else if held port then find (port + 1)
      else port
    in
    find 49152

  let overlaps (a : Eio.Net.Sockaddr.datagram) (b : Eio.Net.Sockaddr.datagram) =
    match (a, b) with
    | `Udp a, `Udp b -> ip_overlaps a b
    | `Unix path, `Unix path' -> String.equal path path'
    | `Udp _, `Unix _ | `Unix _, `Udp _ -> false

  let v hub addr =
    Mutex.protect hub.mutex @@ fun () ->
    let addr : Eio.Net.Sockaddr.datagram =
      match addr with
      | `UdpV4 -> `Udp (Eio.Net.Ipaddr.V4.loopback, free_port hub)
      | `UdpV6 -> `Udp (Eio.Net.Ipaddr.V6.loopback, free_port hub)
      | `Udp (ip, 0) -> `Udp (ip, free_port hub)
      | (`Udp _ | `Unix _) as addr -> addr
    in
    let file =
      match addr with
      | `Unix path -> Hashtbl.mem hub.files path
      | `Udp _ -> false
    in
    if
      file
      || Hashtbl.fold
           (fun held _ in_use -> in_use || overlaps addr held)
           hub.sockets false
    then raise (address_in_use ());
    let t =
      {
        hub;
        addr;
        lock = Mutex.create ();
        queued = Queue.create ();
        readable = Eio.Condition.create ();
        closed = false;
        release = Eio.Switch.null_hook;
      }
    in
    Hashtbl.replace hub.sockets addr t;
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

  (* A datagram to an address no socket binds is lost, as UDP loses it, and
     the sender is not told; at a Unix path, the send finds no file, or one
     that no socket holds any more and refuses it. *)
  let send t ?dst bufs =
    if t.closed then invalid_arg "eio-net.mem: send on a closed datagram socket";
    let dst =
      match dst with
      | Some dst -> dst
      | None -> raise (Eio.Exn.create (Eio.Exn.X No_destination))
    in
    if Cstruct.lenv bufs > max_length dst then
      raise (Eio.Exn.create (Eio.Exn.X Message_too_long));
    (match dst with
    | `Udp (_, 0) -> raise (Eio.Exn.create (Eio.Exn.X Invalid_port))
    | `Udp _ | `Unix _ -> ());
    match find t.hub dst with
    | None -> (
        match dst with
        | `Unix path ->
            if
              Mutex.protect t.hub.mutex (fun () -> Hashtbl.mem t.hub.files path)
            then raise (Eio.Net.err (Connection_failure (Refused No_listener)))
            else raise (no_file ())
        | `Udp _ -> ())
    | Some peer ->
        let delivered =
          Mutex.protect peer.lock @@ fun () ->
          if not peer.closed then
            Queue.push
              (t.addr, Cstruct.to_string (Cstruct.concat bufs))
              peer.queued;
          not peer.closed
        in
        if delivered then Eio.Condition.broadcast peer.readable

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
          | `Unix path -> Hashtbl.replace t.hub.files path ()
          | `Udp _ -> ());
      Eio.Condition.broadcast t.readable
    end

  let shutdown _ _ = ()
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
    next_port : int Atomic.t;
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

  (* The address of a client that connects to [ip]: the address its
     destination reaches, in the destination's family, at the next port of the
     dynamic range, counted down from the top of the range as listeners count
     up from its bottom. *)
  let client_addr t ?bind ip =
    let reached = List.hd (reaching ip) in
    let mapped = String.length (ip :> string) = 16 && is_v4 ip in
    let reached = if mapped then map reached else reached in
    let next () =
      let n = Atomic.fetch_and_add t.next_port 1 in
      65535 - (n mod (65535 - 49152 + 1))
    in
    match bind with
    | None -> (reached, next ())
    | Some (b, port) ->
        let b = if same (unmap b) (any (unmap b)) then reached else b in
        (b, if port = 0 then next () else port)

  (* A connect bound to an address of the other family, IPv6 against
     IPv4-mapped, fails as on Linux. *)
  let check_bind (bind_to : Eio.Net.Sockaddr.stream option)
      (addr : Eio.Net.Sockaddr.stream) =
    let fail code = raise (Eio.Exn.create (Eio.Exn.X code)) in
    match (bind_to, addr) with
    | None, _ | Some (`Unix _), `Unix _ -> ()
    | Some (`Tcp (b, _)), `Tcp (d, _)
      when String.length (b :> string) = String.length (d :> string) ->
        if String.length (b :> string) = 16 then
          if is_v4 b && not (is_v4 d) then fail Address_family
          else if
            (not (is_v4 b)) && (not (same b Eio.Net.Ipaddr.V6.any)) && is_v4 d
          then fail Network_unreachable
    | Some _, _ ->
        invalid_arg
          "eio-net.mem: bind_to is not of the connect address's family"

  (* The client's address as the listener at [listener] sees it: an IPv4
     client of an IPv6 listener is IPv4-mapped. A Unix-domain client binds no
     path. *)
  let peer_addr (listener : Eio.Net.Sockaddr.stream) (ip, port) :
      Eio.Net.Sockaddr.stream =
    match listener with
    | `Unix _ -> `Unix ""
    | `Tcp (lip, _) ->
        let ip =
          if String.length (lip :> string) = 4 then unmap ip
          else if is_v4 ip then map (unmap ip)
          else ip
        in
        `Tcp (ip, port)

  (* A connection is a pair of queues. A local address it is bound at, at a
     port a listener holds, is in use. *)
  let connect t ~bind_to ~options ~sw addr =
    check_options options;
    check_bind bind_to addr;
    (match bind_to with
    | Some (`Tcp (_, port) as local)
      when port <> 0
           && Eio.Mutex.use_ro t.hub.mutex (fun () ->
               Listening_socket.in_use t.hub local) ->
        raise (address_in_use ())
    | _ -> ());
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
    | None, `Unix _ -> raise (no_file ())
    | None, `Tcp _ -> refused ()
    | Some listener, _ when Atomic.get listener.closed -> refused ()
    | Some listener, _ ->
        let local = match addr with `Unix _ -> true | `Tcp _ -> false in
        let client, server = Flow.pair ~local ~capacity:t.capacity in
        let peer =
          match (addr, bind_to) with
          | `Tcp (ip, _), Some (`Tcp bind) ->
              peer_addr listener.addr (client_addr t ~bind ip)
          | `Tcp (ip, _), _ -> peer_addr listener.addr (client_addr t ip)
          | `Unix _, Some (`Unix path) -> `Unix path
          | `Unix _, _ -> `Unix ""
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
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    {
      Datagram_socket.sockets = Hashtbl.create 8;
      files = Hashtbl.create 8;
      mutex = Mutex.create ();
    }
  in
  Eio.Resource.T
    ( Network.{ sw; hub; datagrams; capacity; next_port = Atomic.make 0 },
      handler )
