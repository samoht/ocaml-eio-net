open! Hermetic

type Eio.Exn.Backend.t += Closed | No_listener | Address_in_use

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
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)

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
   which a write broadcasts after it has queued its bytes. A write never waits: a
   pipe holds whatever has been written to it. *)
module Pipe = struct
  type t = {
    mutex : Mutex.t;
    queued : Cstruct.t Queue.t;
    mutable head : int; (* Bytes of the first queued buffer already read. *)
    mutable closed : bool;
    readable : Eio.Condition.t;
  }

  let v () =
    {
      mutex = Mutex.create ();
      queued = Queue.create ();
      head = 0;
      closed = false;
      readable = Eio.Condition.create ();
    }

  (* The bytes are copied: the caller owns its buffers again once this returns. *)
  let write t bufs =
    let n =
      Mutex.protect t.mutex @@ fun () ->
      if t.closed then raise (reset_exn ());
      List.iter
        (fun b ->
          if Cstruct.length b > 0 then
            Queue.push (Cstruct.of_string (Cstruct.to_string b)) t.queued)
        bufs;
      Cstruct.lenv bufs
    in
    Eio.Condition.broadcast t.readable;
    n

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

  (* What is queued drains before a closed pipe reads as EOF. *)
  let read t dst =
    Eio.Condition.loop_no_mutex t.readable @@ fun () ->
    Mutex.protect t.mutex @@ fun () ->
    if not (Queue.is_empty t.queued) then Some (take t dst 0)
    else if t.closed then raise End_of_file
    else None

  let close t =
    Mutex.protect t.mutex (fun () -> t.closed <- true);
    Eio.Condition.broadcast t.readable
end

module Flow = struct
  module Impl = struct
    type tag = [ `Generic ]
    type t = { to_peer : Pipe.t; from_peer : Pipe.t }

    let single_write t bufs = Pipe.write t.to_peer bufs
    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    let single_read t buf =
      if Cstruct.length buf = 0 then 0 else Pipe.read t.from_peer buf

    let shutdown t = function
      | `Send -> Pipe.close t.to_peer
      | `Receive -> Pipe.close t.from_peer
      | `All ->
          Pipe.close t.to_peer;
          Pipe.close t.from_peer

    let close t = shutdown t `All
    let read_methods = []
    let setsockopt _ opt _ = Sockopt.stream opt

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  let v ~from_peer ~to_peer =
    Eio.Resource.T (Impl.{ from_peer; to_peer }, handler)

  let pair () =
    let to_client = Pipe.v () in
    let to_server = Pipe.v () in
    let client = v ~from_peer:to_client ~to_peer:to_server in
    let server = v ~from_peer:to_server ~to_peer:to_client in
    (client, server)
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
    incoming :
      (tag Eio.Net.stream_socket_ty Eio.Resource.t * Eio.Net.Sockaddr.stream)
      Eio.Stream.t;
    mutable closed : bool;
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

  let any ip =
    Eio.Net.Ipaddr.fold ip
      ~v4:(fun _ -> Eio.Net.Ipaddr.V4.any)
      ~v6:(fun _ -> Eio.Net.Ipaddr.V6.any)

  (* Two TCP addresses of one port overlap when they are equal or one is the
     wildcard address of the other's family, as bind(2) on Linux has it. *)
  let overlaps (a : Eio.Net.Sockaddr.stream) (b : Eio.Net.Sockaddr.stream) =
    let same (x : Eio.Net.Ipaddr.v4v6) (y : Eio.Net.Ipaddr.v4v6) =
      String.equal (x :> string) (y :> string)
    in
    match (a, b) with
    | `Tcp (ip, port), `Tcp (ip', port') ->
        port = port'
        && same (any ip) (any ip')
        && (same ip ip' || same ip (any ip) || same ip' (any ip'))
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
              closed = false;
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
    | Error `In_use -> raise (Eio.Exn.create (Eio.Exn.X Address_in_use))
    | Error `No_port ->
        failwith "eio-net.mem: every port of 49152-65535 has a listener"

  let accept t ~sw =
    if t.closed then invalid_arg "accept on closed in-memory listener";
    let flow, client_addr = Eio.Stream.take t.incoming in
    Eio.Switch.on_release sw (fun () -> Eio.Flow.close flow);
    (flow, client_addr)

  let close t =
    if not t.closed then (
      t.closed <- true;
      List.iter Eio.Switch.remove_hook t.hooks;
      t.hooks <- [];
      (* A path taken over is its new listener's to free. *)
      Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
          match Hashtbl.find_opt t.hub.listeners t.addr with
          | Some held when held == t -> Hashtbl.remove t.hub.listeners t.addr
          | Some _ | None -> ()))

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
    mutex : Mutex.t;
  }

  and t = {
    hub : hub;
    addr : Eio.Net.Sockaddr.datagram;
    lock : Mutex.t;
    queued : (Eio.Net.Sockaddr.datagram * string) Queue.t;
    readable : Eio.Condition.t;
    mutable closed : bool;
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

  let v hub addr =
    Mutex.protect hub.mutex @@ fun () ->
    let addr : Eio.Net.Sockaddr.datagram =
      match addr with
      | `UdpV4 -> `Udp (Eio.Net.Ipaddr.V4.loopback, free_port hub)
      | `UdpV6 -> `Udp (Eio.Net.Ipaddr.V6.loopback, free_port hub)
      | `Udp (ip, 0) -> `Udp (ip, free_port hub)
      | (`Udp _ | `Unix _) as addr -> addr
    in
    if Hashtbl.mem hub.sockets addr then
      Fmt.invalid_arg "eio-net.mem: %a already has a datagram socket"
        Eio.Net.Sockaddr.pp addr;
    let t =
      {
        hub;
        addr;
        lock = Mutex.create ();
        queued = Queue.create ();
        readable = Eio.Condition.create ();
        closed = false;
      }
    in
    Hashtbl.replace hub.sockets addr t;
    t

  (* The socket bound at [addr], or at the wildcard address of its port. *)
  let find hub (addr : Eio.Net.Sockaddr.datagram) =
    Mutex.protect hub.mutex @@ fun () ->
    match Hashtbl.find_opt hub.sockets addr with
    | Some _ as found -> found
    | None -> (
        match addr with
        | `Udp (ip, port) ->
            let any =
              Eio.Net.Ipaddr.fold ip
                ~v4:(fun _ -> Eio.Net.Ipaddr.V4.any)
                ~v6:(fun _ -> Eio.Net.Ipaddr.V6.any)
            in
            Hashtbl.find_opt hub.sockets (`Udp (any, port))
        | `Unix _ -> None)

  (* A datagram to an address no socket binds is lost, as UDP loses it, and
     the sender is not told. *)
  let send t ?dst bufs =
    if t.closed then invalid_arg "eio-net.mem: send on a closed datagram socket";
    let dst =
      match dst with
      | Some dst -> dst
      | None -> invalid_arg "eio-net.mem: a datagram send needs a destination"
    in
    match find t.hub dst with
    | None -> ()
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
      Mutex.protect t.hub.mutex (fun () -> Hashtbl.remove t.hub.sockets t.addr);
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

  (* A connection is a pair of queues: there is no local address to bind, so a
     caller asking for one is refused. *)
  let connect t ~bind_to ~options ~sw addr =
    if Option.is_some bind_to then
      invalid_arg "eio-net.mem does not support binding a local address";
    check_options options;
    let listener =
      Eio.Mutex.use_ro t.hub.mutex (fun () ->
          match Hashtbl.find_opt t.hub.listeners addr with
          | Some _ as listener -> listener
          | None ->
              let wildcard =
                match addr with
                | `Tcp (ip, port) ->
                    Some
                      (Eio.Net.Ipaddr.fold ip
                         ~v4:(fun _ -> `Tcp (Eio.Net.Ipaddr.V4.any, port))
                         ~v6:(fun _ -> `Tcp (Eio.Net.Ipaddr.V6.any, port)))
                | `Unix _ -> None
              in
              Option.bind wildcard (Hashtbl.find_opt t.hub.listeners))
    in
    match listener with
    | None -> raise (Eio.Net.err (Connection_failure (Refused No_listener)))
    | Some listener ->
        let client, server = Flow.pair () in
        Eio.Switch.on_release sw (fun () -> Eio.Flow.close client);
        Eio.Stream.add listener.incoming (server, `Unix "in-memory-client");
        client

  let datagram_handler = Eio.Net.Pi.datagram_socket (module Datagram_socket)

  (* One socket per address: there is no kernel to spread datagrams across
     several, so [reuse_port] is refused. *)
  let datagram_socket t ~reuse_addr:_ ~reuse_port ~sw addr =
    if reuse_port then
      invalid_arg "eio-net.mem does not support SO_REUSEPORT datagram sockets";
    let socket =
      Eio.Resource.T (Datagram_socket.v t.datagrams addr, datagram_handler)
    in
    Eio.Switch.on_release sw (fun () -> Eio.Resource.close socket);
    socket

  let getaddrinfo _ ~service node =
    match int_of_string_opt service with
    | Some port when node = "127.0.0.1" || node = "localhost" ->
        [ `Tcp (Eio.Net.Ipaddr.V4.loopback, port) ]
    | _ -> []

  let getnameinfo _ = function
    | `Tcp (_, port) | `Udp (_, port) -> ("localhost", string_of_int port)
    | `Unix path -> ("localhost", path)
end

let handler = Eio.Net.Pi.network (module Network)
let pipe () = Flow.pair ()

let v ~sw () =
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      mutex = Eio.Mutex.create ();
    }
  in
  let datagrams =
    { Datagram_socket.sockets = Hashtbl.create 8; mutex = Mutex.create () }
  in
  Eio.Resource.T (Network.{ sw; hub; datagrams }, handler)
