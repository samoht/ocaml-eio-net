type Eio.Exn.Backend.t += Closed | No_listener

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Closed ->
        Format.pp_print_string ppf "Closed (eio-net.mem)";
        true
    | No_listener ->
        Format.pp_print_string ppf "No listener bound (eio-net.mem)";
        true
    | _ -> false)

let reset_exn () = Eio.Net.err (Eio.Net.Connection_reset Closed)

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
    let setsockopt _ _ _ = ()

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
  }

  let v hub addr =
    let t = { hub; addr; incoming = Eio.Stream.create 128; closed = false } in
    Eio.Mutex.use_rw ~protect:true hub.mutex (fun () ->
        Hashtbl.replace hub.listeners addr t);
    t

  let accept t ~sw =
    if t.closed then invalid_arg "accept on closed in-memory listener";
    let flow, client_addr = Eio.Stream.take t.incoming in
    Eio.Switch.on_release sw (fun () -> Eio.Flow.close flow);
    (flow, client_addr)

  let close t =
    if not t.closed then (
      t.closed <- true;
      Eio.Mutex.use_rw ~protect:true t.hub.mutex (fun () ->
          Hashtbl.remove t.hub.listeners t.addr))

  let listening_addr t = t.addr
  let setsockopt _ _ _ = ()

  let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
   fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
end

module Network = struct
  type tag = [ `Generic ]
  type t = { sw : Eio.Switch.t; hub : Listening_socket.hub }

  let listening_handler = Eio.Net.Pi.listening_socket (module Listening_socket)

  let listen t ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw:_ addr =
    let socket =
      Eio.Resource.T (Listening_socket.v t.hub addr, listening_handler)
    in
    Eio.Switch.on_release t.sw (fun () -> Eio.Resource.close socket);
    socket

  (* An option is ignored only where these queues already provide what it asks
     for, so ignoring it yields the requested behaviour rather than withholding
     it: [TCP_NODELAY] and [TCP_QUICKACK]. Every other option would change what
     a caller observes and is refused by name; post-connect [setsockopt] on a
     mem socket remains a blanket no-op. *)
  let ignored_hint : type a. a Eio.Net.Sockopt.t -> bool = function
    | Eio.Net.Sockopt.TCP_NODELAY -> true
    | Eio.Net.Sockopt.TCP_QUICKACK -> true
    | _ -> false

  let rec check_options (options : Eio.Net.Sockopt.settings) =
    let open Eio.Net.Sockopt in
    match options with
    | [] -> ()
    | (opt, _) :: rest ->
        if not (ignored_hint opt) then
          Fmt.invalid_arg "eio-net.mem does not support socket option %a"
            Eio.Net.Sockopt.pp opt;
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

  let datagram_socket _ ~reuse_addr:_ ~reuse_port:_ ~sw:_ _ =
    invalid_arg "eio-net.mem does not support datagram sockets"

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
  Eio.Resource.T (Network.{ sw; hub }, handler)
