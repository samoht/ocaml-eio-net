module Flow = struct
  module W = Eio.Buf_write

  let rec takev len = function
    | [] -> []
    | x :: xs ->
        if len = 0 then []
        else if Cstruct.length x >= len then [ Cstruct.sub x 0 len ]
        else x :: takev (len - Cstruct.length x) xs

  module Impl = struct
    type tag = [ `Generic ]
    type t = { to_peer : W.t; from_peer : W.t }

    let single_write t bufs =
      let bufs = takev (Cstruct.lenv bufs) bufs in
      List.iter (W.cstruct t.to_peer) bufs;
      Cstruct.lenv bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

    let single_read t buf =
      let batch = W.await_batch t.from_peer in
      let got, _ = Cstruct.fillv ~src:batch ~dst:buf in
      W.shift t.from_peer got;
      got

    let shutdown t = function
      | `Send -> W.close t.to_peer
      | `Receive -> W.close t.from_peer
      | `All ->
          W.close t.to_peer;
          W.close t.from_peer

    let close t = shutdown t `All
    let read_methods = []

    (* An in-memory pipe has no kernel socket, so options do not apply:
       setsockopt is a no-op (best effort) and getsockopt is unsupported. *)
    let setsockopt _ _ _ = ()

    let getsockopt : type a. t -> a Eio.Net.Sockopt.t -> a =
     fun _ _ -> invalid_arg "eio-net.mem: getsockopt is not supported"
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  let v ~from_peer ~to_peer =
    Eio.Resource.T (Impl.{ from_peer; to_peer }, handler)

  let pair () =
    let to_client = W.create 0x10000 in
    let to_server = W.create 0x10000 in
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

  let connect t ~sw:_ addr =
    let listener =
      Eio.Mutex.use_ro t.hub.mutex (fun () ->
          Hashtbl.find_opt t.hub.listeners addr)
    in
    match listener with
    | None -> invalid_arg "no in-memory listener bound"
    | Some listener ->
        let client, server = Flow.pair () in
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

let v ~sw () =
  let hub =
    {
      Listening_socket.listeners = Hashtbl.create 8;
      mutex = Eio.Mutex.create ();
    }
  in
  Eio.Resource.T (Network.{ sw; hub }, handler)
