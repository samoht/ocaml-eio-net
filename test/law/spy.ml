type call = Law.interface * string

let equal_call (a : call) b = a = b

let operations =
  [
    (`Network, "Eio.Net.listen");
    (`Network, "Eio.Net.connect");
    (`Network, "Eio.Net.datagram_socket");
    (`Network, "Eio.Net.getaddrinfo");
    (`Network, "Eio.Net.getnameinfo");
    (`Listening_socket, "Eio.Net.accept");
    (`Listening_socket, "Eio.Net.listening_addr");
    (`Listening_socket, "Eio.Net.setsockopt");
    (`Listening_socket, "Eio.Net.getsockopt");
    (`Listening_socket, "Eio.Resource.close");
    (`Listening_socket, "Eio_unix.Resource.fd_opt");
    (`Stream_socket, "Eio.Flow.Pi.SOURCE.read_methods");
    (`Stream_socket, "Eio.Flow.single_read");
    (`Stream_socket, "Eio.Flow.single_write");
    (`Stream_socket, "Eio.Flow.copy");
    (`Stream_socket, "Eio.Flow.shutdown");
    (`Stream_socket, "Eio.Net.setsockopt");
    (`Stream_socket, "Eio.Net.getsockopt");
    (`Stream_socket, "Eio.Resource.close");
    (`Stream_socket, "Eio_unix.Resource.fd_opt");
    (`Datagram_socket, "Eio.Net.send");
    (`Datagram_socket, "Eio.Net.recv");
    (`Datagram_socket, "Eio.Flow.shutdown");
    (`Datagram_socket, "Eio.Net.setsockopt");
    (`Datagram_socket, "Eio.Net.getsockopt");
    (`Datagram_socket, "Eio.Resource.close");
    (`Datagram_socket, "Eio_unix.Resource.fd_opt");
  ]

type t = {
  mutable calls : call list;
  mutable made : int;
  mutable opened : (int * string) list;
}

let call t c = t.calls <- c :: t.calls
let calls t = List.rev t.calls
let clear t = t.calls <- []
let opened t = List.rev_map (fun (n, kind) -> Fmt.str "%s %d" kind n) t.opened

(* A new socket of [kind], open until [close] or the end of [sw]. *)
let made t ~sw kind =
  t.made <- t.made + 1;
  let n = t.made in
  t.opened <- (n, kind) :: t.opened;
  let close () = t.opened <- List.remove_assoc n t.opened in
  Eio.Switch.on_release sw close;
  close

type stream = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t

let shutdown_of (Eio.Resource.T (t, ops)) cmd =
  let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
  X.shutdown t cmd

module Stream = struct
  type nonrec t = { spy : t; base : stream; close : unit -> unit }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let c t name = call t.spy (`Stream_socket, name)
    let read_methods = []

    let single_read t buf =
      c t "Eio.Flow.single_read";
      Eio.Flow.single_read t.base buf

    let single_write t bufs =
      c t "Eio.Flow.single_write";
      Eio.Flow.single_write t.base bufs

    let copy t ~src =
      c t "Eio.Flow.copy";
      Eio.Flow.copy src t.base

    let shutdown t cmd =
      c t "Eio.Flow.shutdown";
      Eio.Flow.shutdown t.base cmd

    let setsockopt t o v =
      c t "Eio.Net.setsockopt";
      Eio.Net.setsockopt t.base o v

    let getsockopt t o =
      c t "Eio.Net.getsockopt";
      Eio.Net.getsockopt t.base o

    let close t =
      c t "Eio.Resource.close";
      t.close ();
      Eio.Resource.close t.base
  end

  let handler = Eio.Net.Pi.stream_socket (module Impl)

  let v spy ~sw base : stream =
    Eio.Resource.T ({ spy; base; close = made spy ~sw "stream" }, handler)
end

module Datagram = struct
  type socket = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t
  type nonrec t = { spy : t; base : socket; close : unit -> unit }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let c t name = call t.spy (`Datagram_socket, name)

    let send t ?dst bufs =
      c t "Eio.Net.send";
      Eio.Net.send t.base ?dst bufs

    let recv t buf =
      c t "Eio.Net.recv";
      Eio.Net.recv t.base buf

    let shutdown t cmd =
      c t "Eio.Flow.shutdown";
      shutdown_of t.base cmd

    let setsockopt t o v =
      c t "Eio.Net.setsockopt";
      Eio.Net.setsockopt t.base o v

    let getsockopt t o =
      c t "Eio.Net.getsockopt";
      Eio.Net.getsockopt t.base o

    let close t =
      c t "Eio.Resource.close";
      t.close ();
      Eio.Resource.close t.base
  end

  let handler = Eio.Net.Pi.datagram_socket (module Impl)

  let v spy ~sw base : socket =
    Eio.Resource.T ({ spy; base; close = made spy ~sw "datagram" }, handler)
end

module Listener = struct
  type nonrec t = {
    spy : t;
    base : [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
    close : unit -> unit;
  }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let c t name = call t.spy (`Listening_socket, name)

    let accept t ~sw =
      c t "Eio.Net.accept";
      let s, addr = Eio.Net.accept ~sw t.base in
      (Stream.v t.spy ~sw (s :> stream), addr)

    let listening_addr t =
      c t "Eio.Net.listening_addr";
      Eio.Net.listening_addr t.base

    let setsockopt t o v =
      c t "Eio.Net.setsockopt";
      Eio.Net.setsockopt t.base o v

    let getsockopt t o =
      c t "Eio.Net.getsockopt";
      Eio.Net.getsockopt t.base o

    let close t =
      c t "Eio.Resource.close";
      t.close ();
      Eio.Resource.close t.base
  end

  let handler = Eio.Net.Pi.listening_socket (module Impl)
end

module Network = struct
  type nonrec t = { spy : t; base : [ `Generic ] Eio.Net.ty Eio.Resource.t }

  module Impl = struct
    type tag = [ `Generic ]
    type nonrec t = t

    let c t name = call t.spy (`Network, name)

    let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
      c t "Eio.Net.listen";
      let base =
        Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.base addr
      in
      Eio.Resource.T
        ( {
            Listener.spy = t.spy;
            base :> [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t;
            close = made t.spy ~sw "listener";
          },
          Listener.handler )

    let connect t ~bind_to ~options ~sw addr =
      c t "Eio.Net.connect";
      Stream.v t.spy ~sw
        (Eio.Net.connect ?bind_to ~options ~sw t.base addr :> stream)

    let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
      c t "Eio.Net.datagram_socket";
      Datagram.v t.spy ~sw
        (Eio.Net.datagram_socket ~reuse_addr ~reuse_port ~sw t.base addr
          :> Datagram.socket)

    let getaddrinfo t ~service node =
      c t "Eio.Net.getaddrinfo";
      Eio.Net.getaddrinfo ~service t.base node

    let getnameinfo t addr =
      c t "Eio.Net.getnameinfo";
      Eio.Net.getnameinfo t.base addr
  end

  let handler = Eio.Net.Pi.network (module Impl)
end

let v base =
  let spy = { calls = []; made = 0; opened = [] } in
  (spy, Eio.Resource.T ({ Network.spy; base }, Network.handler))
