open! Hermetic

type kind = [ `Connect | `Accept | `Read | `Write | `Recv ]
type side = [ `Client | `Server ]
type family = [ `Inet | `Unix ]

type t = {
  index : int;
  kind : kind;
  socket : int;
  side : side;
  family : family;
  offset : int;
  length : int;
}

let kind_name = function
  | `Connect -> "connect"
  | `Accept -> "accept"
  | `Read -> "read"
  | `Write -> "write"
  | `Recv -> "recv"

let side_name = function `Client -> "c" | `Server -> "s"

let family_of_stream : Eio.Net.Sockaddr.stream -> family = function
  | `Unix _ -> `Unix
  | `Tcp _ -> `Inet

let family_of_datagram :
    [ Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6 ] -> family = function
  | `Unix _ -> `Unix
  | `Udp _ | `UdpV4 | `UdpV6 -> `Inet

let is_bytes = function
  | `Read | `Write -> true
  | `Connect | `Accept | `Recv -> false

let pp ppf t =
  if is_bytes t.kind then
    Fmt.pf ppf "#%d %s %d%s @%d+%d" t.index (kind_name t.kind) t.socket
      (side_name t.side) t.offset t.length
  else
    Fmt.pf ppf "#%d %s %d%s @%d" t.index (kind_name t.kind) t.socket
      (side_name t.side) t.offset
