(** The operations a wrapper sees. {!Eio_net_chaos.Op} documents them. *)

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

val kind_name : kind -> string
(** [kind_name k] is [k] in lower case. *)

val equal_kind : kind -> kind -> bool
(** [equal_kind a b] is [true] iff [a] and [b] are the same kind. *)

val side_name : side -> string
(** [side_name s] is ["c"] or ["s"]. *)

val other : side -> side
(** [other s] is the side opposite [s]. *)

val family_of_stream : Eio.Net.Sockaddr.stream -> family
(** [family_of_stream addr] is the address family of [addr]. *)

val family_of_datagram :
  [ Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6 ] -> family
(** [family_of_datagram addr] is the address family of [addr]. *)

val is_bytes : kind -> bool
(** [is_bytes k] is [true] iff [k] moves the bytes of a connection. *)

val pp : t Fmt.t
(** [pp] formats an operation as [#index kind socket side @offset+length]. *)
