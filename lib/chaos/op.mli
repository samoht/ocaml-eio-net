(** The operations a wrapper sees. {!Eio_net_chaos.Op} documents them. *)

type kind = [ `Connect | `Accept | `Read | `Write | `Recv ]
type side = [ `Client | `Server ]

type t = {
  index : int;
  kind : kind;
  socket : int;
  side : side;
  offset : int;
  length : int;
}

val kind_name : kind -> string
(** [kind_name k] is [k] in lower case. *)

val side_name : side -> string
(** [side_name s] is ["c"] or ["s"]. *)

val is_bytes : kind -> bool
(** [is_bytes k] is [true] iff [k] moves the bytes of a connection. *)

val pp : t Fmt.t
(** [pp] formats an operation as [#index kind socket side @offset+length]. *)
