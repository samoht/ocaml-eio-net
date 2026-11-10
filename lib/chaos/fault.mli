(** The faults a wrapper injects. {!Eio_net_chaos.Fault} documents them. *)

type t =
  | Short of int
  | Stall of float
  | Hang
  | Reset
  | Eof
  | Refuse
  | Timeout
  | Emfile
  | Drop
  | Dup
  | Reorder
  | Garble of int

val pp : t Fmt.t
(** [pp] formats a fault as what it does to an operation. *)

val key : t -> string
(** [key f] is [f] as one word, for a replay key. *)
