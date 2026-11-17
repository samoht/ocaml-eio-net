(** Numbers in the prose of reports. *)

val plural : int -> string
(** [plural n] is [""] if [n] is [1], ["s"] otherwise. *)

val pp_count : int Fmt.t
(** [pp_count] formats an integer in groups of three digits, as [21,361]. *)
