(** Does a network answer as Linux or macOS does? *)

type os = [ `Linux | `Macos ]
(** The type for the kernels the traces hold. *)

type summary = {
  differed : string list;
      (** A line for each sequence or scenario whose answer is outside its set,
          in order. *)
  passed : int;
  left_out : (string * int) list;
      (** How many were left out, by why, in the order of the report. *)
  allowed : string list;
      (** The allowances an answer needed, each by why it holds. *)
}
(** The type for what a replay of the traces found. *)

val replay : os -> (sw:Eio.Switch.t -> _ Eio.Net.t) -> summary
(** [replay os net] runs each sequence and scenario of the traces of [os] on a
    fresh [net ~sw]. *)

val check : ?os:[< os ] -> (sw:Eio.Switch.t -> _ Eio.Net.t) -> unit
(** See {!Eio_net_chaos.device}. *)

val allowances : (os * string) list
(** [allowances] is each allowance, by the kernel it applies to and why it
    holds. *)
