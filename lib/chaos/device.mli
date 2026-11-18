(** Does a network answer as Linux or macOS does? *)

type os = [ `Linux | `Macos ]
(** The type for the kernels the traces hold. *)

type difference = {
  step : string;  (** The operation, or the scenario, that answered. *)
  expected : string;  (** The answers allowed, as a report names them. *)
  got : string;  (** The answer given, as a report names it. *)
  sequence : string option;
      (** The sequence up to that operation, with its transport, as
          [tcp4 accept; connect]; [None] for a scenario. *)
}
(** The type for an answer outside its set. *)

type summary = {
  differed : difference list;
      (** The first answer outside its set of each sequence or scenario, in
          order. *)
  passed : int;
  left_out : (string * int) list;
      (** How many were left out, by why, in the order of the report. *)
  allowed : string list;
      (** The allowances an answer needed, each by why it holds. *)
}
(** The type for what a replay of the traces found. *)

val replay :
  ?under:Eio_chaos.part list ->
  os ->
  (sw:Eio.Switch.t -> Eio_chaos.env -> _ Eio.Net.t) ->
  summary
(** [replay os net] runs each sequence and scenario of the traces of [os] on a
    fresh [net ~sw env], each in an event loop of its own with a simulated
    [env]. *)

val check :
  ?os:[< os ] ->
  ?under:Eio_chaos.part list ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  (sw:Eio.Switch.t -> Eio_chaos.env -> _ Eio.Net.t) ->
  unit
(** See {!Eio_net_chaos.device}. *)

val check_in_loop : ?os:[< os ] -> (sw:Eio.Switch.t -> _ Eio.Net.t) -> unit
(** [check_in_loop ~os net] is {!check} with each sequence in the caller's event
    loop, on a fresh [net ~sw]: for a network of the host's sockets, which only
    the host's event loop drives. *)

val allowances : (os * string) list
(** [allowances] is each allowance, by the kernel it applies to and why it
    holds. *)
