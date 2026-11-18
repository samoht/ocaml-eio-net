(** The network of {!Eio_net_chaos.run}, as a part of {!Eio_chaos.run}. *)

type t
(** The type for networks: an in-memory host with a loopback interface. *)

val v : unit -> t
(** [v ()] is a network. *)

module Part : sig
  val v : t -> Eio_chaos.part
  (** [v t] is [t] as a part named ["net"]. *)

  val find : Eio_chaos.t -> t option
  (** [find c] is the network of the run of [c], while it runs. *)
end

val part : unit -> Eio_chaos.part
(** [part ()] is [Part.v (v ())]. *)

val tiny_buffer : int
(** [tiny_buffer] is the most a connection queues each way while tiny buffers
    hold: 32 KiB. *)
