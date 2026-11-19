(** The network of {!Eio_net_chaos.run}, as a device of {!Eio_chaos.run}. *)

type t
(** The type for networks: an in-memory host with a loopback interface. *)

val v : unit -> t
(** [v ()] is a network. *)

module Device : sig
  val v : t -> Eio_chaos.device
  (** [v t] is [t] as a device named ["net"]. *)

  val find : Eio_chaos.t -> t option
  (** [find c] is the network of the run of [c], while it runs. *)
end

val device : unit -> Eio_chaos.device
(** [device ()] is [Device.v (v ())]. *)

val tiny_buffer : int
(** [tiny_buffer] is the most a connection queues each way while tiny buffers
    hold: 32 KiB. *)
