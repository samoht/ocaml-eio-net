(** Unix adapters for frame channels, stream writes, sockets and the host's
    interface addresses. *)

module Frame = Frame
(** Frame channels over Unix datagram descriptors. *)

module Stream = Stream
(** Stream writes offloaded to a systhread. *)

module Local = Local
(** Unix-domain stream sockets. *)

module Interface = Interface
(** Interface addresses and their changes. *)
