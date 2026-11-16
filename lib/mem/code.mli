(** The backend codes of the errors the network raises, and their makers.
    {!Eio_net_mem} documents each code. *)

type Eio.Exn.Backend.t +=
  | Closed  (** A connection reset. *)
  | No_listener  (** A refused connect or datagram. *)
  | Address_in_use  (** [EADDRINUSE]. *)
  | No_destination  (** [EDESTADDRREQ]. *)
  | Message_too_long  (** [EMSGSIZE]. *)
  | Invalid_port  (** [EINVAL] for UDP port 0. *)
  | Network_unreachable  (** [ENETUNREACH]. *)
  | Address_family  (** [EAFNOSUPPORT]. *)
  | Wrong_socket_type  (** [EPROTOTYPE]. *)
  | Not_connected  (** [ENOTCONN]. *)
  | Address_not_available  (** [EADDRNOTAVAIL]. *)
  | Name_too_long  (** [ENAMETOOLONG]. *)
  | Invalid_address  (** [EINVAL] for an address. *)
  | Broadcast  (** [EACCES] for a broadcast. *)
  | Host_unreachable  (** [EHOSTUNREACH]. *)

val reset_exn : unit -> exn
(** [reset_exn ()] is the connection reset a flow raises. *)

val address_in_use : unit -> exn
(** [address_in_use ()] is the unclassified error marked {!Address_in_use}. *)

val fail : Eio.Exn.Backend.t -> 'a
(** [fail code] raises the unclassified {!Eio.Io} error marked [code]. *)

val refused : unit -> 'a
(** [refused ()] raises the connection failure marked {!No_listener}. *)

val no_file : unit -> exn
(** [no_file ()] is the [Eio.Fs.Not_found] error of a Unix path no socket binds.
*)
