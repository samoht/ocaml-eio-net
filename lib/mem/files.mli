(** The Unix-domain paths that hold a socket file, shared by stream and datagram
    sockets as a file system shares them. A socket's file stays once it closes,
    as {!Stale}, as Eio's backends leave the file of a client bound with
    [bind_to], until it is removed. *)

(** What holds a path. *)
type kind =
  | Listener  (** A listener. *)
  | Datagram  (** A datagram socket. *)
  | Client  (** A client bound with [bind_to]. *)
  | Stale  (** The file a closed socket left. *)

type t
(** The paths of one network. *)

val v : unit -> t
(** [v ()] holds no path. *)

val abstract : string -> bool
(** [abstract path] is [true] for an address of Linux's abstract namespace,
    which starts with a NUL. *)

val find : t -> stream:bool -> string -> kind option
(** [find t ~stream path] is what holds [path] for a socket of the type [stream]
    says: at an abstract address, only a socket of that type. *)

val autobind : t -> string
(** [autobind t] is an abstract address no socket holds, a NUL and five
    hexadecimal digits, as Linux autobinds one. *)

val bound : t -> string -> string
(** [bound t path] is [path], or for the empty path {!autobind}'s address. *)

val bind : t -> ?take_over:bool -> string -> kind -> bool
(** [bind t ~take_over path kind] binds [path] to [kind] unless a file is there,
    and is [true] if it did. [take_over] binds over a file; an abstract address
    has none. *)

val leave : ?remove:bool -> t -> string -> kind -> unit
(** [leave ~remove t path kind] closes the socket of [kind] at [path], which
    leaves its file, or at an abstract address takes it away. With [remove] it
    takes away the file at [path], whichever socket made it, as unlink(2) does.
*)
