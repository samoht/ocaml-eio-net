(** The life of a socket whose close defers to the operations in flight, as
    Eio's [Rcfd] defers a descriptor's close to its users: a close refuses new
    operations at once, and the last operation in flight closes the socket. *)

(** A socket's life. *)
type t =
  | Open of int  (** Open, with this many operations in flight. *)
  | Closing of int
      (** Closed by hand, with this many operations, at least one, still in
          flight. *)
  | Closed  (** Closed. *)

val v : t
(** [v] is open, with nothing in flight. *)

val is_open : t -> bool
(** [is_open t] is [true] until a close. *)

val is_closed : t -> bool
(** [is_closed t] is [true] once the socket has closed. *)

val enter : t -> t option
(** [enter t] is the life once an operation begins, or [None] after a close,
    which refuses it. *)

val leave : t -> t * bool
(** [leave t] is the life once an operation ends, and [true] if it was the last
    of a closing socket, which closes now. *)

val close : t -> t * [ `Now | `Deferred | `Again ]
(** [close t] is the life once closed by hand: closed [`Now] with nothing in
    flight, [`Deferred] to the last operation otherwise, or [`Again] if it was
    closed before. *)
