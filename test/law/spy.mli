(** A network that hands every operation to another and records it: the calls a
    wrapper makes of its base, and the sockets left open. *)

type call = Law.interface * string
(** An operation, by its interface and the function that runs it, as {!Law.name}
    names it. *)

val equal_call : call -> call -> bool
(** [equal_call a b] is [true] iff [a] and [b] are the same operation. *)

val operations : call list
(** [operations] is every operation of the interfaces the spy implements, Eio's
    and Eio_unix's, each of which needs a row of {!Law}. *)

type t
(** A record of the calls made of a spy network and its sockets. *)

val v :
  [ `Generic ] Eio.Net.ty Eio.Resource.t ->
  t * [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v base] is a record and the network that hands each operation to [base] and
    records it there. *)

val calls : t -> call list
(** [calls t] is the calls made so far, oldest first. *)

val clear : t -> unit
(** [clear t] forgets the calls made so far. *)

val opened : t -> string list
(** [opened t] is the sockets made and not yet closed, by their own close or
    their switch's end, each named by its kind and its place among them. *)
