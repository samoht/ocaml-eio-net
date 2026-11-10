(** Review 2's tests: a keep-alive client that reconnects, and a broker that
    fans messages out to subscribers. *)

val test_kv :
  ?bug:[ `Eof_body | `No_reconnect | `None | `Read_only | `Reset_only ] ->
  ?lenient:bool ->
  unit ->
  unit
(** [test_kv ~bug ~lenient ()] checks that four requests get their values, or
    give up in the outage, under {!Eio_net_chaos.check}. [lenient] makes the
    server take an unterminated last line for a whole one. *)

val test_pubsub :
  ?bug:[ `Fanout_dies | `Hol | `None | `Single_read ] -> unit -> unit
(** [test_pubsub ~bug ()] checks that one of three fast subscribers gets every
    message within a second, beside a slow one, under {!Eio_net_chaos.check}. *)

val pinned : ?bug:[ `Fanout_dies | `Hol | `None | `Single_read ] -> unit -> unit
(** [pinned ~bug ()] runs the broker's scenario once, with no fault, on
    connections that queue 8 bytes each way. *)
