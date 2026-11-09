(** A transfer bounded by its progress, as curl's low-speed check bounds one
    (CURLOPT_LOW_SPEED_LIMIT and CURLOPT_LOW_SPEED_TIME).

    The bytes a transfer's reads and writes move are counted over windows of a
    stated length, each starting where the last ended, the first with the
    transfer's first move, so the time spent before it (a connection, a
    handshake, an authentication) counts in none of them: a window that ends
    with fewer than [limit] bytes a second moved ends the transfer with
    {!Too_slow}. A move that waits on a peer that sends or takes nothing is cut
    at its window's end, so a stalled peer costs at most two windows, and a peer
    that sends a byte now and then costs the same as one that sends nothing. *)

type t
(** The type for the progress of one transfer. *)

exception
  Too_slow of {
    limit : int;  (** The transfer's limit, in bytes a second. *)
    window : Mtime.Span.t;  (** The length of its windows. *)
  }
(** Raised when a window ended with fewer than [limit] bytes a second moved over
    [window]. Its registered printer words it as curl does:
    ["Operation too slow. Less than LIMIT bytes/sec transferred the last SECONDS
     seconds"]. *)

val v : mono:_ Eio.Time.Mono.t -> limit:int -> window:Mtime.Span.t -> t
(** [v ~mono ~limit ~window] is a transfer timed on [mono] whose windows last
    [window] and each move at least [limit] bytes a second.

    Raises [Invalid_argument] if [limit] is not positive or [window] is zero. *)

val move : t -> (unit -> int) -> int
(** [move t f] is [f ()], which moves bytes and returns how many, counted in
    [t]. [f] is called once. Each window that ends while [f] runs is judged: one
    with too few bytes cancels [f] and raises {!Too_slow}, and otherwise [f]
    goes on into the next window. Bytes [f] moved as a window ended count in it.
    Fibers may move in [t] at once, as a reader and a writer of one flow do:
    their bytes count in the same windows, and a window with too few bytes
    cancels every move under way as it ends, each raising {!Too_slow}. An
    exception from [f] propagates. *)

val reader : t -> Bytesrw.Bytes.Reader.t -> Bytesrw.Bytes.Reader.t
(** [reader t r] is [r], each read a {!move} in [t]. *)

val flow : t -> _ Eio.Flow.two_way -> Eio.Flow.two_way_ty Eio.Resource.t
(** [flow t f] is [f], each read and each write a {!move} in [t]. A shutdown
    passes through to [f]. *)
