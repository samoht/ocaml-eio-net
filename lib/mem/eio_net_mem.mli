(** In-memory {!Eio.Net} stream network.

    This backend is intended for hermetic tests that need client code and server
    code to talk through the normal {!Eio.Net} APIs without binding OS sockets.
    It supports stream sockets only. *)

val v : sw:Eio.Switch.t -> unit -> [ `Generic ] Eio.Net.ty Eio.Resource.t
(** [v ~sw ()] returns a network whose [listen] and [connect] operations are
    paired in memory.

    TCP stream addresses are matched exactly. Host resolution maps ["localhost"]
    and ["127.0.0.1"] to IPv4 loopback for the requested service port. Unix
    stream addresses are also accepted directly by [listen] and [connect]. *)
