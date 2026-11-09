(** Frame channels for Eio, a stream relay and a transfer bounded by its
    progress.

    A frame channel ({!Frame}) is a packet link where one receive is one frame.
    It runs over a datagram socket, over a byte stream where each frame is a
    length prefix and a payload in the format of {!Eio_net_framing}, or over an
    in-memory pair. [Eio_net_unix.Frame] builds one over a Unix datagram
    descriptor, with batched system calls. {!Datagram} is the addressed channel
    over a datagram socket: each receive also gives the source.

    {!Relay} forwards stream connections between two addresses, and {!Progress}
    ends a transfer that moves too few bytes a second. *)

module Frame = Frame
(** Frame channels. *)

module Datagram = Datagram
(** Addressed datagram channels. *)

module Relay = Relay
(** Stream relays. *)

module Progress = Progress
(** Transfers bounded by their progress. *)
