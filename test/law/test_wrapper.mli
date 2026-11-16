(** A wrapper against its bare network, on generated sequences. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs generated sequences of operations, with closes, uses after a
    close, switch ends, cancels, pending operations and shutdowns among them, on
    eio-net.mem bare and wrapped with no fault, and checks that each step does
    the same and leaves the same sockets open. Under random faults, it checks
    that closing every socket a sequence holds closes every socket the wrapper
    made. *)
