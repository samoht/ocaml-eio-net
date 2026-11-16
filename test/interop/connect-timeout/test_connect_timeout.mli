(** The connect timeouts of eio-net.chaos and eio-net.mem against the kernels'
    measured ones. *)

val suite : string * unit Alcotest.test_case list
(** One test per model of a connect whose SYNs are all dropped. *)
