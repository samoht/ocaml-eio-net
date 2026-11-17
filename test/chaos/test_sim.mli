(** Tests of the environment a program runs with. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs programs written against [Eio.Stdenv] and checks what each
    device does. *)
