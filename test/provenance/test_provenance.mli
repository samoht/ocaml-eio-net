(** Every directory of committed traces under [test/interop] says where its
    traces come from, and still matches the generator that would regenerate
    them. *)

val suite : string * unit Alcotest.test_case list
(** One test per oracle directory. *)
