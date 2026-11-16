(** How long a closed TCP end waits for its peer's FIN. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks FIN_WAIT2 against [TCP_LINGER2]. *)
