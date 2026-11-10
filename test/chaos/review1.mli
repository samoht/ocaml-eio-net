(** Review 1's tests: a request-response server under five clients, and its idle
    timeout. Each takes the server under test. *)

val test :
  serve:
    (clock:float Eio.Time.clock_ty Eio.Resource.t ->
    [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t ->
    [ `Stop_daemon ]) ->
  unit ->
  unit
(** [test ~serve ()] checks that at least three of five clients are served,
    under {!Eio_net_chaos.check}. *)

val test_idle :
  serve:
    (clock:Eio_mock.Clock.t ->
    [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t ->
    [ `Stop_daemon ]) ->
  unit ->
  unit
(** [test_idle ~serve ()] pins a client that sends nothing, and checks that the
    server lets it go within 30 seconds. *)
