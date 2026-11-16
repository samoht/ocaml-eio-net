(** Tests of three clients: a framed reader, a client that retries, and a UDP
    request-response client. Each takes the program under test. *)

val test_frames :
  ?budget:int ->
  ?replay:string ->
  read:
    (clock:float Eio.Time.clock_ty Eio.Resource.t ->
    [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t ->
    string option) ->
  unit ->
  unit
(** [test_frames ~budget ~replay ~read ()] checks that [read] gets a prefix of
    the frames a server sends, under {!Eio_net_chaos.check}. *)

val test_cut :
  read:
    (clock:Eio_mock.Clock.t ->
    [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t ->
    string option) ->
  unit ->
  unit
(** [test_cut ~read ()] pins an end of stream inside the third frame's header,
    and checks that [read] raises [App.Frame.Truncated] there. *)

val test_retries :
  fetch:
    (clock:float Eio.Time.clock_ty Eio.Resource.t ->
    [ `Generic ] Eio.Net.ty Eio.Resource.t ->
    Eio.Net.Sockaddr.stream ->
    (string, [ `Unreachable ]) result) ->
  unit ->
  unit
(** [test_retries ~fetch ()] checks that [fetch] gets the server's answer within
    10 seconds, or gives up in the outage. *)

val test_datagrams :
  call:
    (clock:float Eio.Time.clock_ty Eio.Resource.t ->
    [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t ->
    Eio.Net.Sockaddr.datagram ->
    id:string ->
    string ->
    (string, [ `Timeout ]) result) ->
  unit ->
  unit
(** [test_datagrams ~call ()] checks that five calls get their own answers, or
    time out in the outage. *)
