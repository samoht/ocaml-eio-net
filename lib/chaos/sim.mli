(** The environment of a run: Eio's standard environment, each device simulated.
    {!Eio_net_chaos.env} documents it. *)

type env =
  < net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string >

type output
(** The type for what a run prints on its standard output and error, and traces:
    its last 2 KiB. *)

val output : unit -> output
(** [output ()] is a new, empty output. *)

val printed : output -> int * string
(** [printed o] is the last bytes [o] holds, cut where a UTF-8 character starts,
    and how many came before them. *)

val v :
  net:[ `Generic ] Eio.Net.ty Eio.Resource.t Lazy.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  mono_clock:Eio.Time.Mono.ty Eio.Resource.t ->
  seed:int array ->
  output ->
  env
(** [v ~net ~clock ~mono_clock ~seed o] is an environment with [net], the
    clocks, entropy drawn from [seed], an empty standard input, and a standard
    output and error that write to [o]. Its domain manager runs work in the
    calling fiber, and its process manager refuses with [EPERM]. *)

val with_process : seed:int array -> output -> (unit -> 'a) -> 'a
(** [with_process ~seed o f] is [f ()], with OCaml's [Random] seeded from [seed]
    and Eio's traces written to [o]. *)
