(** Runs a socket case against eio-net.mem and real sockets, and compares each
    outcome with what Linux, macOS and eio-net.mem do. *)

(** {1 Platforms and backends} *)

type platform = Linux | Macos

val platform : platform
(** The platform the suite runs on. *)

type backend = Mem | Real

type ctx = {
  backend : backend;
  sw : Eio.Switch.t;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  mutable next : int;
}
(** A case's network, under the backend's event loop: eio-net.mem under
    {!Eio_mock.Backend}, or the host's sockets under {!Eio_posix}. *)

(** {1 Outcomes} *)

val classify : exn -> string
(** [classify e] names the error [e] at the level of Eio's errors: ["EOF"],
    ["reset"], ["refused"], ["ENOENT"], ["unsupported"] for [Invalid_argument],
    or the system error's name. *)

val outcome : (unit -> string) -> string
(** [outcome f] is [f ()], or the error it raises, classified. *)

val outcome_now : (unit -> string) -> string
(** [outcome_now f] is [outcome f] if [f] completes without waiting, and
    ["blocked"] otherwise, as MSG_DONTWAIT tells. *)

val payload : ?seed:int -> int -> string
(** [payload ~seed n] is [n] deterministic bytes. *)

(** {1 Addresses} *)

val pp_ip : Eio.Net.Ipaddr.v4v6 -> string
(** [pp_ip ip] is [ip] in its usual notation. *)

val v4_loopback : Eio.Net.Ipaddr.v4v6
(** 127.0.0.1. *)

val v6_loopback : Eio.Net.Ipaddr.v4v6
(** ::1. *)

val v4_mapped_loopback : Eio.Net.Ipaddr.v4v6
(** ::ffff:127.0.0.1. *)

val v4_mapped_any : Eio.Net.Ipaddr.v4v6
(** ::ffff:0.0.0.0. *)

(** {1 Stream sockets} *)

type stream_kind = Tcp4 | Tcp6 | Unix_stream
type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
type listener = [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t

val listen :
  ?backlog:int -> ?reuse_addr:bool -> ctx -> Eio.Net.Sockaddr.stream -> listener
(** [listen ctx addr] listens at [addr], with a backlog of 16 by default. *)

val stream_addr : ctx -> stream_kind -> Eio.Net.Sockaddr.stream
(** [stream_addr ctx k] is a loopback address with port 0, or a fresh
    Unix-domain path. *)

val connect : ctx -> Eio.Net.Sockaddr.stream -> flow
(** [connect ctx addr] connects to [addr]. *)

val stream_pair : ctx -> stream_kind -> flow * flow * listener
(** [stream_pair ctx k] is a connecting end, the end its listener accepted, and
    the listener. *)

val wait_queued : ctx -> flow -> int -> unit
(** [wait_queued ctx flow n] waits until [n] bytes are queued to be read on
    [flow]. *)

val write : flow -> string -> unit
(** [write flow s] writes all of [s]. *)

val read : flow -> int -> string
(** [read flow n] is what one read of at most [n] bytes returns. *)

val read_exact : flow -> int -> string
(** [read_exact flow n] is the next [n] bytes, as MSG_WAITALL waits for them. *)

(** {1 Datagram sockets} *)

type dgram_kind = Udp4 | Udp6 | Unix_dgram
type dgram = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t

val datagram_socket :
  ctx -> Eio.Net.Sockaddr.datagram -> dgram * Eio.Net.Sockaddr.datagram
(** [datagram_socket ctx addr] is a socket bound at [addr] and the address it
    holds, port 0 resolved. *)

val dgram_socket : ctx -> dgram_kind -> dgram * Eio.Net.Sockaddr.datagram
(** [dgram_socket ctx k] is a socket bound on loopback or at a fresh path. *)

val dgram_shutdown : dgram -> Eio.Flow.shutdown_command -> unit
(** [dgram_shutdown sock cmd] shuts [sock] down, which Eio's datagram socket
    type does not offer. *)

val send : dgram -> dst:Eio.Net.Sockaddr.datagram -> string -> unit
(** [send sock ~dst s] sends [s] as one datagram to [dst]. *)

val recv : dgram -> int -> string
(** [recv sock n] is the next datagram, cut to [n] bytes. *)

(** {1 Socket pairs} *)

type kind = Stream of stream_kind | Dgram of dgram_kind

val kind_name : kind -> string
(** [kind_name k] names [k], as a case's variant. *)

val needs_ipv6 : kind -> bool
(** [needs_ipv6 k] is [true] for the IPv6 kinds. *)

type pair = {
  send : string -> unit;
  recv : int -> string;  (** One read, or one datagram. *)
  recv_all : int -> string;  (** As MSG_WAITALL waits. *)
}

val pair : ctx -> kind -> pair
(** [pair ctx k] sends on one socket and receives on another, as gVisor's socket
    pair fixtures do. *)

(** {1 Cases} *)

type expect = {
  linux : string;
  macos : string;
  mem : string;
  mem_bug : string option;
}
(** A case's outcome on each platform. Linux's is gVisor's. A real platform's
    outcome may list, separated by [" | "], outcomes that depend on timing. A
    mem outcome that matches neither platform is a defect, which [mem_bug] says
    how to fix. *)

val expect : ?macos:string -> ?mem:string -> ?mem_bug:string -> string -> expect
(** [expect linux] is [linux] everywhere unless [macos] or [mem] says otherwise.
*)

type case

val case :
  ?variant:string ->
  ?ipv6:bool ->
  ?capacity:int ->
  file:string ->
  test:string ->
  expect ->
  (ctx -> string) ->
  case
(** [case ~file ~test expect body] runs [body] for gVisor's test [test] of
    [file]. A case with [ipv6] is skipped on real sockets where ::1 cannot be
    bound. eio-net.mem runs it with [capacity], unbounded by default. *)

val suite : string -> case list -> string * unit Alcotest.test_case list
(** [suite name cases] runs each case once on eio-net.mem and once on real
    sockets, in tests named after the case's gVisor file and test. The mem test
    first checks that a mem outcome matching neither platform has a mem bug
    noted. *)
