(** Host and service resolution as glibc's getaddrinfo(3) reads numbers, on a
    dual-stack host with no services database and no name server, or as macOS
    reads them. *)

val port : string -> int option
(** [port service] is the port glibc reads in [service], if any. *)

val port_macos : string -> int option
(** [port_macos service] is the port macOS reads in [service], if any. *)

val host : os:Kernel.os -> string -> Eio.Net.Ipaddr.v4v6 list
(** [host ~os name] is the addresses [name] stands for. *)
