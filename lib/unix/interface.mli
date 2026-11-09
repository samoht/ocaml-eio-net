(** The host's network interface addresses, and their changes. *)

(** Interface addresses. *)
module Address : sig
  type t
  (** The type for one address an interface carries. *)

  val v : interface:string -> prefix:Ipaddr.Prefix.t -> loopback:bool -> t
  (** [v ~interface ~prefix ~loopback] is the address [prefix] of the interface
      named [interface], a loopback interface iff [loopback] is [true]. *)

  val interface : t -> string
  (** [interface a] is the name of [a]'s interface, as [en0] or [eth0]. *)

  val prefix : t -> Ipaddr.Prefix.t
  (** [prefix a] is [a] with its on-link prefix length, as [192.0.2.7/24]: the
      interface's netmask, or the full length where it has none. *)

  val is_loopback : t -> bool
  (** [is_loopback a] is [true] iff [a]'s interface is a loopback interface. *)

  val equal : t -> t -> bool
  (** [equal a0 a1] is [true] iff [a0] and [a1] have equal interfaces, prefixes
      and loopback flags. *)

  val pp : t Fmt.t
  (** [pp] formats an address as its interface and prefix, as [lo0 127.0.0.1/8].
  *)
end

type address = Address.t
(** See {!Address.t}. *)

(** The type for a host's interface addresses and their changes: this module's,
    or a stand-in's. *)
module type S = sig
  val addresses : unit -> address list
  (** [addresses ()] is every IPv4 and IPv6 address of the host's interfaces, up
      or down, in the order getifaddrs(3) lists them. An IPv6 link-local address
      is the one the interface's link sees, with no scope embedded in it as a
      KAME stack (macOS, the BSDs) embeds it.

      Raises the {!Eio.Io} error {!Eio_unix.Err.v} makes of a failed
      getifaddrs(3). *)

  type watch
  (** The type for subscriptions to the host's address changes. *)

  val watch : sw:Eio.Switch.t -> watch
  (** [watch ~sw] subscribes to the kernel's address-change notifications:
      rtnetlink's [RTMGRP_IPV4_IFADDR] and [RTMGRP_IPV6_IFADDR] groups on Linux,
      a [PF_ROUTE] socket elsewhere. Releasing [sw] closes it. A caller that
      reads {!addresses} after opening the watch misses no change.

      Raises the {!Eio.Io} error {!Eio_unix.Err.v} makes of a failure to open
      the socket. *)

  val changed : watch -> unit
  (** [changed w] waits until an address has been added to or removed from an
      interface since the previous call returned, or since [w] opened. A
      notification the kernel dropped because the socket's buffer was full
      counts as a change. On Linux only the kernel's messages count: one a
      process with CAP_NET_ADMIN sends to the watch's port is ignored. A routing
      socket also carries messages of no address, a route learned for each new
      destination among them: each costs one read and returns nothing. A failed
      read raises the {!Eio.Io} error {!Eio_unix.Err.v} makes of it. *)
end

include S
