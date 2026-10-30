(** The host's network interface addresses, and their changes. *)

type address = {
  interface : string;  (** The interface's name, as [en0] or [eth0]. *)
  prefix : Ipaddr.Prefix.t;
      (** The address with its on-link prefix length, as [192.0.2.7/24]: the
          interface's netmask, or the full length where it has none. *)
  loopback : bool;  (** Whether the interface is a loopback one. *)
}
(** The type for one address an interface carries. *)

val addresses : unit -> address list
(** [addresses ()] is every IPv4 and IPv6 address of the host's interfaces, up
    or down, in the order getifaddrs(3) lists them. An IPv6 link-local address
    is the one the interface's link sees, with no scope embedded in it as a KAME
    stack (macOS, the BSDs) embeds it.

    @raise Unix.Unix_error if getifaddrs fails. *)

type watch
(** The type for subscriptions to the host's address changes. *)

val watch : sw:Eio.Switch.t -> watch
(** [watch ~sw] subscribes to the kernel's address-change notifications:
    rtnetlink's [RTMGRP_IPV4_IFADDR] and [RTMGRP_IPV6_IFADDR] groups on Linux, a
    [PF_ROUTE] socket elsewhere. Releasing [sw] closes it. A caller that reads
    {!addresses} after opening the watch misses no change.

    @raise Unix.Unix_error if the socket cannot be opened. *)

val changed : watch -> unit
(** [changed w] waits until an address has been added to or removed from an
    interface since the previous call returned, or since [w] opened. A
    notification the kernel dropped because the socket's buffer was full counts
    as a change. A routing socket also carries messages of no address, a route
    learned for each new destination among them: each costs one read and returns
    nothing. *)
