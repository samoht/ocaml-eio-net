module O = Eio.Net.Sockopt

type class_ =
  | Modelled
  | Approximated of string
  | Stored_only of string
  | Unsupported of string

type sample = Sample : 'a Eio.Net.Sockopt.t * 'a -> sample

type feature = {
  name : string;
  linux : class_;
  macos : class_;
  samples : sample list;
}

let feature ?(samples = []) name ~linux ~macos = { name; linux; macos; samples }
let both ?samples name c = feature ?samples name ~linux:c ~macos:c

let linux_only =
  Unsupported
    "Under macOS, an option of Linux's is refused with `Invalid_option`, as \
     eio_posix has no such option there."

let behaviours =
  [
    feature "Ephemeral ports"
      ~linux:
        (Approximated
           "A socket bound at port 0 takes a port of 49152-65535 in a fixed \
            order, where Linux's default range is 32768-60999. Read the port a \
            listener got with `Eio.Net.listening_addr`.")
      ~macos:
        (Approximated
           "A socket bound at port 0 takes a port of 49152-65535 in a fixed \
            order, and a UDP one the lowest free, where macOS picks its own. \
            Read the port a listener got with `Eio.Net.listening_addr`.");
    both "An unbound UDP socket's port"
      (Approximated
         "`` `UdpV4 `` and `` `UdpV6 `` take their port as the socket is made, \
          so a full range raises `EADDRINUSE` from `datagram_socket`, where \
          the kernel takes it at the first send, and Linux fails that send \
          with `EAGAIN`.");
    both "Host resolution"
      (Approximated
         "Only numeric addresses and `localhost` resolve, as on a host with no \
          name server and no services database. Give tests addresses, not host \
          names.");
    both "Unix paths"
      (Approximated
         "Unix paths live in a namespace of their own, not in a file system: a \
          missing directory or a denied permission fails no bind. Test those \
          on real sockets.");
    feature "A connection's queue with no buffer set"
      ~linux:
        (Approximated
           "A connection queues 4 MiB over TCP and 256 KiB over AF_UNIX, at \
            least the 2.6 MiB and 250 KiB that Linux's default buffers hold, \
            so a writer that fills it waits later than on Linux.")
      ~macos:
        (Approximated
           "A connection queues 1 MiB over TCP, what macOS's buffers grow to \
            from about 330 KiB, and 8 KiB over AF_UNIX, as macOS holds.");
    feature "Loopback delivery" ~linux:Modelled
      ~macos:
        (Approximated
           "A segment arrives at once, where macOS's loopback delivers it \
            asynchronously. Test code that depends on it against real sockets.");
    feature "Datagram memory" ~linux:Modelled
      ~macos:
        (Approximated
           "A UDP datagram is counted by its length and a Unix-domain one as \
            at least 128 bytes, where macOS counts mbufs.");
    both "Accept queue" Modelled;
    both "Routes off loopback" Modelled;
    both "TIME_WAIT and FIN_WAIT2, on a clock" Modelled;
    both "A silent connection, on a clock" Modelled;
  ]

let options =
  [
    feature "`SO_DEBUG`"
      ~samples:[ Sample (O.SO_DEBUG, false) ]
      ~linux:
        (Stored_only
           "Turning `SO_DEBUG` on raises `EACCES`, as for a process without \
            `CAP_NET_ADMIN`.")
      ~macos:(Stored_only "");
    feature "`SO_BROADCAST`"
      ~samples:[ Sample (O.SO_BROADCAST, true) ]
      ~linux:
        (Stored_only
           "A send to a broadcast address is refused with `EACCES` whatever \
            `SO_BROADCAST` says.")
      ~macos:(Stored_only "");
    both "The `reuse_addr` argument" Modelled;
    both "`SO_REUSEADDR`"
      ~samples:[ Sample (O.SO_REUSEADDR, true) ]
      (Stored_only
         "A bind looks at the `reuse_addr` argument of `listen` and \
          `datagram_socket` only, not at `SO_REUSEADDR` among a connect's \
          options.");
    both "`SO_KEEPALIVE`" ~samples:[ Sample (O.SO_KEEPALIVE, false) ] Modelled;
    both "`SO_DONTROUTE`"
      ~samples:[ Sample (O.SO_DONTROUTE, true) ]
      (Stored_only "");
    both "`SO_OOBINLINE`"
      ~samples:[ Sample (O.SO_OOBINLINE, true) ]
      (Stored_only "Eio sends no urgent data, so `SO_OOBINLINE` acts on none.");
    both "The `reuse_port` argument"
      (Unsupported
         "`listen` and `datagram_socket` refuse `reuse_port` with \
          `Invalid_argument`, where the kernel shares the address between \
          sockets.");
    both "`SO_REUSEPORT`"
      ~samples:[ Sample (O.SO_REUSEPORT, true) ]
      (Stored_only "");
    feature "`SO_SNDBUF` and `SO_RCVBUF`"
      ~samples:[ Sample (O.SO_SNDBUF, 65536); Sample (O.SO_RCVBUF, 65536) ]
      ~linux:
        (Approximated
           "`SO_SNDBUF` and `SO_RCVBUF` read back as Linux keeps them, and a \
            TCP connection's queue is within 15% of what Linux holds in writes \
            of 1 KiB.")
      ~macos:
        (Approximated
           "`SO_SNDBUF` and `SO_RCVBUF` read back as macOS keeps them, but are \
            not rounded to whole segments when set before a connect or taken \
            from a listener, and a TCP connection's queue fits macOS less \
            closely than it fits Linux.");
    both "`SO_RCVLOWAT`"
      ~samples:[ Sample (O.SO_RCVLOWAT, 10) ]
      (Stored_only "A waiting read wakes before `SO_RCVLOWAT` bytes are queued.");
    feature "`SO_SNDLOWAT`"
      ~samples:[ Sample (O.SO_SNDLOWAT, 10) ]
      ~linux:
        (Unsupported
           "Linux refuses `SO_SNDLOWAT`, and so does the network, with \
            `Invalid_option`.")
      ~macos:
        (Stored_only "A write does not wait for `SO_SNDLOWAT` bytes of room.");
    both "`SO_LINGER` of 0" ~samples:[ Sample (O.SO_LINGER, Some 0) ] Modelled;
    feature "`SO_LINGER` above 0"
      ~samples:[ Sample (O.SO_LINGER, Some 5) ]
      ~linux:
        (Stored_only
           "A close returns at once, where the kernel waits up to the \
            `SO_LINGER` time for unsent bytes to go. A negative time reads \
            back as set, where Linux's answer is not recorded.")
      ~macos:
        (Stored_only
           "A close returns at once, where the kernel waits up to the \
            `SO_LINGER` time for unsent bytes to go.");
    both "`SO_RCVTIMEO` and `SO_SNDTIMEO`"
      ~samples:[ Sample (O.SO_RCVTIMEO, 1.); Sample (O.SO_SNDTIMEO, 1.) ]
      (Stored_only
         "No read or write times out, as under eio_posix, which waits for its \
          non-blocking sockets in poll.");
    both "`TCP_NODELAY`"
      ~samples:[ Sample (O.TCP_NODELAY, true) ]
      (Stored_only
         "A write is delivered at once, as without Nagle's algorithm, whatever \
          `TCP_NODELAY` says.");
    both "`TCP_KEEPIDLE`" ~samples:[ Sample (O.TCP_KEEPIDLE, 10) ] Modelled;
    both "`TCP_KEEPINTVL`" ~samples:[ Sample (O.TCP_KEEPINTVL, 10) ] Modelled;
    both "`TCP_KEEPCNT`" ~samples:[ Sample (O.TCP_KEEPCNT, 2) ] Modelled;
    feature "`TCP_USER_TIMEOUT`"
      ~samples:[ Sample (O.TCP_USER_TIMEOUT, 1000) ]
      ~linux:Modelled ~macos:linux_only;
    both "`TCP_MAXSEG`"
      ~samples:[ Sample (O.TCP_MAXSEG, 500) ]
      (Stored_only
         "`TCP_MAXSEG` reads back as the kernel works it out, but a write is \
          not cut into segments.");
    feature "`TCP_CONGESTION`"
      ~samples:[ Sample (O.TCP_CONGESTION, "reno") ]
      ~linux:
        (Stored_only
           "`TCP_CONGESTION` takes `cubic` and `reno` only, with no effect on \
            timing.")
      ~macos:linux_only;
    feature "`TCP_CORK`"
      ~samples:[ Sample (O.TCP_CORK, true) ]
      ~linux:
        (Stored_only "A write is delivered at once, whatever `TCP_CORK` says.")
      ~macos:linux_only;
    feature "`TCP_LINGER2`"
      ~samples:[ Sample (O.TCP_LINGER2, Some 10) ]
      ~linux:Modelled ~macos:linux_only;
    feature "`TCP_DEFER_ACCEPT`"
      ~samples:[ Sample (O.TCP_DEFER_ACCEPT, 5) ]
      ~linux:
        (Stored_only
           "An accept does not wait for the client's first data under \
            `TCP_DEFER_ACCEPT`.")
      ~macos:linux_only;
    feature "`TCP_SYNCNT`"
      ~samples:[ Sample (O.TCP_SYNCNT, 3) ]
      ~linux:
        (Stored_only
           "No handshake is lost, and eio-net.chaos's lost one times out after \
            133 seconds whatever `TCP_SYNCNT` says.")
      ~macos:linux_only;
    feature "`TCP_WINDOW_CLAMP`"
      ~samples:[ Sample (O.TCP_WINDOW_CLAMP, 10000) ]
      ~linux:
        (Stored_only
           "A connection's queue does not shrink to `TCP_WINDOW_CLAMP`.")
      ~macos:linux_only;
    feature "`TCP_QUICKACK`"
      ~samples:[ Sample (O.TCP_QUICKACK, true) ]
      ~linux:
        (Stored_only
           "No acknowledgement is delayed, with `TCP_QUICKACK` or without.")
      ~macos:linux_only;
    feature "`TCP_FASTOPEN`"
      ~samples:[ Sample (O.TCP_FASTOPEN, 5) ]
      ~linux:(Stored_only "A connect carries no data under `TCP_FASTOPEN`.")
      ~macos:linux_only;
    feature "`IP_FREEBIND`"
      ~samples:[ Sample (O.IP_FREEBIND, true) ]
      ~linux:
        (Stored_only
           "A connect's `bind_to` still needs a loopback address with \
            `IP_FREEBIND`.")
      ~macos:linux_only;
    feature "`IP_BIND_ADDRESS_NO_PORT`"
      ~samples:[ Sample (O.IP_BIND_ADDRESS_NO_PORT, true) ]
      ~linux:
        (Stored_only
           "A connect's `bind_to` at port 0 takes its port at once, whatever \
            `IP_BIND_ADDRESS_NO_PORT` says.")
      ~macos:linux_only;
    feature "`IP_LOCAL_PORT_RANGE`"
      ~samples:[ Sample (O.IP_LOCAL_PORT_RANGE, (40000, 40100)) ]
      ~linux:
        (Stored_only
           "Ports still come from 49152-65535 whatever `IP_LOCAL_PORT_RANGE` \
            says.")
      ~macos:linux_only;
    both "`IP_TTL`"
      ~samples:[ Sample (O.IP_TTL, 32) ]
      (Stored_only "No router lies on the loopback path for `IP_TTL` to count.");
    feature "`IP_MTU`" ~linux:Modelled ~macos:linux_only;
    feature "`IP_MTU_DISCOVER`"
      ~samples:[ Sample (O.IP_MTU_DISCOVER, `Do) ]
      ~linux:
        (Stored_only
           "No datagram is fragmented on loopback, whatever `IP_MTU_DISCOVER` \
            says.")
      ~macos:linux_only;
    both "`IPV6_ONLY`"
      ~samples:[ Sample (O.IPV6_ONLY, true) ]
      (Stored_only
         "Only a connect's options set `IPV6_ONLY` before a bind, and that \
          bind does not look at it.");
    feature "`SO_ACCEPTCONN`" ~linux:Modelled ~macos:linux_only;
    both "`SO_ERROR`"
      (Approximated
         "`SO_ERROR` reads 0, where the kernel gives an error not yet \
          reported, such as a reset, and clears it.");
    both "`SO_TYPE`" Modelled;
  ]
