open! Hermetic

type forwarded = [ `Forwarded ]
type intercepted = [ `Intercepted ]
type synthesized = [ `Synthesized ]
type unsupported = [ `Unsupported ]

type _ class_ =
  | Forwarded : forwarded class_
  | Intercepted : {
      kind : Op.kind;
      inet : Fault.t list;
      unix : Fault.t list;
    }
      -> intercepted class_
  | Synthesized : synthesized class_
  | Unsupported : unsupported class_

type interface =
  [ `Network | `Listening_socket | `Stream_socket | `Datagram_socket ]

type _ t =
  | Connect : intercepted t
  | Listen : forwarded t
  | Datagram_socket : forwarded t
  | Getaddrinfo : forwarded t
  | Getnameinfo : forwarded t
  | Outage : synthesized t
  | Livelocked : synthesized t
  | Overrun : synthesized t
  | Splits : synthesized t
  | Activity : synthesized t
  | Accept : intercepted t
  | Listening_addr : forwarded t
  | Listener_setsockopt : forwarded t
  | Listener_getsockopt : forwarded t
  | Listener_close : forwarded t
  | Listener_fd : unsupported t
  | Read_methods : synthesized t
  | Single_read : intercepted t
  | Single_write : intercepted t
  | Copy : synthesized t
  | Shutdown : forwarded t
  | Setsockopt : forwarded t
  | Getsockopt : forwarded t
  | Close : forwarded t
  | Fd : unsupported t
  | Connection : unsupported t
  | Silence : unsupported t
  | Send : forwarded t
  | Recv : intercepted t
  | Datagram_shutdown : forwarded t
  | Datagram_setsockopt : forwarded t
  | Datagram_getsockopt : forwarded t
  | Datagram_close : forwarded t
  | Datagram_fd : unsupported t

type op = Op : _ t -> op

let all =
  [
    Op Connect;
    Op Listen;
    Op Datagram_socket;
    Op Getaddrinfo;
    Op Getnameinfo;
    Op Outage;
    Op Livelocked;
    Op Overrun;
    Op Splits;
    Op Activity;
    Op Accept;
    Op Listening_addr;
    Op Listener_setsockopt;
    Op Listener_getsockopt;
    Op Listener_close;
    Op Listener_fd;
    Op Read_methods;
    Op Single_read;
    Op Single_write;
    Op Copy;
    Op Shutdown;
    Op Setsockopt;
    Op Getsockopt;
    Op Close;
    Op Fd;
    Op Connection;
    Op Silence;
    Op Send;
    Op Recv;
    Op Datagram_shutdown;
    Op Datagram_setsockopt;
    Op Datagram_getsockopt;
    Op Datagram_close;
    Op Datagram_fd;
  ]

let interface : type c. c t -> interface = function
  | Connect | Listen | Datagram_socket | Getaddrinfo | Getnameinfo | Outage
  | Livelocked | Overrun | Splits | Activity ->
      `Network
  | Accept | Listening_addr | Listener_setsockopt | Listener_getsockopt
  | Listener_close | Listener_fd ->
      `Listening_socket
  | Read_methods | Single_read | Single_write | Copy | Shutdown | Setsockopt
  | Getsockopt | Close | Fd | Connection | Silence ->
      `Stream_socket
  | Send | Recv | Datagram_shutdown | Datagram_setsockopt | Datagram_getsockopt
  | Datagram_close | Datagram_fd ->
      `Datagram_socket

let name : type c. c t -> string = function
  | Connect -> "Eio.Net.connect"
  | Listen -> "Eio.Net.listen"
  | Datagram_socket -> "Eio.Net.datagram_socket"
  | Getaddrinfo -> "Eio.Net.getaddrinfo"
  | Getnameinfo -> "Eio.Net.getnameinfo"
  | Outage -> "Wrapper.is_outage"
  | Livelocked -> "Wrapper.livelocked"
  | Overrun -> "Wrapper.overrun"
  | Splits -> "Wrapper.splits"
  | Activity -> "Wrapper.activity"
  | Accept -> "Eio.Net.accept"
  | Listening_addr -> "Eio.Net.listening_addr"
  | Listener_setsockopt | Setsockopt | Datagram_setsockopt ->
      "Eio.Net.setsockopt"
  | Listener_getsockopt | Getsockopt | Datagram_getsockopt ->
      "Eio.Net.getsockopt"
  | Listener_close | Close | Datagram_close -> "Eio.Resource.close"
  | Listener_fd | Fd | Datagram_fd -> "Eio_unix.Resource.fd_opt"
  | Read_methods -> "Eio.Flow.Pi.SOURCE.read_methods"
  | Single_read -> "Eio.Flow.single_read"
  | Single_write -> "Eio.Flow.single_write"
  | Copy -> "Eio.Flow.copy"
  | Shutdown | Datagram_shutdown -> "Eio.Flow.shutdown"
  | Connection -> "Eio_net_mem.Private.connection_id"
  | Silence -> "Eio_net_mem.Private.silence"
  | Send -> "Eio.Net.send"
  | Recv -> "Eio.Net.recv"

let pp ppf op =
  Fmt.pf ppf "%s of %s" (name op)
    (match interface op with
    | `Network -> "a network"
    | `Listening_socket -> "a listener"
    | `Stream_socket -> "a stream"
    | `Datagram_socket -> "a datagram socket")

let stream_faults = Fault.[ Short 1; Stall 1.; Hang; Reset; Eof ]

let class_ : type c. c t -> c class_ = function
  | Connect ->
      Intercepted
        {
          kind = `Connect;
          inet = Fault.[ Refuse; Reset; Timeout; Unreachable; Hang; Stall 1. ];
          unix = Fault.[ Refuse; Reset; Hang; Stall 1. ];
        }
  | Accept ->
      let faults = Fault.[ Reset; Eof; No_descriptor; Hang; Stall 1. ] in
      Intercepted { kind = `Accept; inet = faults; unix = faults }
  | Single_read ->
      Intercepted { kind = `Read; inet = stream_faults; unix = stream_faults }
  | Single_write ->
      Intercepted { kind = `Write; inet = stream_faults; unix = stream_faults }
  | Recv ->
      Intercepted
        {
          kind = `Recv;
          inet = Fault.[ Drop; Dup; Reorder; Garble 3; Stall 1.; Hang ];
          unix = Fault.[ Stall 1. ];
        }
  | Listen -> Forwarded
  | Datagram_socket -> Forwarded
  | Getaddrinfo -> Forwarded
  | Getnameinfo -> Forwarded
  | Listening_addr -> Forwarded
  | Listener_setsockopt -> Forwarded
  | Listener_getsockopt -> Forwarded
  | Listener_close -> Forwarded
  | Shutdown -> Forwarded
  | Setsockopt -> Forwarded
  | Getsockopt -> Forwarded
  | Close -> Forwarded
  | Send -> Forwarded
  | Datagram_shutdown -> Forwarded
  | Datagram_setsockopt -> Forwarded
  | Datagram_getsockopt -> Forwarded
  | Datagram_close -> Forwarded
  | Outage -> Synthesized
  | Livelocked -> Synthesized
  | Overrun -> Synthesized
  | Splits -> Synthesized
  | Activity -> Synthesized
  | Read_methods -> Synthesized
  | Copy -> Synthesized
  | Listener_fd -> Unsupported
  | Fd -> Unsupported
  | Connection -> Unsupported
  | Silence -> Unsupported
  | Datagram_fd -> Unsupported

let kind (op : intercepted t) = match class_ op with Intercepted i -> i.kind

let no_fd =
  "The wrapper exposes no descriptor, so no code reaches the socket past its \
   faults: [fd_opt] is [None] where eio_posix gives the socket's."

let doc : type c. c t -> string = function
  | Connect ->
      "Refuse and Reset refuse it, Timeout fails it after the kernel's \
       handshake time, Unreachable fails it at once, Hang waits for a cancel \
       and Stall delays it. A Unix-domain connect has no handshake to lose and \
       no route to miss."
  | Accept ->
      "Reset hands up a connection reset at both ends, or over AF_UNIX one its \
       client closed, Eof one its client closed, No_descriptor fails it and \
       keeps the connection queued until a descriptor is freed, Hang waits for \
       a cancel and keeps it queued, and Stall delays it. A listener's close, \
       or its switch's end, closes the connections it kept."
  | Single_read ->
      "Short cuts it, Stall delays it, Hang silences the connection, Reset \
       resets it and Eof ends it. A read takes from the socket no more than it \
       hands up."
  | Single_write ->
      "Short cuts it, Stall delays it, Hang silences the connection, Reset \
       resets it, and Eof ends this end's sends and fails the write."
  | Recv ->
      "Drop loses the datagram, Dup delivers it twice, Reorder after the next, \
       Garble replaces it with noise, Stall delays it and Hang waits for a \
       cancel. A Unix-domain socket loses, duplicates, reorders and garbles \
       nothing, so only Stall changes it."
  | Listen | Datagram_socket ->
      "The socket is wrapped, and its switch's end drops what the wrapper \
       keeps for it."
  | Close | Listener_close | Datagram_close ->
      "The close drops what the wrapper keeps for the socket and frees a \
       descriptor for an accept that No_descriptor failed."
  | Shutdown ->
      "After a fault broke the connection, a shutdown raises nothing: the \
       wrapper already shut the socket down."
  | Getaddrinfo | Getnameinfo | Listening_addr | Listener_setsockopt
  | Listener_getsockopt | Setsockopt | Getsockopt | Send | Datagram_shutdown
  | Datagram_setsockopt | Datagram_getsockopt ->
      ""
  | Outage | Livelocked | Overrun | Splits | Activity ->
      "The wrapper's own state, for the report of a check."
  | Read_methods ->
      "The wrapper offers no read method, so every read goes through \
       [single_read] and meets its faults."
  | Copy ->
      "The wrapper copies by its own [single_write], so every write of a copy \
       meets its faults."
  | Listener_fd | Fd | Datagram_fd -> no_fd
  | Connection ->
      "A wrapper's connection is not one of eio-net.mem's: a wrapper over a \
       wrapper sees the ends of a connection apart, as over real sockets."
  | Silence ->
      "A wrapper's connection is not one of eio-net.mem's, so [silence] raises \
       [Invalid_argument]; the wrapper silences a connection with Hang."
