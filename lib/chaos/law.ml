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
