(** What a wrapper does with each operation of the resources it makes. Every
    operation is classified once: the wrapper forwards it, intercepts it,
    answers it itself, or does not offer it. The wrapper's dispatch is typed by
    this table, so a forwarded operation cannot ask about a fault and an
    intercepted one cannot skip it, and a test checks each class against what
    the operation does. *)

type forwarded = [ `Forwarded ]
type intercepted = [ `Intercepted ]
type synthesized = [ `Synthesized ]
type unsupported = [ `Unsupported ]

(** How the wrapper treats an operation. *)
type _ class_ =
  | Forwarded : forwarded class_
      (** The base answers, and the wrapper asks about no fault: it only wraps
          what the base returns and keeps its own state. *)
  | Intercepted : {
      kind : Op.kind;  (** What the function a wrapper asks sees. *)
      inet : Fault.t list;  (** The faults that change it over IP. *)
      unix : Fault.t list;  (** Those that change it over AF_UNIX. *)
    }
      -> intercepted class_
      (** The wrapper asks about a fault, and a fault may replace or cut what
          the base does; any other fault leaves it as the base does it. *)
  | Synthesized : synthesized class_
      (** The wrapper answers itself, without the base's own answer. *)
  | Unsupported : unsupported class_
      (** The wrapper does not offer it, where the base may. *)

type interface =
  [ `Network | `Listening_socket | `Stream_socket | `Datagram_socket ]
(** The interface an operation belongs to. *)

(** The operations, by the class of each. *)
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

(** An operation of any class. *)
type op = Op : _ t -> op

val all : op list
(** [all] is every operation, each once, in the order above. *)

val interface : _ t -> interface
(** [interface op] is the interface [op] belongs to. *)

val name : _ t -> string
(** [name op] is the function that runs [op], as its library names it. *)

val class_ : 'c t -> 'c class_
(** [class_ op] is how the wrapper treats [op]. *)

val doc : _ t -> string
(** [doc op] says, in whole sentences, what the wrapper adds to [op] or why it
    differs from the base, or is empty. *)

val kind : intercepted t -> Op.kind
(** [kind op] is the kind of the operations a wrapper asks about for [op]. *)
