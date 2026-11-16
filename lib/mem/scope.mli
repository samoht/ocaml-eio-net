(** What eio-net.mem models of each kernel. Every behaviour and socket option it
    knows of is classified once, under Linux and under macOS. The README's
    section of the same name is made from this table, and a test checks each
    option's class against what a connection does. *)

(** How the network follows the kernel for a feature. Each text is whole
    sentences, said of that kernel. *)
type class_ =
  | Modelled  (** As the kernel behaves. *)
  | Approximated of string  (** Within the bound the text states. *)
  | Stored_only of string
      (** Set and read back as the kernel keeps it, with no other effect; the
          text says what is missing, if more than that. *)
  | Unsupported of string  (** Refused, with the error the text names. *)

(** An option and a value to set it to. *)
type sample = Sample : 'a Eio.Net.Sockopt.t * 'a -> sample

type feature = {
  name : string;  (** The feature, in Markdown. *)
  linux : class_;  (** Under Linux. *)
  macos : class_;  (** Under macOS. *)
  samples : sample list;
      (** One value of each settable option the feature names, to check its
          class with. *)
}
(** A behaviour or a socket option. *)

val behaviours : feature list
(** [behaviours] is how the network follows each behaviour of the kernel. *)

val options : feature list
(** [options] is how the network follows each socket option, and each argument
    of [listen] that stands for one. *)
