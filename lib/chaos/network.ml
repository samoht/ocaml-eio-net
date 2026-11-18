open! Hermetic

(* {1 Faults} *)

type lasting = Short_reads | Short_writes | Coalesce | Tiny_buffers | Outage

let tiny_buffer = 32768
let one_byte = 4096

let lasting_name = function
  | Short_reads -> "one-byte-reads"
  | Short_writes -> "one-byte-writes"
  | Coalesce -> "coalesced-reads"
  | Tiny_buffers -> "tiny-buffers"
  | Outage -> "outage"

let lasting_text = function
  | Short_reads -> "every read of the first 4 KiB returns one byte"
  | Short_writes -> "every write of the first 4 KiB takes one byte"
  | Coalesce -> "every read waits 5 ms, so it takes what several writes sent"
  | Tiny_buffers ->
      "every connection queues 32 KiB each way, and a write takes 32 KiB at \
       most"
  | Outage -> "the outage: every connect refused, every datagram over IP lost"

(* What the network proposes at an operation: a fault, or a condition that
   holds for the whole run. *)
type choice = Fault of Fault.t | Lasting of lasting

let choice_name = function
  | Fault f -> Fault.key f
  | Lasting l -> lasting_name l

let pp_choice ppf = function
  | Fault f -> Fault.pp ppf f
  | Lasting l -> Fmt.string ppf (lasting_text l)

(* A datagram or a connect held back, a read or a write that moves one byte,
   and the noise in place of a datagram. *)
let late = Fault.Stall 0.05
let mild = Fault.Short 1
let noise = Fault.Garble 12

(* The faults the kernel allows at an operation, alone, and as the mild
   second of a pair. A Unix-domain connect has no handshake to lose and no
   route to miss, a Unix-domain client that leaves first closes, and a
   Unix-domain datagram is never lost, duplicated, reordered or garbled
   (unix(7)); any datagram may arrive late, alone. A Unix-domain read
   returns what is queued, up to its buffer, and a write queues whole. *)
let faults (op : Op.t) : Fault.t list * Fault.t list =
  match (op.kind, op.family) with
  | `Connect, `Inet -> ([ Refuse; Timeout; Unreachable ], [ late ])
  | `Connect, `Unix -> ([ Refuse ], [ late ])
  | `Accept, `Inet -> ([ Reset ], [])
  | `Accept, `Unix -> ([ Eof ], [])
  | `Read, `Inet -> ([ Reset; Eof; Hang ], [ mild ])
  | `Read, `Unix -> ([ Reset; Eof; Hang ], [])
  | `Write, `Inet -> ([ Reset; Eof ], [ mild ])
  | `Write, `Unix -> ([ Reset; Eof ], [])
  | `Recv, `Inet -> ([ Drop; Dup; Reorder; noise; late ], [])
  | `Recv, `Unix -> ([ late ], [])

(* What a condition does to an operation. *)
let condition l (op : Op.t) =
  match (l, op.kind) with
  | Short_reads, `Read when op.offset < one_byte && op.family = `Inet ->
      Some (Fault.Short 1)
  | Short_writes, `Write when op.offset < one_byte && op.family = `Inet ->
      Some (Short 1)
  | Coalesce, `Read -> Some (Stall 0.005)
  | Tiny_buffers, `Write when op.length > tiny_buffer ->
      Some (Short tiny_buffer)
  | Outage, `Connect -> Some Refuse
  | Outage, `Recv when op.family = `Inet -> Some Drop
  | _ -> None

(* The operations a condition multiplies: the reads a one-byte condition
   splits a read into, of the first 4 KiB of an end over IP, under one-byte
   reads, and the reads and writes under one-byte writes, as the peer of a
   one-byte write reads one byte at a time; and under the outage the
   datagrams a receive takes and loses before the one it returns. *)
let split l (op : Op.t) =
  op.family = `Inet
  &&
  match (l, op.kind) with
  | Short_reads, `Read | Short_writes, (`Read | `Write) -> op.offset < one_byte
  | Outage, `Recv -> true
  | _ -> false

let generic (op : Op.t) : Eio_chaos.Op.t =
  {
    device = "net";
    kind = Op.kind_name op.kind;
    target = Fmt.str "%d%s" op.socket (Op.side_name op.side);
    instance = (match op.kind with `Recv -> "" | _ -> string_of_int op.socket);
    offset = op.offset;
    length = op.length;
    detail = (match op.family with `Unix -> "unix" | `Inet -> "");
    structure = (match op.kind with `Connect | `Accept -> Every | _ -> Thin);
    changes = Never;
  }

let pp_op ppf (op : Eio_chaos.Op.t) =
  match op.kind with
  | "read" | "write" | "recv" ->
      Fmt.pf ppf "%s %s @%d+%d" op.kind op.target op.offset op.length
  | k -> Fmt.pf ppf "%s %s @%d" k op.target op.offset

(* {1 The part} *)

type t = {
  mutable os : Eio_chaos.os;
  mutable chaos : Eio_chaos.t;
  mutable sw : Eio.Switch.t option;
  mutable clock : float Eio.Time.clock_ty Eio.Resource.t option;
  mutable mono : Eio.Time.Mono.ty Eio.Resource.t option;
  mutable base : [ `Generic ] Eio.Net.ty Eio.Resource.t option;
  mutable lasting : lasting option; (* the condition of this run *)
  mutable listed : bool; (* the run's conditions were proposed *)
  mutable listed_bytes : bool; (* and those over IP streams *)
}

let v () =
  {
    os = `Linux;
    chaos = Eio_chaos.none;
    sw = None;
    clock = None;
    mono = None;
    base = None;
    lasting = None;
    listed = false;
    listed_bytes = false;
  }

let noun = "network operation"

(* The conditions proposed at an operation: those of any network at its first
   one, the one-byte ones at its first read or write over IP. Each holds for
   the whole run, which [run] learns from its faults. *)
let conditions t (op : Op.t) =
  let any =
    if t.listed then []
    else begin
      t.listed <- true;
      [ Coalesce; Tiny_buffers; Outage ]
    end
  in
  let bytes =
    if t.listed_bytes || not (Op.is_bytes op.kind && op.family = `Inet) then []
    else begin
      t.listed_bytes <- true;
      [ Short_reads; Short_writes ]
    end
  in
  List.map (fun l -> Lasting l) (any @ bytes)

let hook t (op : Op.t) =
  let once, mild = faults op in
  let fault l = List.map (fun f -> Fault f) l in
  let split = Option.fold ~none:false ~some:(fun l -> split l op) t.lasting in
  let asked =
    Eio_chaos.decide t.chaos (generic op) ~name:choice_name ~pp:pp_choice
      ~lasting:(conditions t op) ~mild:(fault mild) ~split (fault once)
  in
  match Option.bind t.lasting (fun l -> condition l op) with
  | Some f -> Some (f, 0)
  | None -> (
      match asked with
      | Some (Fault f) -> Some (f, Eio_chaos.place t.chaos)
      | Some (Lasting _) | None -> None)

let get = Option.get

let wrapped t ~outage =
  Eio_chaos.Net
    (Wrapper.v ~os:t.os ~chaos:t.chaos ~clock:(get t.clock) ~outage (hook t)
       (get t.base))

(* A connection queues 32 KiB each way, or the kernel's default for its
   family if that is smaller, while tiny buffers hold. *)
let capacity t c =
  match t.lasting with Some Tiny_buffers -> Int.min tiny_buffer c | _ -> c

let fresh t =
  t.base <-
    Some
      (Eio_net_mem.Private.v ~os:t.os ~capacity:(capacity t) ~mono:(get t.mono)
         ~sw:(get t.sw) ())

let start t ~os ~sw ~clock ~mono_clock chaos =
  t.os <- os;
  t.chaos <- chaos;
  t.sw <- Some sw;
  t.clock <- Some clock;
  t.mono <- Some mono_clock;
  t.lasting <- None;
  t.listed <- false;
  t.listed_bytes <- false;
  fresh t;
  Eio_chaos.Net (get t.base)

let run t ~cut:_ ~faults =
  t.lasting <-
    List.find_opt
      (fun l -> List.mem (lasting_name l) faults)
      [ Short_reads; Short_writes; Coalesce; Tiny_buffers; Outage ];
  t.listed <- false;
  t.listed_bytes <- false;
  wrapped t ~outage:(t.lasting = Some Outage)

let cut _ = []

let restart t ~power ~cut:_ =
  t.lasting <- None;
  if power then fresh t;
  wrapped t ~outage:false

let indeterminate = function
  | End_of_file | Eio.Time.Timeout -> true
  | _ -> false

module Part = Eio_chaos.Part (struct
  type nonrec t = t

  let name = "net"
  let noun = noun
  let start = start
  let run = run
  let cut = cut
  let restart = restart
  let pp_op = pp_op
  let indeterminate = indeterminate
end)

let part () = Part.v (v ())
