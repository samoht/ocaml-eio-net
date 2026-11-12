open! Hermetic

type rule = {
  kind : Op.kind;
  socket : int;
  side : Op.side;
  at : int;
  fault : Fault.t;
}

type lasting = Short_reads | Short_writes | Coalesce | Tiny_buffers | Outage
type scenario = { rules : rule list; lasting : lasting option }
type trace = (Op.t * Fault.t option) list

let none = { rules = []; lasting = None }
let tiny_buffer = 32768
let lastings = [ Short_reads; Short_writes; Coalesce; Tiny_buffers; Outage ]

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

let rule_key r =
  Fmt.str "%s%d%s@%d.%s" (Op.kind_name r.kind) r.socket (Op.side_name r.side)
    r.at (Fault.key r.fault)

let key s =
  match (s.lasting, s.rules) with
  | Some l, _ -> lasting_name l
  | None, [] -> "none"
  | None, rs -> String.concat "+" (List.map rule_key rs)

let is_number s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* A datagram or a connect held back, a read or a write that moves one byte,
   and the noise in place of a datagram. *)
let late = Fault.Stall 0.05
let mild = Fault.Short 1
let noise = Fault.Garble 12

(* The faults the plans give a rule of each kind, alone or as the mild
   second of a pair, over either family. *)
let planned : Op.kind -> Fault.t list = function
  | `Connect -> [ Refuse; Timeout; Unreachable; late ]
  | `Accept -> [ Reset; Eof ]
  | `Read -> [ Reset; Eof; Hang; mild ]
  | `Write -> [ Reset; Eof; mild ]
  | `Recv -> [ Drop; Dup; Reorder; noise; late ]

let number s = if is_number s then int_of_string_opt s else None

(* KIND SOCKET SIDE @ AT . FAULT, as [rule_key] writes it, of a rule the
   plans make: a socket from 1, the side of its kind, and one of its
   faults. *)
let rule_of_key r =
  match String.index_opt r '@' with
  | None -> None
  | Some i -> (
      let head = String.sub r 0 i
      and tail = String.sub r (i + 1) (String.length r - i - 1) in
      match String.index_opt tail '.' with
      | None -> None
      | Some j ->
          let at = String.sub tail 0 j
          and fault = String.sub tail (j + 1) (String.length tail - j - 1) in
          let n = String.length head in
          let side =
            if n = 0 then None
            else
              match head.[n - 1] with
              | 'c' -> Some `Client
              | 's' -> Some `Server
              | _ -> None
          in
          List.find_map
            (fun kind ->
              let p = Op.kind_name kind in
              let m = String.length p in
              let own =
                match kind with
                | `Connect | `Recv -> `Client
                | `Accept -> `Server
                | `Read | `Write -> Option.value side ~default:`Client
              in
              match (side, number at) with
              | Some side, Some at
                when side = own && n > m && String.starts_with ~prefix:p head
                -> (
                  match number (String.sub head m (n - 1 - m)) with
                  | Some socket when socket >= 1 ->
                      List.find_opt
                        (fun f -> Fault.key f = fault)
                        (planned kind)
                      |> Option.map (fun fault ->
                          { kind; socket; side; at; fault })
                  | _ -> None)
              | _ -> None)
            [ `Connect; `Accept; `Read; `Write; `Recv ])

let is_key k =
  k = "none"
  || List.exists (fun l -> lasting_name l = k) lastings
  ||
  match List.map rule_of_key (String.split_on_char '+' k) with
  | [ Some _ ] -> true
  | [ Some a; Some b ] ->
      (a.kind, a.socket, a.side, a.at) <> (b.kind, b.socket, b.side, b.at)
  | _ -> false

let rule_text r =
  Fmt.str "%s of %d%s at %d %a" (Op.kind_name r.kind) r.socket
    (Op.side_name r.side) r.at Fault.pp r.fault

let text s =
  match (s.lasting, s.rules) with
  | Some l, _ -> lasting_text l
  | None, [] -> "none"
  | None, rs -> String.concat "; then " (List.map rule_text rs)

let other = function `Client -> `Server | `Server -> `Client

let ends (trace : trace) =
  List.sort_uniq compare
    (List.filter_map
       (fun ((op : Op.t), _) ->
         if Op.is_bytes op.kind then Some (op.socket, op.side) else None)
       trace)

(* The operations of a trace by kind and end, each in order, so that what
   one end did costs a lookup, not a pass over the whole trace. *)
type index = (Op.kind * int * Op.side, Op.t list) Hashtbl.t

let index (trace : trace) : index =
  let h = Hashtbl.create 64 in
  List.iter
    (fun ((op : Op.t), _) ->
      let k = (op.kind, op.socket, op.side) in
      Hashtbl.replace h k (op :: Option.value ~default:[] (Hashtbl.find_opt h k)))
    (List.rev trace);
  h

let looked = Atomic.make 0
let looked_at () = Atomic.get looked

let ops ix kind (s, side) =
  let l = Option.value ~default:[] (Hashtbl.find_opt ix (kind, s, side)) in
  ignore (Atomic.fetch_and_add looked (List.length l) : int);
  l

(* Whether a cut of end [e] at byte [b] fires in a run like [trace]: a read of
   [e] starts at or past [b], or one spans [b] and the peer wrote past it. *)
let reached ix ((s, side) as e) =
  let reads = ops ix `Read e in
  let extent =
    List.fold_left (fun m (op : Op.t) -> max m op.offset) (-1) reads
  in
  let sent =
    List.fold_left
      (fun m (op : Op.t) -> max m (op.offset + op.length))
      0
      (ops ix `Write (s, other side))
  in
  let spans b (op : Op.t) = op.offset < b && b < op.offset + op.length in
  fun b -> b >= 0 && (b <= extent || (b < sent && List.exists (spans b) reads))

(* The length class of a write: its length rounded down to a power of two. *)
let length_class n =
  let rec go c n = if n <= 1 then c else go (c + 1) (n lsr 1) in
  go 0 n

(* The writes of an end that make its structure: the first three, the last,
   and the first of each length class, so an exchange of many messages of
   varying length costs runs by its classes, not its messages. *)
let structural writes =
  let n = List.length writes in
  let seen = Hashtbl.create 8 in
  List.filteri
    (fun i (op : Op.t) ->
      let c = length_class op.length in
      let first = not (Hashtbl.mem seen c) in
      Hashtbl.replace seen c ();
      i < 3 || i = n - 1 || first)
    writes

let around l = List.concat_map (fun b -> [ b - 1; b; b + 1 ]) l

(* The address family of an end. *)
let family ix e =
  match ops ix `Read e @ ops ix `Write e with
  | (op : Op.t) :: _ -> op.family
  | [] -> `Inet

(* Over IP a cut may land a byte either side of a write's end, as TCP splits
   and joins segments. A Unix-domain stream queues each small write whole
   (unix(7)), so a cut lands between two writes. *)
let near ix e l = match family ix e with `Inet -> around l | `Unix -> l

(* Where a reader's bytes may be cut: around the ends of the peer's writes.
   [all] takes every write, not only the structural ones. *)
let read_positions ~all ix ((s, side) as e) =
  let w = ops ix `Write (s, other side) in
  let w = if all then w else structural w in
  0 :: near ix e (List.map (fun (op : Op.t) -> op.offset + op.length) w)
  |> List.filter (reached ix e)
  |> List.sort_uniq compare

let write_positions ~all ix e =
  let w = ops ix `Write e in
  let top =
    List.fold_left (fun m (op : Op.t) -> max m (op.offset + op.length)) 0 w
  in
  let w = if all then w else structural w in
  (* Around the start of each write and, over IP, the last byte of the
     stream, where a writer that closes cuts its last message. *)
  let starts = near ix e (List.map (fun (op : Op.t) -> op.offset) w) in
  (match family ix e with `Inet -> (top - 1) :: starts | `Unix -> starts)
  |> List.filter (fun b -> b >= 0 && b < top)
  |> List.sort_uniq compare

let byte_rules ~all trace =
  let ix = index trace in
  List.concat_map
    (fun ((s, side) as e) ->
      List.concat_map
        (fun at ->
          List.map
            (fun fault -> { kind = `Read; socket = s; side; at; fault })
            Fault.[ Reset; Eof; Hang ])
        (read_positions ~all ix e)
      @ List.concat_map
          (fun at ->
            List.map
              (fun fault -> { kind = `Write; socket = s; side; at; fault })
              Fault.[ Reset; Eof ])
          (write_positions ~all ix e))
    (ends trace)

let structural_rules (trace : trace) =
  List.concat_map
    (fun ((op : Op.t), _) ->
      let r fault =
        {
          kind = op.kind;
          socket = op.socket;
          side = op.side;
          at = op.offset;
          fault;
        }
      in
      (* A Unix-domain connect has no handshake to lose and no route to
         miss, a Unix-domain client that leaves first closes, and a
         Unix-domain datagram is never lost, duplicated, reordered or
         garbled (unix(7)); any datagram may arrive late, alone. *)
      match (op.kind, op.family) with
      | `Connect, `Inet -> [ r Refuse; r Timeout; r Unreachable ]
      | `Connect, `Unix -> [ r Refuse ]
      | `Accept, `Inet -> [ r Reset ]
      | `Accept, `Unix -> [ r Eof ]
      | `Recv, `Inet -> [ r Drop; r Dup; r Reorder; r noise; r late ]
      | `Recv, `Unix -> [ r late ]
      | (`Read | `Write), _ -> [])
    trace

let singles trace = structural_rules trace @ byte_rules ~all:false trace

(* One-byte reads and writes happen over IP only: a Unix-domain read returns
   what is queued, up to its buffer, and a write queues whole. *)
let applies (trace : trace) = function
  | Short_reads | Short_writes ->
      List.exists
        (fun ((op : Op.t), _) -> Op.is_bytes op.kind && op.family = `Inet)
        trace
  | Coalesce | Tiny_buffers | Outage -> true

let linear trace =
  List.map (fun r -> { rules = [ r ]; lasting = None }) (singles trace)
  @ List.filter_map
      (fun l ->
        if applies trace l then Some { rules = []; lasting = Some l } else None)
      lastings

let others trace =
  let lin = Hashtbl.create 64 in
  List.iter (fun r -> Hashtbl.replace lin r ()) (singles trace);
  byte_rules ~all:true trace
  |> List.filter (fun r -> not (Hashtbl.mem lin r))
  |> List.map (fun r -> { rules = [ r ]; lasting = None })

let fires_at ix r =
  List.find_map
    (fun (op : Op.t) ->
      if
        op.offset = r.at
        || Op.is_bytes op.kind && op.offset < r.at
           && r.at < op.offset + op.length
      then Some op.index
      else None)
    (ops ix r.kind (r.socket, r.side))

(* Second faults after [r1], from the run [r1] changed. A benign one (a single
   byte, a 50 ms stall) counts as one of the two. *)
let seconds r1 ~first ~(after : trace) =
  let ix = index after in
  match fires_at (index first) r1 with
  | None -> []
  | Some i1 ->
      let benign =
        List.concat_map
          (fun ((op : Op.t), _) ->
            let r fault =
              {
                kind = op.kind;
                socket = op.socket;
                side = op.side;
                at = op.offset;
                fault;
              }
            in
            match (op.kind, op.family) with
            | (`Read | `Write), `Inet -> [ r mild ]
            | (`Read | `Write), `Unix -> []
            | (`Connect | `Recv), _ -> [ r late ]
            | `Accept, _ -> [])
          after
      in
      List.filter_map
        (fun r2 ->
          match fires_at ix r2 with
          | Some i2 when i2 > i1 -> Some { rules = [ r1; r2 ]; lasting = None }
          | _ -> None)
        (singles after @ benign)

let spread n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i -> a.(i * len / n))

let sampled_share ~budget ~linear ~classes =
  let spend = min (budget - linear) (max 200 (linear / 2)) in
  max 0 spend / max 1 classes

let one_byte = 4096

let split s (op : Op.t) =
  op.offset < one_byte && op.family = `Inet
  &&
  match (s.lasting, op.kind) with
  | Some Short_reads, `Read | Some Short_writes, (`Read | `Write) -> true
  | _ -> false

let faults s =
  let pending = ref s.rules in
  fun (op : Op.t) ->
    match (s.lasting, op.kind) with
    | Some Short_reads, `Read when op.offset < one_byte && op.family = `Inet ->
        Some (Fault.Short 1)
    | Some Short_writes, `Write when op.offset < one_byte && op.family = `Inet
      ->
        Some (Short 1)
    | Some Coalesce, `Read -> Some (Stall 0.005)
    | Some Tiny_buffers, `Write when op.length > tiny_buffer ->
        Some (Short tiny_buffer)
    | Some Outage, `Connect -> Some Refuse
    | Some Outage, `Recv when op.family = `Inet -> Some Drop
    | Some _, _ -> None
    | None, _ -> (
        match !pending with
        | r :: rest
          when r.kind = op.kind && r.socket = op.socket && r.side = op.side ->
            if op.offset = r.at then begin
              pending := rest;
              Some r.fault
            end
            else if
              Op.is_bytes op.kind && op.offset < r.at
              && r.at < op.offset + op.length
            then Some (Short (r.at - op.offset))
            else None
        | _ -> None)
