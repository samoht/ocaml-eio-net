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
  | Tiny_buffers -> "every connection queues 4 KiB each way"
  | Outage -> "the outage: every connect refused, every datagram lost"

let rule_key r =
  Fmt.str "%s%d%s@%d.%s" (Op.kind_name r.kind) r.socket (Op.side_name r.side)
    r.at (Fault.key r.fault)

let key s =
  match (s.lasting, s.rules) with
  | Some l, _ -> lasting_name l
  | None, [] -> "none"
  | None, rs -> String.concat "+" (List.map rule_key rs)

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

let ops kind (s, side) (trace : trace) =
  List.filter_map
    (fun ((op : Op.t), _) ->
      if op.kind = kind && op.socket = s && op.side = side then Some op
      else None)
    trace

(* Whether a cut of end [e] at byte [b] fires in a run like [trace]: a read of
   [e] starts at or past [b], or one spans [b] and the peer wrote past it. *)
let reached trace ((s, side) as e) =
  let reads = ops `Read e trace in
  let extent =
    List.fold_left (fun m (op : Op.t) -> max m op.offset) (-1) reads
  in
  let sent =
    List.fold_left
      (fun m (op : Op.t) -> max m (op.offset + op.length))
      0
      (ops `Write (s, other side) trace)
  in
  let spans b (op : Op.t) = op.offset < b && b < op.offset + op.length in
  fun b -> b >= 0 && (b <= extent || (b < sent && List.exists (spans b) reads))

(* The writes of an end that make its structure: the first three, the last,
   and each whose length differs from the one before. *)
let structural writes =
  let a = Array.of_list writes in
  let n = Array.length a in
  List.filteri
    (fun i (op : Op.t) ->
      i < 3 || i = n - 1 || (i > 0 && a.(i - 1).Op.length <> op.length))
    writes

let around l = List.concat_map (fun b -> [ b - 1; b; b + 1 ]) l

(* Where a reader's bytes may be cut: around the ends of the peer's writes.
   [all] takes every write, not only the structural ones. *)
let read_positions ~all trace ((s, side) as e) =
  let w = ops `Write (s, other side) trace in
  let w = if all then w else structural w in
  0 :: around (List.map (fun (op : Op.t) -> op.offset + op.length) w)
  |> List.filter (reached trace e)
  |> List.sort_uniq compare

let write_positions ~all trace e =
  let w = ops `Write e trace in
  let top =
    List.fold_left (fun m (op : Op.t) -> max m (op.offset + op.length)) 0 w
  in
  let w = if all then w else structural w in
  around (List.map (fun (op : Op.t) -> op.offset) w)
  |> List.filter (fun b -> b >= 0 && b < top)
  |> List.sort_uniq compare

let byte_rules ~all trace =
  List.concat_map
    (fun ((s, side) as e) ->
      List.concat_map
        (fun at ->
          List.map
            (fun fault -> { kind = `Read; socket = s; side; at; fault })
            Fault.[ Reset; Eof; Hang ])
        (read_positions ~all trace e)
      @ List.map
          (fun at ->
            { kind = `Write; socket = s; side; at; fault = Fault.Reset })
          (write_positions ~all trace e))
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
      match op.kind with
      | `Connect -> [ r Refuse; r Timeout; r Unreachable ]
      | `Accept -> [ r Reset ]
      | `Recv -> [ r Drop; r Dup; r Reorder; r (Garble 12) ]
      | `Read | `Write -> [])
    trace

let singles trace = structural_rules trace @ byte_rules ~all:false trace

let linear trace =
  List.map (fun r -> { rules = [ r ]; lasting = None }) (singles trace)
  @ List.map (fun l -> { rules = []; lasting = Some l }) lastings

let others trace =
  let lin = singles trace in
  byte_rules ~all:true trace
  |> List.filter (fun r -> not (List.mem r lin))
  |> List.map (fun r -> { rules = [ r ]; lasting = None })

let fires_at (trace : trace) r =
  List.find_map
    (fun ((op : Op.t), _) ->
      if
        op.kind = r.kind && op.socket = r.socket && op.side = r.side
        && (op.offset = r.at
           || Op.is_bytes op.kind && op.offset < r.at
              && r.at < op.offset + op.length)
      then Some op.index
      else None)
    trace

(* Second faults after [r1], from the run [r1] changed. A benign one (a single
   byte, a 50 ms stall) counts as one of the two. *)
let seconds r1 ~first ~(after : trace) =
  match fires_at first r1 with
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
            match op.kind with
            | `Read | `Write -> [ r (Short 1) ]
            | `Connect | `Recv -> [ r (Stall 0.05) ]
            | `Accept -> [])
          after
      in
      List.filter_map
        (fun r2 ->
          match fires_at after r2 with
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

let faults s =
  let pending = ref s.rules in
  fun (op : Op.t) ->
    match (s.lasting, op.kind) with
    | Some Short_reads, `Read when op.offset < 4096 -> Some (Fault.Short 1)
    | Some Short_writes, `Write when op.offset < 4096 -> Some (Short 1)
    | Some Coalesce, `Read -> Some (Stall 0.005)
    | Some Outage, `Connect -> Some Refuse
    | Some Outage, `Recv -> Some Drop
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
