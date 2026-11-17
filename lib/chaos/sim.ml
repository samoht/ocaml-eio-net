open! Hermetic

type env =
  < net : [ `Generic ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t
  ; secure_random : Eio.Flow.source_ty Eio.Resource.t
  ; stdin : Eio.Flow.source_ty Eio.Resource.t
  ; stdout : Eio.Flow.sink_ty Eio.Resource.t
  ; stderr : Eio.Flow.sink_ty Eio.Resource.t
  ; domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
  ; debug : Eio.Debug.t
  ; backend_id : string >

(* {1 Entropy} *)

module Entropy = struct
  type t = Random.State.t

  let read_methods = []

  let single_read t buf =
    for i = 0 to Cstruct.length buf - 1 do
      Cstruct.set_uint8 buf i (Random.State.int t 256)
    done;
    Cstruct.length buf
end

let entropy seed =
  Eio.Resource.T (Random.State.make seed, Eio.Flow.Pi.source (module Entropy))

(* {1 Output} *)

type output = { buf : Buffer.t; mutable dropped : int }

let kept = 2048

(* The first place at or after [i] where a UTF-8 character starts, if one
   does within a character's length; [i] otherwise, as binary output has
   none. *)
let boundary s i =
  let len = String.length s in
  let rec go j =
    if j >= len then len
    else if j >= i + 4 then i
    else if Uchar.utf_decode_is_valid (String.get_utf_8_uchar s j) then j
    else go (j + 1)
  in
  go i

(* The last [kept] bytes of [t], or fewer to start on a character, and how
   many came before them. *)
let tail t =
  let s = Buffer.contents t.buf in
  let cut = boundary s (max 0 (String.length s - kept)) in
  (t.dropped + cut, String.sub s cut (String.length s - cut))

module Sink = struct
  type t = output

  (* The buffer holds at most twice [kept] bytes, then keeps its tail. *)
  let add t s =
    Buffer.add_string t.buf s;
    if Buffer.length t.buf > 2 * kept then begin
      let dropped, s = tail t in
      t.dropped <- dropped;
      Buffer.clear t.buf;
      Buffer.add_string t.buf s
    end

  let single_write t bufs =
    List.iter (fun b -> add t (Cstruct.to_string b)) bufs;
    Cstruct.lenv bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let output () = { buf = Buffer.create 256; dropped = 0 }
let printed = tail

let traceln t =
  {
    Eio.Debug.traceln =
      (fun ?__POS__:_ fmt ->
        Fmt.kstr (fun s -> Sink.add t ("+" ^ s ^ "\n")) fmt);
  }

(* {1 Domains and processes} *)

module Domains = struct
  type t = unit

  let run () fn =
    let cancelled, _ = Eio.Promise.create () in
    fn ~cancelled

  let run_raw () fn = fn ()
end

let domains = Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Domains))

module Processes = struct
  type tag = [ `Generic ]
  type t = unit

  let environment () = [||]
  let getenv_opt () _ = None
  let pipe () ~sw:_ = raise (Posix.refused "pipe" "")

  let spawn () ~sw:_ ?cwd:_ ?stdin:_ ?stdout:_ ?stderr:_ ?env:_ ?executable:_
      args =
    raise (Posix.refused "spawn" (String.concat " " args))
end

let processes = Eio.Resource.T ((), Eio.Process.Pi.mgr (module Processes))

(* {1 Environments} *)

let v ~net ~clock ~mono_clock ~seed out : env =
  let out = Eio.Resource.T (out, Eio.Flow.Pi.sink (module Sink)) in
  let random = entropy seed in
  let stdin = Eio.Flow.string_source "" in
  object
    method net = Lazy.force net
    method clock = clock
    method mono_clock = mono_clock
    method secure_random = random
    method stdin = stdin
    method stdout = out
    method stderr = out
    method domain_mgr = domains
    method process_mgr = processes
    method debug = Eio.Private.Debug.v
    method backend_id = "mock"
  end

let with_process ~seed out f =
  Random.full_init seed;
  Eio.Fiber.with_binding Eio.Private.Debug.v#traceln (traceln out) f
