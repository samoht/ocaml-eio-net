open! Hermetic

(* The Unix-domain paths that hold a socket file, shared by stream and
   datagram sockets as a file system shares them. A socket's file stays once
   it closes, [Stale], as Eio's backends leave a datagram socket's file, until
   it is removed. *)
type kind = Listener | Datagram | Client | Stale

(* A path, and for an abstract address whether its socket is a stream
   one. *)
type t = {
  mutex : Mutex.t;
  files : (string * bool option, kind) Hashtbl.t;
  mutable autobind : int;
}

let v () = { mutex = Mutex.create (); files = Hashtbl.create 8; autobind = 0 }

(* A path that starts with a NUL is in Linux's abstract namespace, which has
   no files, and a name space per socket type. *)
let abstract path = String.length path > 0 && path.[0] = '\000'
let key ~stream path = (path, if abstract path then Some stream else None)
let is_stream = function Datagram -> false | Listener | Client | Stale -> true

(* What is at [path] for a socket of the type [stream] says: at an abstract
   address, only a socket of that type. *)
let find t ~stream path =
  Mutex.protect t.mutex (fun () -> Hashtbl.find_opt t.files (key ~stream path))

(* The empty path binds an abstract address no socket holds, as Linux
   autobinds one: a NUL and five hexadecimal digits. *)
let autobind t =
  Mutex.protect t.mutex @@ fun () ->
  let rec next () =
    let name = Fmt.str "\000%05x" t.autobind in
    t.autobind <- (t.autobind + 1) land 0xfffff;
    let held stream = Hashtbl.mem t.files (key ~stream name) in
    if held true || held false then next () else name
  in
  next ()

(* [path], or for the empty path the address it autobinds. *)
let bound t path = if path = "" then autobind t else path

(* Binds [path] unless a file is there; [take_over] binds over a file, and
   an abstract address has none. *)
let bind t ?(take_over = false) path kind =
  let k = key ~stream:(is_stream kind) path in
  Mutex.protect t.mutex @@ fun () ->
  ((take_over && not (abstract path)) || not (Hashtbl.mem t.files k))
  && begin
    Hashtbl.replace t.files k kind;
    true
  end

(* The socket of [kind] at [path] closes and leaves its file, or with
   [remove], or at an abstract address, takes it away. *)
let leave ?(remove = false) t path kind =
  let k = key ~stream:(is_stream kind) path in
  Mutex.protect t.mutex @@ fun () ->
  match Hashtbl.find_opt t.files k with
  | Some k' when k' = kind ->
      if remove || abstract path then Hashtbl.remove t.files k
      else Hashtbl.replace t.files k Stale
  | Some _ | None -> ()
