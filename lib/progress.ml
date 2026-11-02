open! Hermetic

exception Too_slow of { limit : int; window : Mtime.Span.t }

let () =
  Printexc.register_printer @@ function
  | Too_slow { limit; window } ->
      Some
        (Fmt.str
           "Operation too slow. Less than %d bytes/sec transferred the last %g \
            seconds"
           limit
           (Int64.to_float (Mtime.Span.to_uint64_ns window) /. 1e9))
  | _ -> None

type t = {
  now : unit -> Mtime.t;
  sleep : Mtime.Span.t -> unit;
  limit : int;
  window : Mtime.Span.t;
  mutable start : Mtime.t option;
      (* the current window's start, from the transfer's first move *)
  mutable moved : int;
}

let v ~mono ~limit ~window =
  if limit <= 0 then invalid_arg "Progress.v: the limit is not positive";
  if Mtime.Span.equal window Mtime.Span.zero then
    invalid_arg "Progress.v: the window is zero";
  {
    now = (fun () -> Eio.Time.Mono.now mono);
    sleep = Eio.Time.Mono.sleep_span mono;
    limit;
    window;
    start = None;
    moved = 0;
  }

(* The bytes a window must move: [limit] a second over [window]. *)
let needed t =
  Int64.to_float (Mtime.Span.to_uint64_ns t.window)
  /. 1e9 *. float_of_int t.limit

(* What is left of the current window, zero once it has ended. *)
let left t start =
  let since = Mtime.span start (t.now ()) in
  if Mtime.Span.compare since t.window >= 0 then Mtime.Span.zero
  else Mtime.Span.abs_diff t.window since

(* Judge the window that started at [start] if it has ended, and start the
   next. Moves of several fibers share [t], so the window may have been judged
   and replaced by another's move already: it is then not judged again, and
   the caller goes on in the current one. *)
let judge t start =
  match t.start with
  | Some current
    when Mtime.equal current start
         && Mtime.Span.equal (left t start) Mtime.Span.zero ->
      if float_of_int t.moved < needed t then
        raise (Too_slow { limit = t.limit; window = t.window });
      t.start <- Some (t.now ());
      t.moved <- 0
  | Some _ | None -> ()

let start t =
  match t.start with
  | Some s -> s
  | None ->
      let s = t.now () in
      t.start <- Some s;
      s

let move t f =
  let rec go () =
    let start = start t in
    match
      Eio.Fiber.first
        ~combine:(fun a b -> match a with `Moved _ -> a | `Quiet -> b)
        (fun () -> `Moved (f ()))
        (fun () ->
          t.sleep (left t start);
          `Quiet)
    with
    | `Moved n ->
        t.moved <- t.moved + n;
        judge t start;
        n
    | `Quiet ->
        judge t start;
        go ()
  in
  go ()

let reader t r =
  let open Bytesrw in
  Bytes.Reader.make ~slice_length:(Bytes.Reader.slice_length r) @@ fun () ->
  let slice = ref Bytes.Slice.eod in
  let (_ : int) =
    move t (fun () ->
        slice := Bytes.Reader.read r;
        Bytes.Slice.length !slice)
  in
  !slice

let flow t (f : _ Eio.Flow.two_way) =
  let f = (f :> Eio.Flow.two_way_ty Eio.Resource.t) in
  let module Flow = struct
    type t = Eio.Flow.two_way_ty Eio.Resource.t

    let read_methods = []

    let single_read inner buf =
      move t (fun () -> Eio.Flow.single_read inner buf)

    let single_write inner bufs =
      move t (fun () -> Eio.Flow.single_write inner bufs)

    let copy inner ~src = Eio.Flow.Pi.simple_copy ~single_write inner ~src
    let shutdown inner how = Eio.Flow.shutdown inner how
  end in
  Eio.Resource.T (f, Eio.Flow.Pi.two_way (module Flow))
