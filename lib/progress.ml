open! Hermetic

exception Too_slow of { min_rate : int; window : Mtime.Span.t; moved : int }

let () =
  Printexc.register_printer @@ function
  | Too_slow { min_rate; window; moved } ->
      Fmt.kstr Option.some
        "Eio_net.Progress: transfer too slow: %d bytes in %a, under %d B/s"
        moved Mtime.Span.pp window min_rate
  | _ -> None

type t = {
  now : unit -> Mtime.t;
  sleep : Mtime.Span.t -> unit;
  min_rate : int;
  window : Mtime.Span.t;
  mutable start : Mtime.t option;
      (* the current window's start, from the transfer's first move *)
  mutable moved : int;
}

let v ~mono ~min_rate ~window =
  if min_rate <= 0 then
    Fmt.invalid_arg "Eio_net.Progress.v: min_rate is %d, it must be positive"
      min_rate;
  if Mtime.Span.equal window Mtime.Span.zero then
    invalid_arg "Eio_net.Progress.v: window is 0, it must be positive";
  {
    now = (fun () -> Eio.Time.Mono.now mono);
    sleep = Eio.Time.Mono.sleep_span mono;
    min_rate;
    window;
    start = None;
    moved = 0;
  }

(* The bytes a window must move: [min_rate] a second over [window]. *)
let needed t = Mtime.Span.to_float_ns t.window /. 1e9 *. float_of_int t.min_rate

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
        raise
          (Too_slow
             { min_rate = t.min_rate; window = t.window; moved = t.moved });
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

(* [f] runs once, in a fiber of its own: cancelling a write that has sent
   part of its bytes and calling it again would send them twice. Each window
   is judged while it waits, and only [Too_slow] cancels it. *)
let move t f =
  Eio.Switch.run @@ fun sw ->
  let moved = Eio.Fiber.fork_promise ~sw f in
  let rec go () =
    let start = start t in
    match
      Eio.Fiber.first
        ~combine:(fun a b -> match a with `Moved _ -> a | `Quiet -> b)
        (fun () -> `Moved (Eio.Promise.await_exn moved))
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

let pp ppf t =
  Fmt.pf ppf "<progress: %d B/s over %a>" t.min_rate Mtime.Span.pp t.window

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
