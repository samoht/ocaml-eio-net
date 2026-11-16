(* Times TCP connects whose SYNs are all dropped, on the monotonic clock, and
   prints one "SECONDS,OUTCOME" line per connect, once all have ended.

   measure CLIENT N TARGET

   CLIENT is unix, a blocking Unix.connect per thread, or eio, Eio.Net.connect
   in a fiber, on the backend $EIO_BACKEND names. The N connects overlap,
   started [stagger] seconds apart: a kernel's timer wheel fires a timer at
   the end of a granule, of seconds for a long one, so how late a connect
   times out depends on when it started. TARGET is full, a listener on
   127.0.0.1 whose accept queue is full, so that the kernel drops each SYN to
   it, or IP:PORT, an address whose SYNs are dropped. *)

let full () =
  let l = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind l (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen l 0;
  let addr = Unix.getsockname l in
  let c = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.connect c addr;
  (match Unix.select [ l ] [] [] 5. with
  | [], _, _ -> failwith "the first connection never reached the queue"
  | _ -> ());
  (* Neither is closed nor accepted, so the queue stays full. *)
  addr

let target = function
  | "full" -> full ()
  | s -> (
      match String.rindex_opt s ':' with
      | Some i ->
          let ip = Unix.inet_addr_of_string (String.sub s 0 i) in
          let port = String.sub s (i + 1) (String.length s - i - 1) in
          ADDR_INET (ip, int_of_string port)
      | None -> failwith ("measure: bad target " ^ s))

let outcome = function
  | Unix.Unix_error (Unix.ETIMEDOUT, _, _)
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ETIMEDOUT, _, _)), _)
  | Eio.Io (Eio.Net.E (Connection_failure Timeout), _) ->
      "ETIMEDOUT"
  | e ->
      String.map (function ',' | '\n' -> ' ' | c -> c) (Printexc.to_string e)

let stagger = 0.7

let timed f =
  let t = Mtime_clock.counter () in
  let r = match f () with () -> "connected" | exception e -> outcome e in
  (Mtime.Span.to_float_ns (Mtime_clock.count t) /. 1e9, r)

let unix n addr =
  let results = Array.make n (0., "") in
  let connect i =
    Thread.delay (stagger *. float i);
    let s = Unix.socket PF_INET SOCK_STREAM 0 in
    results.(i) <- timed (fun () -> Unix.connect s addr);
    Unix.close s
  in
  List.init n (Thread.create connect) |> List.iter Thread.join;
  Array.to_list results

let eio n addr =
  let addr =
    match addr with
    | Unix.ADDR_INET (ip, port) -> `Tcp (Eio_unix.Net.Ipaddr.of_unix ip, port)
    | ADDR_UNIX _ -> assert false
  in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Fiber.List.map
    (fun i ->
      Eio.Time.sleep env#clock (stagger *. float i);
      Eio.Switch.run @@ fun sw ->
      timed (fun () -> Eio.Flow.close (Eio.Net.connect ~sw net addr)))
    (List.init n Fun.id)

let () =
  match Sys.argv with
  | [| _; client; n; t |] ->
      let run =
        match client with "unix" -> unix | "eio" -> eio | _ -> exit 2
      in
      run (int_of_string n) (target t)
      |> List.iter (fun (s, r) -> Printf.printf "%.3f,%s\n" s r)
  | _ ->
      prerr_endline "usage: measure (unix | eio) N (full | IP:PORT)";
      exit 2
