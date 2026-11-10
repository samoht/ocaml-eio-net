(* Review 2: probes of the one-sided reset, coalesced reads and the budget. *)

module R = Eio.Buf_read

let strict_line = App.strict_line
let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9100)

(* One-sided reset: a client drops a reset connection without closing it;
   the test waits for the server's handlers to end. On TCP the handler of
   that connection reads the reset at once. *)
let one_sided () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  let done_, set_done = Eio.Promise.create () in
  let server () =
    Eio.Switch.run @@ fun hsw ->
    Eio.Fiber.first
      (fun () ->
        while true do
          let flow, _ = Eio.Net.accept ~sw:hsw l in
          Eio.Fiber.fork ~sw:hsw (fun () ->
              let r = R.of_flow ~max_size:256 flow in
              try
                while true do
                  let _ = strict_line r in
                  Eio.Flow.copy_string "ok\n" flow
                done
              with End_of_file | Eio.Io _ | Failure _ ->
                Eio.Resource.close flow)
        done)
      (fun () -> Eio.Promise.await done_)
  in
  let client () =
    let conn = ref None in
    let get () =
      let rec go n =
        match
          Eio.Time.with_timeout_exn env#clock 5. (fun () ->
              let f, r =
                match !conn with
                | Some c -> c
                | None ->
                    let f = Eio.Net.connect ~sw env#net addr in
                    let c = (f, R.of_flow ~max_size:256 f) in
                    conn := Some c;
                    c
              in
              Eio.Flow.copy_string "q\n" f;
              strict_line r)
        with
        | v -> Ok v
        | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _) ->
            conn := None;
            if n < 3 then go (n + 1) else Error ()
      in
      go 1
    in
    for _ = 1 to 3 do
      ignore (get ())
    done;
    (match !conn with Some (f, _) -> Eio.Resource.close f | None -> ());
    Eio.Promise.resolve set_done ()
  in
  Eio.Fiber.both server client

(* Coalescing: the server handles fragmented requests, but takes one
   request per read and drops what follows. The client pipelines two
   requests, with a pause between them. *)
let coalesce ?(bug = true) () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let buf = Cstruct.create 256 and acc = Buffer.create 16 in
          let rec loop () =
            let n = Eio.Flow.single_read flow buf in
            Buffer.add_string acc (Cstruct.to_string ~len:n buf);
            let s = Buffer.contents acc in
            match String.index_opt s '\n' with
            | None -> loop ()
            | Some i ->
                Buffer.clear acc;
                if not bug then
                  Buffer.add_string acc
                    (String.sub s (i + 1) (String.length s - i - 1));
                Eio.Flow.copy_string
                  (String.uppercase_ascii (String.sub s 0 (i + 1)))
                  flow;
                loop ()
          in
          try loop () with End_of_file -> ()));
  match
    let f = Eio.Net.connect ~sw env#net addr in
    Eio.Flow.copy_string "a\n" f;
    Eio.Time.sleep env#clock 0.001;
    Eio.Flow.copy_string "b\n" f;
    let r = R.of_flow ~max_size:256 f in
    Eio.Time.with_timeout_exn env#clock 5. (fun () ->
        let x = strict_line r in
        let y = strict_line r in
        (x, y))
  with
  | "A", "B" -> ()
  | x, y -> Fmt.failwith "got %S %S" x y
  | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _) -> ()

(* The budget: a download of [n] chunks of 100 bytes. *)
let stream n () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string (Fmt.str "%d\n" (n * 100)) flow;
          for i = 1 to n do
            Eio.Flow.copy_string
              (String.make 100 (Char.chr (65 + (i mod 26))))
              flow
          done));
  match
    let f = Eio.Net.connect ~sw env#net addr in
    let r = R.of_flow ~max_size:1_000_000 f in
    Eio.Time.with_timeout_exn env#clock 5. (fun () ->
        let len = int_of_string (strict_line r) in
        R.take len r)
  with
  | body -> if String.length body <> n * 100 then failwith "short body"
  | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _) -> ()

(* Added: the same server, with a client that retries on a new connection
   and so may demand success under at most two faults. *)
let coalesce_retry ?(bug = true) () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          let buf = Cstruct.create 256 and acc = Buffer.create 16 in
          let rec loop () =
            let n = Eio.Flow.single_read flow buf in
            Buffer.add_string acc (Cstruct.to_string ~len:n buf);
            let rec lines () =
              let s = Buffer.contents acc in
              match String.index_opt s '\n' with
              | None -> ()
              | Some i ->
                  Buffer.clear acc;
                  if not bug then
                    Buffer.add_string acc
                      (String.sub s (i + 1) (String.length s - i - 1));
                  Eio.Flow.copy_string
                    (String.uppercase_ascii (String.sub s 0 (i + 1)))
                    flow;
                  if not bug then lines ()
            in
            lines ();
            loop ()
          in
          try loop () with End_of_file -> ()));
  let attempt () =
    Eio.Switch.run @@ fun sw ->
    let f = Eio.Net.connect ~sw env#net addr in
    Eio.Flow.copy_string "a\n" f;
    Eio.Time.sleep env#clock 0.001;
    Eio.Flow.copy_string "b\n" f;
    let r = R.of_flow ~max_size:256 f in
    Eio.Time.with_timeout_exn env#clock 5. (fun () ->
        let x = strict_line r in
        let y = strict_line r in
        (x, y))
  in
  let rec go n =
    match attempt () with
    | v -> Some v
    | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _) ->
        if n < 3 then go (n + 1) else None
  in
  match (go 1, Eio_net_chaos.outage env) with
  | Some ("A", "B"), false | None, true -> ()
  | Some (x, y), _ -> Fmt.failwith "got %S %S" x y
  | None, false -> failwith "gave up"

(* Added: the one-sided probe with an idle timeout in the handler. *)
let one_sided_timeout () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  let done_, set_done = Eio.Promise.create () in
  let server () =
    Eio.Switch.run @@ fun hsw ->
    Eio.Fiber.first
      (fun () ->
        while true do
          let flow, _ = Eio.Net.accept ~sw:hsw l in
          Eio.Fiber.fork ~sw:hsw (fun () ->
              let r = R.of_flow ~max_size:256 flow in
              try
                while true do
                  let _ =
                    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
                        strict_line r)
                  in
                  Eio.Flow.copy_string "ok\n" flow
                done
              with End_of_file | Eio.Io _ | Failure _ | Eio.Time.Timeout ->
                Eio.Resource.close flow)
        done)
      (fun () -> Eio.Promise.await done_)
  in
  let client () =
    let conn = ref None in
    let get () =
      let rec go n =
        match
          Eio.Time.with_timeout_exn env#clock 5. (fun () ->
              let f, r =
                match !conn with
                | Some c -> c
                | None ->
                    let f = Eio.Net.connect ~sw env#net addr in
                    let c = (f, R.of_flow ~max_size:256 f) in
                    conn := Some c;
                    c
              in
              Eio.Flow.copy_string "q\n" f;
              strict_line r)
        with
        | v -> Ok v
        | exception (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _) ->
            conn := None;
            if n < 3 then go (n + 1) else Error ()
      in
      go 1
    in
    for _ = 1 to 3 do
      ignore (get ())
    done;
    (match !conn with Some (f, _) -> Eio.Resource.close f | None -> ());
    Eio.Promise.resolve set_done ()
  in
  Eio.Fiber.both server client
