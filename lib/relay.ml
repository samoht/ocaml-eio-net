let log_src = Logs.Src.create "eio-net.relay"

module Log = (val Logs.src_log log_src)

(* Each direction runs to end-of-stream, then half-closes its destination so
   the peer's read loop finishes too. Copy failures surface once both fibers
   are done; a shutdown on an already-closed flow is the normal race with the
   peer and stays quiet. *)
let one_way src dst =
  let () = try Eio.Flow.copy src dst with End_of_file -> () in
  try Eio.Flow.shutdown dst `Send with Eio.Io _ -> ()

let splice a b = Eio.Fiber.both (fun () -> one_way a b) (fun () -> one_way b a)

let default_on_error exn =
  Log.warn (fun m -> m "relay connection failed: %a" Fmt.exn exn)

let forward ~sw ~net ?(on_error = default_on_error) ~listen ~connect () =
  let socket = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net listen in
  (* A daemon fiber: the listener must not hold the switch open once the
     caller's own work is done, or every user of the relay would hang at the
     end of its switch. *)
  let rec serve () : [ `Stop_daemon ] =
    Eio.Net.accept_fork ~sw socket ~on_error (fun client _addr ->
        Eio.Switch.run @@ fun conn_sw ->
        let upstream = Eio.Net.connect ~sw:conn_sw net connect in
        splice client upstream);
    serve ()
  in
  Eio.Fiber.fork_daemon ~sw serve;
  Eio.Net.listening_addr socket
