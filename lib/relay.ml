open! Hermetic

let one_way src dst =
  let () = try Eio.Flow.copy src dst with End_of_file -> () in
  try Eio.Flow.shutdown dst `Send with Eio.Io _ -> ()

let splice a b = Eio.Fiber.both (fun () -> one_way a b) (fun () -> one_way b a)

let forward ~sw ~net ?on_error ?pool ?classify ~max_connections ~listen ~connect
    () =
  (* Checked here as well as by the loop, which runs in a daemon fiber where
     the error would fail [sw] rather than reach the caller. *)
  if max_connections < 1 then
    Fmt.invalid_arg "Relay.forward: max_connections is %d" max_connections;
  (* On a Unix path, Eio's POSIX backend takes [reuse_addr] as leave to
     remove the file there, a live listener's included. *)
  let reuse_addr = match listen with `Tcp _ -> true | `Unix _ -> false in
  let socket = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr net listen in
  (* A daemon fiber: the listener must not hold the switch open once the
     caller's own work ends. A relay has no head: it serves from the accept. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio_net_accept.run ?on_error ?pool ?classify ~max_connections socket
        (fun head client _addr ->
          Eio_net_accept.complete head;
          Eio.Switch.run @@ fun conn_sw ->
          let upstream = Eio.Net.connect ~sw:conn_sw net connect in
          splice client upstream));
  Eio.Net.listening_addr socket
