(* Races across domains whose outcomes Linux's loopback sockets bound: over
   many seeds, every outcome eio-net.mem gives must be one the kernel gives.
   The kernel's outcome sets were measured through eio_posix on Linux over
   2000 seeds of the same workloads; none of these depends on timing. *)

let spin st =
  for _ = 1 to Random.State.int st 2000 do
    Domain.cpu_relax ()
  done

let lo = Eio.Net.Ipaddr.V4.loopback

let classify = function
  | End_of_file -> "EOF"
  | Invalid_argument _ -> "INVAL"
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> "RESET"
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> "REFUSED"
  | e -> Printexc.to_string e

let pair ~sw net =
  let l = Eio.Net.listen ~sw ~backlog:128 net (`Tcp (lo, 0)) in
  let a = Eio.Net.connect ~sw net (Eio.Net.listening_addr l) in
  let b, _ = Eio.Net.accept ~sw l in
  (l, a, b)

(* A reader waits on one end while another domain closes or shuts down
   either end: it reads the bytes written before, then end-of-file. *)
let read_race pool how seed =
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let _, a, b = pair ~sw net in
  let st = Random.State.make [| seed |] in
  let pre = Random.State.int st 3 in
  if pre > 0 then Eio.Flow.write a [ Cstruct.create pre ];
  let reader () =
    let buf = Cstruct.create 16 in
    let rec loop acc =
      match Eio.Flow.single_read b buf with
      | n -> loop (("r" ^ string_of_int n) :: acc)
      | exception e -> List.rev (classify e :: acc)
    in
    loop []
  in
  let run f = Eio.Executor_pool.submit_exn pool ~weight:1.0 f in
  let got, () =
    Eio.Fiber.pair
      (fun () -> run reader)
      (fun () ->
        run (fun () ->
            spin st;
            match how with
            | `Peer_close -> Eio.Flow.close a
            | `Own_shut_recv -> Eio.Flow.shutdown b `Receive
            | `Own_shut_all -> Eio.Flow.shutdown b `All
            | `Peer_shut_send -> Eio.Flow.shutdown a `Send))
  in
  let expected =
    (if pre > 0 then [ "r" ^ string_of_int pre ] else []) @ [ "EOF" ]
  in
  if got <> expected then
    Alcotest.failf "seed %d, pre %d: the reader got %s" seed pre
      (String.concat "," got)

(* A write loop on one domain while another closes the same end: each write
   in flight finishes, later ones raise [Invalid_argument], and the peer
   reads end-of-file. *)
let close_vs_write pool seed =
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let _, a, b = pair ~sw net in
  let st = Random.State.make [| seed |] in
  let run f = Eio.Executor_pool.submit_exn pool ~weight:1.0 f in
  let writer () =
    let rec loop k =
      if k = 0 then "all-ok"
      else
        match Eio.Flow.single_write a [ Cstruct.create 8 ] with
        | _ -> loop (k - 1)
        | exception e -> classify e
    in
    loop 2000
  in
  let w, () =
    Eio.Fiber.pair
      (fun () -> run writer)
      (fun () ->
        run (fun () ->
            spin st;
            Eio.Flow.close a))
  in
  let buf = Cstruct.create 65536 in
  let rec drain () =
    match Eio.Flow.single_read b buf with
    | _ -> drain ()
    | exception e -> classify e
  in
  let peer = drain () in
  if not (List.mem w [ "all-ok"; "INVAL" ] && peer = "EOF") then
    Alcotest.failf "seed %d: writer=%s peer=%s" seed w peer

(* Connects on one domain race an acceptor and the listener's close on
   others: a connect is refused, or reads what its acceptor wrote, or the
   reset of a connection the close dropped; and every accepted connection
   was one whose connect succeeded. *)
let accept_race pool seed =
  Eio.Switch.run @@ fun sw ->
  let net = Eio_net_mem.v ~sw () in
  let l = Eio.Net.listen ~sw ~backlog:128 net (`Tcp (lo, 0)) in
  let addr = Eio.Net.listening_addr l in
  let st = Random.State.make [| seed |] in
  let st2 = Random.State.copy st in
  let written = Atomic.make 0 in
  let run f = Eio.Executor_pool.submit_exn pool ~weight:1.0 f in
  let acceptor () =
    Eio.Switch.run @@ fun asw ->
    let rec loop () =
      match Eio.Net.accept ~sw:asw l with
      | flow, _ ->
          (try
             Eio.Flow.write flow [ Cstruct.of_string "x" ];
             Atomic.incr written
           with Eio.Io _ -> ());
          Eio.Flow.close flow;
          loop ()
      | exception Invalid_argument _ -> ()
    in
    loop ()
  in
  let client () =
    Eio.Switch.run @@ fun csw ->
    List.init 3 (fun _ ->
        spin st2;
        match Eio.Net.connect ~sw:csw net addr with
        | c ->
            let buf = Cstruct.create 4 in
            let rec read acc =
              match Eio.Flow.single_read c buf with
              | n -> read (acc ^ Cstruct.to_string ~len:n buf)
              | exception e -> "ok:" ^ acc ^ classify e
            in
            read ""
        | exception e -> classify e)
  in
  let clients =
    Eio.Fiber.first
      (fun () ->
        acceptor ();
        Eio.Fiber.await_cancel ())
      (fun () ->
        let clients, () =
          Eio.Fiber.pair
            (fun () -> run client)
            (fun () ->
              run (fun () ->
                  for _ = 1 to Random.State.int st 6 do
                    spin st
                  done;
                  Eio.Resource.close l))
        in
        clients)
  in
  List.iter
    (fun c ->
      if not (List.mem c [ "REFUSED"; "ok:xEOF"; "ok:RESET" ]) then
        Alcotest.failf "seed %d: a client got %s" seed c)
    clients;
  let read_x = List.length (List.filter (( = ) "ok:xEOF") clients) in
  if Atomic.get written <> read_x then
    Alcotest.failf "seed %d: %d accepted connections wrote, %d clients read"
      seed (Atomic.get written) read_x

let seeds = 200

let with_pool f () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pool =
    Eio.Executor_pool.create ~sw ~domain_count:2 (Eio.Stdenv.domain_mgr env)
  in
  for seed = 1 to seeds do
    f pool seed
  done

let suite =
  ( "concurrency",
    List.map
      (fun (name, how) ->
        Alcotest.test_case ("a read racing " ^ name) `Quick
          (with_pool (fun pool -> read_race pool how)))
      [
        ("the peer's close", `Peer_close);
        ("its own receive shutdown", `Own_shut_recv);
        ("its own shutdown", `Own_shut_all);
        ("the peer's send shutdown", `Peer_shut_send);
      ]
    @ [
        Alcotest.test_case "a write racing its own close" `Quick
          (with_pool close_vs_write);
        Alcotest.test_case "connects racing an accept and a close" `Quick
          (with_pool accept_race);
      ] )
