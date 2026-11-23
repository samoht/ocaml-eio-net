(* A seed draws clients that each stream bytes through an echo server and
   check every byte that comes back, while a fault cancels some of the reads
   and writes of both ends. An echo end may run in a domain of its own, on a
   connection accepted in the main one. *)

module Chaos = Eio_net_chaos

let byte ~conn i = Char.chr (((conn * 31) + (i * 7)) land 0xff)

(* The fault of each read and write, from the seed and the operation alone,
   so the draws do not depend on the order in which domains reach them. *)
let plan seed (op : Chaos.Op.t) =
  match op.kind with
  | `Read | `Write ->
      let side = match op.side with `Client -> 0 | `Server -> 1 in
      let kind = match op.kind with `Read -> 0 | _ -> 1 in
      let rng =
        Random.State.make [| seed; op.socket; side; kind; op.offset |]
      in
      if Random.State.int rng 3 = 0 then
        Some (Chaos.Fault.Cancel (Random.State.int rng 5))
      else None
  | `Connect | `Accept | `Recv -> None

let rec echo flow buf =
  match Eio.Flow.single_read flow buf with
  | n ->
      Eio.Flow.write flow [ Cstruct.sub buf 0 n ];
      echo flow buf
  | exception End_of_file -> Eio.Flow.shutdown flow `Send

(* A client of [len] bytes, written in pieces of at most [chunk], and read
   back in pieces of at most [rchunk]. *)
let client ~sw net addr ~conn ~len ~chunk ~rchunk =
  let flow = Eio.Net.connect ~sw net addr in
  let data = String.init len (byte ~conn) in
  let writer () =
    let rec go off =
      if off < len then begin
        let n = min chunk (len - off) in
        Eio.Flow.write flow [ Cstruct.of_string ~off ~len:n data ];
        go (off + n)
      end
    in
    go 0;
    Eio.Flow.shutdown flow `Send
  in
  let reader () =
    let buf = Cstruct.create rchunk in
    let rec go got =
      match Eio.Flow.single_read flow buf with
      | n ->
          for i = 0 to n - 1 do
            if got + i >= len then
              Fmt.failwith "connection %d: byte %d past the %d sent" conn
                (got + i) len;
            if Cstruct.get_char buf i <> data.[got + i] then
              Fmt.failwith "connection %d: byte %d is %C, not %C" conn (got + i)
                (Cstruct.get_char buf i)
                data.[got + i]
          done;
          go (got + n)
      | exception End_of_file ->
          if got <> len then
            Fmt.failwith "connection %d: %d of %d bytes came back" conn got len
    in
    go 0
  in
  Eio.Fiber.both writer reader;
  Eio.Resource.close flow

let program ~seed ~domain_mgr ~sw net =
  let rng = Random.State.make [| seed |] in
  let clients = 1 + Random.State.int rng 4 in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let l = Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net addr in
  let addr = Eio.Net.listening_addr l in
  let away = Array.init clients (fun _ -> Random.State.int rng 3 = 0) in
  let sizes =
    Array.init clients (fun _ ->
        ( 1 + Random.State.int rng 65536,
          1 + Random.State.int rng 8192,
          1 + Random.State.int rng 8192,
          1 + Random.State.int rng 8192 ))
  in
  Eio.Fiber.both
    (fun () ->
      Eio.Fiber.List.iter
        (fun i ->
          let flow, _ = Eio.Net.accept ~sw l in
          let _, _, _, echo_buf = sizes.(i) in
          let run () = echo flow (Cstruct.create echo_buf) in
          if away.(i) then Eio.Domain_manager.run domain_mgr run else run ())
        (List.init clients Fun.id))
    (fun () ->
      Eio.Fiber.List.iter
        (fun conn ->
          let len, chunk, rchunk, _ = sizes.(conn) in
          client ~sw net addr ~conn ~len ~chunk ~rchunk)
        (List.init clients Fun.id))

let networks env ~sw =
  [
    ("host", (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Resource.t));
    ("eio-net.mem", Eio_net_mem.v ~clock:env#mono_clock ~sw ());
  ]

let seeds () =
  match Sys.getenv_opt "CANCEL_SEED" with
  | Some s -> [ int_of_string s ]
  | None ->
      let n =
        Option.fold ~none:16 ~some:int_of_string (Sys.getenv_opt "CANCEL_SEEDS")
      in
      List.init n Fun.id

let timeout () =
  Option.fold ~none:60. ~some:float_of_string (Sys.getenv_opt "CANCEL_TIMEOUT")

let run_seed env ~name ~seed =
  Eio.Switch.run @@ fun sw ->
  let base = List.assoc name (networks env ~sw) in
  let net = Chaos.wrap ~clock:env#clock (plan seed) base in
  match
    Eio.Time.with_timeout_exn env#clock (timeout ()) (fun () ->
        Eio.Switch.run @@ fun sw ->
        program ~seed ~domain_mgr:env#domain_mgr ~sw net)
  with
  | () -> None
  | exception Eio.Time.Timeout ->
      Some (seed, Fmt.str "did not end within %g s" (timeout ()))
  | exception
      (( Failure _ | Invalid_argument _ | End_of_file | Eio.Io _
       | Eio.Exn.Multiple _ | Eio.Cancel.Cancelled _ ) as e) ->
      Some (seed, Printexc.to_string e)

let check name () =
  if name = "host" && not (Loopback.ipv4 ()) then Alcotest.skip ();
  Eio_main.run @@ fun env ->
  let failed =
    List.filter_map (fun seed -> run_seed env ~name ~seed) (seeds ())
  in
  if failed <> [] then
    Alcotest.failf "@[<v>%a@]"
      Fmt.(
        list ~sep:cut (fun ppf (seed, e) ->
            pf ppf "seed %d: %s (replay: CANCEL_SEED=%d)" seed e seed))
      failed

let suite =
  ( "cancel",
    [
      Alcotest.test_case "the host's network" `Quick (check "host");
      Alcotest.test_case "eio-net.mem" `Quick (check "eio-net.mem");
    ] )
