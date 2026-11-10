(* Review 2: a keep-alive client that reconnects, and a broker that fans
   messages out to subscribers. *)

open Kv

let keys = [ "a"; "bb"; "ccc"; "dddd" ]

let test_kv ?bug ?lenient () =
  Eio_net_chaos.check @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let l = Eio.Net.listen ~sw ~backlog:5 env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (serve ?lenient));
  let c = v ~sw ~clock:env#clock env#net addr in
  List.iter
    (fun k ->
      match (get ?bug c k, Eio_net_chaos.outage env) with
      | Ok v, false when v = value k -> ()
      | Error _, true -> ()
      | Ok v, _ -> Fmt.failwith "GET %s returned %S" k v
      | Error _, false -> Fmt.failwith "GET %s gave up" k)
    keys

open Pubsub

let scenario ?bug env =
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock and net = env#net in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7000) in
  let l = Eio.Net.listen ~sw ~backlog:8 net addr in
  let b = v () in
  Eio.Fiber.fork_daemon ~sw (fun () -> serve ?bug ~sw ~clock b l);
  let fast =
    List.init 3 (fun _ ->
        Eio.Fiber.fork_promise ~sw (fun () -> subscriber ~sw ~clock net addr))
  in
  let slow =
    Eio.Fiber.fork_promise ~sw (fun () ->
        subscriber ~sw ~clock ~slow:true net addr)
  in
  (* Publish once all four have subscribed, or after 5 seconds. *)
  (try
     Eio.Time.with_timeout_exn clock 5. (fun () ->
         while List.length b.subs < 4 do
           Eio.Condition.await_no_mutex b.registered
         done)
   with Eio.Time.Timeout -> ());
  let t0 = Eio.Time.now clock in
  List.iter (publish ?bug b "news") msgs;
  finish ?bug b;
  let fast = List.map Eio.Promise.await_exn fast in
  ignore (Eio.Promise.await_exn slow);
  List.iter
    (fun (got, _) ->
      if not (is_prefix got msgs) then
        Fmt.failwith "got %a" Fmt.Dump.(list string) got)
    fast;
  (* At most two faults hit a run, so one of three fast subscribers is untouched. *)
  if not (Eio_net_chaos.outage env) then
    match List.filter (fun (got, _) -> got = msgs) fast with
    | [] -> failwith "no fast subscriber got every message"
    | full ->
        List.iter
          (fun (_, last) ->
            if last -. t0 > 1. then
              Fmt.failwith "a fast subscriber waited %gs" (last -. t0))
          full

let test_pubsub ?bug () = Eio_net_chaos.check (scenario ?bug)

(* The same scenario on a network whose connections queue 8 bytes, so a
   subscriber that does not read blocks its writer at once. *)
let pinned ?bug () =
  Eio_mock.Backend.run_full @@ fun e ->
  Eio.Switch.run @@ fun sw ->
  let net =
    Eio_net_chaos.wrap ~clock:e#clock
      (fun _ -> None)
      (Eio_net_mem.v ~capacity:8 ~sw ())
  in
  let env =
    object
      method net = net
      method clock = (e#clock :> float Eio.Time.clock_ty Eio.Resource.t)
      method mono_clock = (e#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t)
    end
  in
  scenario ?bug env
