(* A client that counts on TCP to notice a server gone silent. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7700)
let requests = 3
let tries = 3
let net_error = function Eio.Io _ | End_of_file -> true | _ -> false

(* The server answers one line per connection with the same line, then
   closes it. *)
let serve ~sw l =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run @@ fun sw ->
      while true do
        match Eio.Net.accept ~sw l with
        | flow, _ ->
            Eio.Fiber.fork ~sw (fun () ->
                (try
                   let r = Eio.Buf_read.of_flow flow ~max_size:64 in
                   Eio.Flow.copy_string (Eio.Buf_read.line r ^ "\n") flow
                 with e when net_error e -> ());
                Eio.Flow.close flow)
        | exception Eio.Io _ -> ()
      done;
      `Stop_daemon)

let set flow opt v = Eio.Net.setsockopt flow opt v

let detect flow = function
  | `Nothing -> ()
  | `Keepalive ->
      set flow Eio.Net.Sockopt.SO_KEEPALIVE true;
      set flow Eio.Net.Sockopt.TCP_KEEPIDLE 1;
      set flow Eio.Net.Sockopt.TCP_KEEPINTVL 1;
      set flow Eio.Net.Sockopt.TCP_KEEPCNT 3
  | `User_timeout ->
      set flow Eio.Net.Sockopt.SO_KEEPALIVE true;
      set flow Eio.Net.Sockopt.TCP_KEEPIDLE 60;
      set flow Eio.Net.Sockopt.TCP_KEEPINTVL 10;
      set flow Eio.Net.Sockopt.TCP_USER_TIMEOUT 10_000

(* Each request on a connection of its own, tried [tries] times. *)
let client ~using (env : Eio_net_chaos.env) =
  let ask i =
    Eio.Switch.run @@ fun sw ->
    let flow = Eio.Net.connect ~sw env#net addr in
    detect flow using;
    Eio.Flow.copy_string (Fmt.str "%d\n" i) flow;
    Eio.Buf_read.(line (of_flow flow ~max_size:64))
  in
  let rec attempt i n =
    match ask i with
    | line ->
        if line <> string_of_int i then Fmt.failwith "request %d: %S" i line;
        true
    | exception e when net_error e -> n > 1 && attempt i (n - 1)
  in
  List.init requests (fun i -> attempt i tries)

let run ~using ~within (env : Eio_net_chaos.env) =
  let start = Eio.Time.now env#clock in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:4 env#net addr in
  serve ~sw l;
  let answered = client ~using env in
  let took = Eio.Time.now env#clock -. start in
  if took > within then Fmt.failwith "the client took %gs" took;
  match (List.for_all Fun.id answered, Eio_net_chaos.outage env) with
  | true, false | false, true -> ()
  | true, true -> failwith "a request was answered in the outage"
  | false, false -> failwith "a request failed three times"

let keepalive env = run ~using:`Keepalive ~within:3600. env
let user_timeout env = run ~using:`User_timeout ~within:300. env
let nothing env = run ~using:`Nothing ~within:3600. env
