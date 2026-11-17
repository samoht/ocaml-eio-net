open! Hermetic

type Eio.Exn.Backend.t += Injected_fault = Wrapper.Injected_fault
type env = Eio_chaos.env

module Op = Op
module Fault = Fault

let wrap ?os ~clock f net =
  Wrapper.v ?os ~clock ~outage:false
    (fun op -> Option.map (fun f -> (f, 0)) (f op))
    net

let pair = Wrapper.pair
let is_outage env = Wrapper.is_outage env#net
let network = Network.device

(* Reads and writes draw from states of their own, so the faults of each
   depend on [seed] alone, not on how the two interleave. *)
let seeded ?dribble ?stall ?short_write ?reset_after ?(reset_kind = `Reset)
    ?fail_write_after seed =
  let reads = Random.State.make [| seed; 1 |]
  and writes = Random.State.make [| seed; 2 |] in
  let capped st = function
    | Some n when n > 0 -> Some (Fault.Short (1 + Random.State.int st n))
    | _ -> None
  in
  let reset =
    match reset_kind with `Reset -> Fault.Reset | `Eof -> Fault.Eof
  in
  fun (op : Op.t) ->
    match op.kind with
    | `Read -> (
        match reset_after with
        | Some n when op.offset >= n -> Some reset
        | _ -> (
            match stall with
            | Some n when n > 0 && Random.State.int reads (n + 1) = 0 ->
                Some (Fault.Stall 0.)
            | _ -> capped reads dribble))
    | `Write -> (
        match fail_write_after with
        | Some n when op.offset >= n -> Some Fault.Reset
        | _ -> capped writes short_write)
    | `Connect | `Accept | `Recv -> None

let by_side ~client ~server (op : Op.t) =
  match op.side with `Client -> client op | `Server -> server op

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Eio_chaos.run ~name:"Eio_net_chaos.run" ?os ?budget ?seed ?replay ?setup
    ?inspect
    [ network () ]
    app

let conformance ?os ?under ?budget ?seed ?replay net =
  Conformance.check ?os ?under ?budget ?seed ?replay net

module Private = struct
  let conformance_allowances = Conformance.allowances
  let conformance_defects = Conformance.defect_findings
  let conformance_allowed os net = (Conformance.replay os net).allowed
  let conformance_in_loop = Conformance.check_in_loop
end

module Listener = struct
  type net = [ `Generic ] Eio.Net.ty Eio.Resource.t

  let accepted ~offered serve =
    let net = Eio_mock.Net.make "net" in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) in
    let listening =
      Eio_mock.Net.listening_socket ~listening_addr:addr "listening"
    in
    Eio_mock.Net.on_listen net [ `Return listening ];
    let silent, _ = Eio.Promise.create () in
    let count = ref 0 in
    let connection i =
      `Run
        (fun () ->
          incr count;
          let flow = Fmt.kstr Eio_mock.Flow.make "peer%d" i in
          Eio_mock.Flow.on_read flow [ `Await silent ];
          (flow, `Tcp (Eio.Net.Ipaddr.V4.loopback, 1024 + i)))
    in
    let no_more, _ = Eio.Promise.create () in
    Eio_mock.Net.on_accept listening
      (List.init offered connection @ [ `Await no_more ]);
    match Eio_mock.Backend.run (fun () -> serve ~net:(net :> net) ~addr) with
    | () -> Fmt.failwith "Listener.accepted: the listener returned"
    | exception Eio_mock.Backend.Deadlock_detected -> !count

  (* A network over [net] whose listener at [addr] fails its accepts with each
     scripted exception in turn, once its cue resolves, and otherwise accepts
     from [net]'s. A cue resolved already fails the accept before it races the
     inner one, which could take a peer the failure would then drop. The
     network keeps [net]'s tag, so a listener that needs a Unix socket gets
     one. *)
  module Failing (T : sig
    type tag = private [> `Generic ]
  end) =
  struct
    module Socket = struct
      type tag = T.tag

      type t = {
        inner : tag Eio.Net.listening_socket_ty Eio.Resource.t;
        failures : (unit Eio.Promise.t * exn) list ref;
      }

      let fail t rest exn =
        t.failures := rest;
        raise exn

      let accept t ~sw =
        match !(t.failures) with
        | [] -> Eio.Net.accept ~sw t.inner
        | (cue, exn) :: rest when Eio.Promise.is_resolved cue -> fail t rest exn
        | (cue, exn) :: rest ->
            Eio.Fiber.first
              (fun () ->
                Eio.Promise.await cue;
                fail t rest exn)
              (fun () -> Eio.Net.accept ~sw t.inner)

      let close t = Eio.Resource.close t.inner
      let listening_addr t = Eio.Net.listening_addr t.inner
      let setsockopt t opt v = Eio.Net.setsockopt t.inner opt v
      let getsockopt t opt = Eio.Net.getsockopt t.inner opt
    end

    let socket = Eio.Net.Pi.listening_socket (module Socket)

    module Net = struct
      type tag = T.tag

      type t = {
        net : tag Eio.Net.ty Eio.Resource.t;
        addr : Eio.Net.Sockaddr.stream;
        failures : (unit Eio.Promise.t * exn) list ref;
      }

      let listen t ~reuse_addr ~reuse_port ~backlog ~sw addr =
        let inner =
          Eio.Net.listen ~reuse_addr ~reuse_port ~backlog ~sw t.net addr
        in
        if Eio_net_addr.equal_sockaddr addr t.addr then
          Eio.Resource.T ({ Socket.inner; failures = t.failures }, socket)
        else inner

      let connect t ~bind_to ~options ~sw addr =
        Eio.Net.connect ?bind_to ~options ~sw t.net addr

      let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
        Eio.Net.datagram_socket ~reuse_addr ~reuse_port ~sw t.net addr

      let getaddrinfo t ~service node = Eio.Net.getaddrinfo ~service t.net node
      let getnameinfo t addr = Eio.Net.getnameinfo t.net addr
    end

    let net = Eio.Net.Pi.network (module Net)
  end

  module Generic = Failing (struct
    type tag = [ `Generic ]
  end)

  module Unix_tagged = Failing (struct
    type tag = [ `Generic | `Unix ]
  end)

  let failing ~addr failures (net : [> [ `Generic ] Eio.Net.ty ] Eio.Resource.t)
      : net =
    Eio.Resource.T
      ({ Generic.Net.net :> net; addr; failures = ref failures }, Generic.net)

  let failing_unix ~addr failures
      (net : [> [ `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t) :
      [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t =
    Eio.Resource.T
      ( {
          Unix_tagged.Net.net :> [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
          addr;
          failures = ref failures;
        },
        Unix_tagged.net )

  let stall ~sw net addr data ~on_end =
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Switch.run @@ fun csw ->
        let flow = Eio.Net.connect ~sw:csw net addr in
        let buf = Cstruct.create 4096 in
        let rec drain () =
          ignore (Eio.Flow.single_read flow buf : int);
          drain ()
        in
        match
          Eio.Flow.copy_string data flow;
          drain ()
        with
        | () -> ()
        | exception (End_of_file | Eio.Io _) -> on_end ())
end
