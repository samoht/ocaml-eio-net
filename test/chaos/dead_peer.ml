(* A broadcaster and a peer that goes silent. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7600)
let subscribers = 3

(* Five frames of 256 KiB: more than a TCP connection queues on macOS. *)
let frame_size = 1 lsl 18
let frames = 5

let frame =
  let all =
    Array.init frames (fun i ->
        let b = Bytes.make frame_size (Char.chr (97 + i)) in
        Bytes.set_uint8 b 0 i;
        Bytes.to_string b)
  in
  Array.get all

let net_error = function
  | Eio.Io _ | End_of_file | Eio.Time.Timeout -> true
  | _ -> false

(* [`Sync] writes each frame to each subscriber in turn. Otherwise each
   subscriber has a queue of two frames and a writer. One whose queue stays
   full for 1 s, or whose writer is not done 1 s after the last frame, is
   shut down, which ends its writer's blocked write. *)
let broadcaster ~bug (env : Eio_net_chaos.env) ~sw conns =
  let send flow alive i =
    if !alive then
      try Eio.Flow.copy_string (frame i) flow
      with e when net_error e -> alive := false
  in
  match bug with
  | `Sync ->
      for i = 0 to frames - 1 do
        List.iter (fun (flow, alive) -> send flow alive i) conns
      done
  | `None ->
      let within f =
        match Eio.Time.with_timeout_exn env#clock 1. f with
        | () -> true
        | exception Eio.Time.Timeout -> false
      in
      let evict flow alive =
        alive := false;
        try Eio.Flow.shutdown flow `All with Eio.Io _ -> ()
      in
      let queues =
        List.map
          (fun (flow, alive) ->
            let q = Eio.Stream.create 2 in
            let finished, finish = Eio.Promise.create () in
            Eio.Fiber.fork ~sw (fun () ->
                let rec loop () =
                  match Eio.Stream.take q with
                  | None -> Eio.Promise.resolve finish ()
                  | Some i ->
                      send flow alive i;
                      loop ()
                in
                loop ());
            (q, finished, flow, alive))
          conns
      in
      for i = 0 to frames - 1 do
        List.iter
          (fun (q, _, flow, alive) ->
            if !alive && not (within (fun () -> Eio.Stream.add q (Some i))) then
              evict flow alive)
          queues
      done;
      List.iter
        (fun (q, finished, flow, alive) ->
          let ended = ref false in
          let drained () =
            Eio.Stream.add q None;
            ended := true;
            Eio.Promise.await finished
          in
          if not (within drained) then begin
            evict flow alive;
            if not !ended then Eio.Stream.add q None
          end)
        queues

(* A subscriber reads each frame with a timeout of 2 s, and tries a read that
   timed out three more times before it gives up and closes. *)
let subscriber (env : Eio_net_chaos.env) =
  match
    Eio.Switch.run @@ fun sw ->
    let flow =
      Eio.Time.with_timeout_exn env#clock 1. (fun () ->
          Eio.Net.connect ~sw env#net addr)
    in
    let r = Eio.Buf_read.of_flow flow ~max_size:(2 * frame_size) in
    let got = ref 0 in
    let rec next tries =
      if !got < frames then
        match
          Eio.Time.with_timeout_exn env#clock 2. (fun () ->
              Eio.Buf_read.take frame_size r)
        with
        | s ->
            if s <> frame !got then Fmt.failwith "frame %d is wrong" !got;
            incr got;
            next 0
        | exception Eio.Time.Timeout when tries < 3 -> next (tries + 1)
    in
    (try next 0 with e when net_error e -> ());
    !got
  with
  | n -> n
  | exception e when net_error e -> 0

let broadcast ?(bug = `None) (env : Eio_net_chaos.env) =
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:4 env#net addr in
  let finished, finish = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
         Eio.Switch.run @@ fun sw ->
         let conns = ref [] in
         (try
            Eio.Time.with_timeout_exn env#clock 1. (fun () ->
                while List.length !conns < subscribers do
                  try
                    let flow, _ = Eio.Net.accept ~sw l in
                    conns := (flow, ref true) :: !conns
                  with Eio.Io _ -> ()
                done)
          with Eio.Time.Timeout -> ());
         broadcaster ~bug env ~sw (List.rev !conns)
       with e when net_error e -> ());
      Eio.Promise.resolve finish ();
      `Stop_daemon);
  let got =
    Eio.Fiber.List.map
      (fun () -> subscriber env)
      (List.init subscribers (fun _ -> ()))
  in
  Eio.Promise.await finished;
  match (List.mem frames got, Eio_net_chaos.outage env) with
  | true, false | false, true -> ()
  | true, true -> failwith "a subscriber got every frame in the outage"
  | false, false ->
      Fmt.failwith "no subscriber got every frame: %a"
        Fmt.(list ~sep:sp int)
        got
