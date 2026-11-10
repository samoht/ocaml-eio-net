(* A broker fanning messages out to subscribers. A subscriber sends
   "SUB topic\n", then reads lines. One slow subscriber must not delay the
   others: each has a bounded queue and its own writer, and one that falls
   8 messages behind is dropped. *)
module R = Eio.Buf_read

let strict_line = App.strict_line

type sub = {
  topic : string;
  flow : [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t;
  q : string Eio.Stream.t;
  mutable dead : bool;
}

type t = { mutable subs : sub list; registered : Eio.Condition.t }

let v () = { subs = []; registered = Eio.Condition.create () }

let kill s =
  s.dead <- true;
  try Eio.Resource.close s.flow with Eio.Io _ | Invalid_argument _ -> ()

let handshake ?(bug = `None) flow =
  if bug = `Single_read then
    let buf = Cstruct.create 64 in
    let n = Eio.Flow.single_read flow buf in
    let line = String.trim (Cstruct.to_string ~len:n buf) in
    String.sub line 4 (String.length line - 4)
  else
    let line = strict_line (R.of_flow ~max_size:64 flow) in
    String.sub line 4 (String.length line - 4)

let serve ?(bug = `None) ~sw ~clock t l =
  let rec loop () =
    let flow, _ = Eio.Net.accept ~sw l in
    Eio.Fiber.fork ~sw (fun () ->
        match
          Eio.Time.with_timeout_exn clock 1. (fun () -> handshake ~bug flow)
        with
        | exception
            (Eio.Io _ | End_of_file | Eio.Time.Timeout | Invalid_argument _) ->
            Eio.Resource.close flow
        | topic ->
            let s =
              {
                topic;
                flow :> [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t;
                q = Eio.Stream.create max_int;
                dead = false;
              }
            in
            t.subs <- t.subs @ [ s ];
            Eio.Condition.broadcast t.registered;
            if bug = `None then
              let rec write () =
                match Eio.Stream.take s.q with
                | "" -> (
                    try Eio.Flow.shutdown s.flow `Send with Eio.Io _ -> ())
                | m -> (
                    match Eio.Flow.copy_string m s.flow with
                    | () -> write ()
                    | exception Eio.Io _ -> kill s)
              in
              write ());
    loop ()
  in
  loop ()

let publish ?(bug = `None) t topic m =
  List.iter
    (fun s ->
      if s.topic = topic && not s.dead then
        match bug with
        | `None ->
            if Eio.Stream.length s.q >= 8 then kill s else Eio.Stream.add s.q m
        | `Hol -> ( try Eio.Flow.copy_string m s.flow with Eio.Io _ -> kill s)
        | `Fanout_dies -> Eio.Flow.copy_string m s.flow
        | `Single_read -> (
            try Eio.Flow.copy_string m s.flow with Eio.Io _ -> kill s))
    t.subs

let finish ?(bug = `None) t =
  List.iter
    (fun s ->
      if not s.dead then
        if bug = `None then Eio.Stream.add s.q ""
        else try Eio.Flow.shutdown s.flow `Send with Eio.Io _ -> ())
    t.subs

(* Five messages of 16 KB: a subscriber that does not read holds two in its
   connection's buffers, and blocks a writer on the third. *)
let msgs = List.init 5 (fun i -> Fmt.str "m%d %s\n" i (String.make 16_000 'x'))

(* A subscriber: the lines it got, and when it got the last one. *)
let subscriber ~sw ~clock ?(slow = false) net addr =
  match
    let flow = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "SUB news\n" flow;
    let r = R.of_flow ~max_size:65536 flow in
    let rec loop acc last =
      if slow then Eio.Time.sleep clock 60.;
      match Eio.Time.with_timeout_exn clock 120. (fun () -> strict_line r) with
      | l -> loop ((l ^ "\n") :: acc) (Eio.Time.now clock)
      | exception (End_of_file | Eio.Io _ | Eio.Time.Timeout) ->
          (List.rev acc, last)
    in
    loop [] 0.
  with
  | v -> v
  | exception Eio.Io _ -> ([], 0.)

let rec is_prefix p l =
  match (p, l) with
  | [], _ -> true
  | x :: p, y :: l -> x = y && is_prefix p l
  | _ -> false
