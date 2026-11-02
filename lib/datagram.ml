open! Hermetic

type Eio.Exn.Backend.t += Too_long of { max : int }

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Too_long { max } ->
        Fmt.pf ppf "Too_long (a datagram longer than %d bytes, eio-net)" max;
        true
    | _ -> false)

type t = {
  recv : unit -> Eio.Net.Sockaddr.datagram * string;
  send : Eio.Net.Sockaddr.datagram option -> Cstruct.t list -> unit;
}

let pp ppf _ = Fmt.string ppf "<datagram channel>"

let of_socket ?(max_frame = 65536) ?dst socket =
  if max_frame <= 0 then
    Fmt.invalid_arg "Eio_net.Datagram: max_frame %d must be positive" max_frame;
  (* One byte of headroom tells a datagram longer than [max_frame] from one
     exactly [max_frame] long. *)
  let buf = Cstruct.create (max_frame + 1) in
  (* Receives share [buf], and a completion-based backend fills the buffer of
     every receive it has submitted, so one runs at a time. *)
  let lock = Eio.Mutex.create () in
  let recv () =
    Eio.Mutex.use_ro lock @@ fun () ->
    let source, len = Eio.Net.recv socket buf in
    if len > max_frame then
      raise (Eio.Exn.create (Eio.Exn.X (Too_long { max = max_frame })));
    (source, Cstruct.to_string (Cstruct.sub buf 0 len))
  in
  let send destination bufs =
    match (destination, dst) with
    | Some dst, _ | None, Some dst -> Eio.Net.send socket ~dst bufs
    | None, None -> Eio.Net.send socket bufs
  in
  { recv; send }

let recv t = t.recv ()
let send t ?dst bufs = t.send dst bufs

let equal_addr (a : Eio.Net.Sockaddr.datagram) (b : Eio.Net.Sockaddr.datagram) =
  match (a, b) with
  | `Udp (host_a, port_a), `Udp (host_b, port_b) ->
      port_a = port_b && String.equal (host_a :> string) (host_b :> string)
  | `Unix path_a, `Unix path_b -> String.equal path_a path_b
  | `Udp _, `Unix _ | `Unix _, `Udp _ -> false
