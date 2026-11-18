open! Hermetic

type Eio.Exn.Backend.t += Too_long of { max_length : int }

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Too_long { max_length } ->
        Fmt.pf ppf "Too_long (a datagram longer than %d bytes, eio-net)"
          max_length;
        true
    | _ -> false)

type t = {
  max_length : int;
  dst : Eio.Net.Sockaddr.datagram option;
  recv : unit -> Eio.Net.Sockaddr.datagram * string;
  send : Eio.Net.Sockaddr.datagram option -> Cstruct.t list -> unit;
}

let too_long ~max_length = Eio.Exn.create (Eio.Exn.X (Too_long { max_length }))

let pp ppf t =
  Fmt.pf ppf "<datagram channel, max_length %d%a>" t.max_length
    (Fmt.option (fun ppf -> Fmt.pf ppf ", dst %a" Eio.Net.Sockaddr.pp))
    t.dst

let of_socket ?(max_length = 65536) ?dst socket =
  if max_length <= 0 then
    Fmt.invalid_arg
      "Eio_net.Datagram.of_socket: max_length is %d, must be positive"
      max_length;
  (* One byte of headroom tells a datagram longer than [max_length] from one
     exactly [max_length] long. *)
  let buf = Cstruct.create (max_length + 1) in
  (* Receives share [buf], and a completion-based backend fills the buffer of
     every receive it has submitted, so one runs at a time. *)
  let lock = Eio.Mutex.create () in
  let recv () =
    Eio.Mutex.use_ro lock @@ fun () ->
    let source, len = Eio.Net.recv socket buf in
    if len > max_length then raise (too_long ~max_length);
    (source, Cstruct.to_string (Cstruct.sub buf 0 len))
  in
  let send destination bufs =
    match (destination, dst) with
    | Some dst, _ | None, Some dst -> Eio.Net.send socket ~dst bufs
    | None, None -> Eio.Net.send socket bufs
  in
  { max_length; dst; recv; send }

let recv t = t.recv ()
let send ?dst t bufs = t.send dst bufs
