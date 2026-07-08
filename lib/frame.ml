type t = { recv : unit -> string; send : string -> unit }

let pp ppf _ = Fmt.string ppf "<frame channel>"
let v ~recv ~send = { recv; send }

let of_datagram_socket ?(max_frame = 65536) ?dst socket =
  let buf = Cstruct.create max_frame in
  let recv () =
    let _src, n = Eio.Net.recv socket buf in
    Cstruct.to_string (Cstruct.sub buf 0 n)
  in
  let send frame =
    let bufs = [ Cstruct.of_string frame ] in
    match dst with
    | Some dst -> Eio.Net.send socket ~dst bufs
    | None -> Eio.Net.send socket bufs
  in
  { recv; send }

let pair () =
  let a_to_b = Eio.Stream.create max_int in
  let b_to_a = Eio.Stream.create max_int in
  let a =
    { recv = (fun () -> Eio.Stream.take b_to_a); send = Eio.Stream.add a_to_b }
  in
  let b =
    { recv = (fun () -> Eio.Stream.take a_to_b); send = Eio.Stream.add b_to_a }
  in
  (a, b)
