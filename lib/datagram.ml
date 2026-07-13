type t = {
  recv : unit -> Eio.Net.Sockaddr.datagram * string;
  send : Eio.Net.Sockaddr.datagram option -> Rope.t -> unit;
}

let pp ppf _ = Fmt.string ppf "<datagram channel>"

(* A rope's leaves map one-to-one onto iovec entries, so a payload carried as a
   zero-copy rope leaf reaches the wire through [writev] without being
   concatenated into a whole-datagram string. *)
let rope_cstructs rope = List.map Cstruct.of_string (Rope.to_strings rope)

let of_socket ?(max_frame = 65536) ?dst socket =
  if max_frame <= 0 then
    Fmt.invalid_arg "Eio_net.Datagram: max_frame %d must be positive" max_frame;
  (* One byte of headroom so a datagram longer than [max_frame] is
     distinguishable from one exactly [max_frame] long: a datagram socket
     reports a truncated receive as if it were complete. *)
  let buf = Cstruct.create (max_frame + 1) in
  let recv () =
    let source, len = Eio.Net.recv socket buf in
    if len > max_frame then
      Fmt.invalid_arg "Eio_net.Datagram: datagram exceeds max_frame (%d bytes)"
        max_frame;
    (source, Cstruct.to_string (Cstruct.sub buf 0 len))
  in
  let send destination rope =
    let bufs = rope_cstructs rope in
    match (destination, dst) with
    | Some dst, _ | None, Some dst -> Eio.Net.send socket ~dst bufs
    | None, None -> Eio.Net.send socket bufs
  in
  { recv; send }

let equal_addr (a : Eio.Net.Sockaddr.datagram) (b : Eio.Net.Sockaddr.datagram) =
  match (a, b) with
  | `Udp (host_a, port_a), `Udp (host_b, port_b) ->
      port_a = port_b && String.equal (host_a :> string) (host_b :> string)
  | `Unix path_a, `Unix path_b -> String.equal path_a path_b
  | `Udp _, `Unix _ | `Unix _, `Udp _ -> false
