(* A keep-alive client: "GET k\n" -> "<len>\n<body>", many requests on
   one connection; on a reset, an end of stream or a timeout it reconnects
   and retries, at most three attempts. *)
module R = Eio.Buf_read

let strict_line = App.strict_line
let value k = String.concat "" (List.init 3 (fun _ -> k))

(* [lenient] takes an unterminated last line for a whole one. *)
let serve ?(lenient = false) flow _ =
  let r = R.of_flow ~max_size:4096 flow in
  let rec loop () =
    match if lenient then R.line r else strict_line r with
    | line ->
        let k = String.sub line 4 (String.length line - 4) in
        let v = value k in
        Eio.Flow.copy_string (Fmt.str "%d\n%s" (String.length v) v) flow;
        loop ()
    | exception End_of_file -> ()
  in
  loop ()

type t = {
  sw : Eio.Switch.t;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  addr : Eio.Net.Sockaddr.stream;
  mutable conn :
    ([ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t * R.t) option;
  mutable connects : int;
}

let v ~sw ~clock net addr = { sw; net; clock; addr; conn = None; connects = 0 }

let drop t =
  (match t.conn with
  | Some (f, _) -> (
      try Eio.Resource.close f with Eio.Io _ | Invalid_argument _ -> ())
  | None -> ());
  t.conn <- None

let conn t =
  match t.conn with
  | Some c -> c
  | None ->
      let f = Eio.Net.connect ~sw:t.sw t.net t.addr in
      t.connects <- t.connects + 1;
      let c =
        ( (f :> [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t),
          R.of_flow ~max_size:4096 f )
      in
      t.conn <- Some c;
      c

let get ?(bug = `None) t k =
  let rec attempt n =
    let phase = ref `Connect in
    match
      Eio.Time.with_timeout_exn t.clock 5. (fun () ->
          let f, r = conn t in
          phase := `Write;
          Eio.Flow.copy_string ("GET " ^ k ^ "\n") f;
          phase := `Read;
          let len = int_of_string (strict_line r) in
          if bug = `Eof_body then
            try R.take len r with End_of_file -> R.take_all r
          else R.take len r)
    with
    | body -> Ok body
    | exception e ->
        let retry =
          match (bug, e, !phase) with
          | `Read_only, Eio.Io _, `Write -> false
          | `Reset_only, End_of_file, `Read -> false
          | _, (Eio.Io _ | End_of_file | Eio.Time.Timeout | Failure _), _ ->
              true
          | _ -> false
        in
        if not retry then raise e;
        if bug <> `No_reconnect then drop t;
        if n < 3 then attempt (n + 1) else Error `Gave_up
  in
  attempt 1
