(* Programs under test. Each takes a [bug] so the tests can show they catch
   the mistake. *)

(* A line ended by a newline: [Eio.Buf_read.line] also returns an
   unterminated last line. *)
let strict_line r =
  let l = Eio.Buf_read.take_while (fun c -> c <> '\n') r in
  Eio.Buf_read.char '\n' r;
  l

(* (d) A length-prefixed protocol: [len:4][payload]. *)
module Frame = struct
  let write flow msg =
    let b = Bytes.create (4 + String.length msg) in
    Bytes.set_int32_be b 0 (Int32.of_int (String.length msg));
    Bytes.blit_string msg 0 b 4 (String.length msg);
    Eio.Flow.copy_string (Bytes.to_string b) flow

  let exact ~bug flow n =
    let buf = Cstruct.create n in
    if bug = `Single_read && n > 0 then ignore (Eio.Flow.single_read flow buf)
    else Eio.Flow.read_exact flow buf;
    Cstruct.to_string buf

  exception Truncated

  (* [None] at a clean end of stream; [Truncated] at an end mid-message;
     [Eio.Time.Timeout] when the peer stalls for [timeout] seconds. *)
  let read ?(bug = `None) ~clock flow =
    let go () =
      let first = Cstruct.create 1 in
      match Eio.Flow.single_read flow first with
      | exception End_of_file -> None
      | _ -> (
          let hdr =
            try Cstruct.to_string first ^ exact ~bug flow 3
            with End_of_file when bug <> `Eof_is_clean -> raise Truncated
          in
          let n = Int32.to_int (String.get_int32_be hdr 0) in
          match exact ~bug flow n with
          | exception End_of_file when bug <> `Eof_is_clean -> raise Truncated
          | exception End_of_file -> None
          | s -> Some s)
    in
    (* [`Eof_is_clean] takes any end of stream for a clean one. *)
    let go () =
      if bug = `Eof_is_clean then try go () with End_of_file -> None
      else go ()
    in
    if bug = `No_timeout then go () else Eio.Time.with_timeout_exn clock 1.0 go
end

(* (e) A client that retries. The server answers one frame. *)
module Client = struct
  let fetch ?(bug = `None) ~clock net addr =
    let once () =
      Eio.Switch.run @@ fun sw ->
      let flow = Eio.Net.connect ~sw net addr in
      match Frame.read ~clock flow with
      | Some v -> v
      | None -> raise Frame.Truncated
    in
    let rec attempt n =
      match
        if bug = `No_timeout then once ()
        else Eio.Time.with_timeout_exn clock 1.0 once
      with
      | v -> Ok v
      | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
        when n < 3 ->
          Eio.Time.sleep clock (0.1 *. float (1 lsl n));
          attempt (n + 1)
      | exception (Eio.Io _ | Eio.Time.Timeout | Frame.Truncated)
        when n < 3 && bug <> `Refused_only ->
          Eio.Time.sleep clock (0.1 *. float (1 lsl n));
          attempt (n + 1)
      | exception (Eio.Io _ | Eio.Time.Timeout | Frame.Truncated) ->
          Error `Unreachable
    in
    attempt 1
end

(* (f) UDP request/response with retransmission. A message is "id:body". *)
module Rpc = struct
  let split s =
    match String.index_opt s ':' with
    | Some i ->
        Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> None

  let serve sock =
    let buf = Cstruct.create 512 in
    while true do
      let src, n = Eio.Net.recv sock buf in
      match split (Cstruct.to_string ~len:n buf) with
      | Some (id, body) ->
          Eio.Net.send sock ~dst:src
            [ Cstruct.of_string (id ^ ":" ^ String.uppercase_ascii body) ]
      | None -> ()
    done

  let call ?(bug = `None) ~clock sock dst ~id body =
    let buf = Cstruct.create 512 in
    let rec wait () =
      let _, n = Eio.Net.recv sock buf in
      match split (Cstruct.to_string ~len:n buf) with
      | Some (id', reply) when id' = id || bug = `Any_reply -> reply
      | _ -> wait ()
    in
    let rec attempt n =
      Eio.Net.send sock ~dst [ Cstruct.of_string (id ^ ":" ^ body) ];
      match Eio.Time.with_timeout_exn clock 0.5 wait with
      | reply -> Ok reply
      | exception Eio.Time.Timeout when n < 8 -> attempt (n + 1)
      | exception Eio.Time.Timeout -> Error `Timeout
    in
    attempt 1
end
