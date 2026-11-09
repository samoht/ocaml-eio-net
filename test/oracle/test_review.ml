(* Socket behaviour a review of eio-net.mem raised, measured on Linux and
   macOS. A case names what it checks; none comes from gVisor. *)

open Oracle

let file = "review"

let per_stream ~test kinds expect body =
  List.map
    (fun k ->
      case ~file ~test ~variant:(kind_name (Stream k))
        ~ipv6:(needs_ipv6 (Stream k)) (expect k) (fun ctx -> body ctx k))
    kinds

let cases =
  List.concat
    [
      (* An empty write after our own send shutdown writes nothing on Linux;
         macOS fails it with EPIPE. *)
      per_stream ~test:"EmptyWriteAfterShutdownSend" [ Tcp4; Unix_stream ]
        (fun _ -> expect ~macos:"reset" "wrote 0")
        (fun ctx k ->
          let c, _, _ = stream_pair ctx k in
          Eio.Flow.shutdown c `Send;
          outcome (fun () ->
              Fmt.str "wrote %d" (Eio.Flow.single_write c [ Cstruct.empty ])));
    ]

let suite = Oracle.suite "review" cases
