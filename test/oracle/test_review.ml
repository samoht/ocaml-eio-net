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
      (* A connect bound to an address of another family fails: EINVAL on
         Linux, EAFNOSUPPORT on macOS. *)
      [
        case ~file ~test:"BindToOtherFamily" ~variant:"127.0.0.1 to ::1"
          ~ipv6:true (expect ~macos:"EAFNOSUPPORT" ~mem:"EAFNOSUPPORT" "EINVAL")
          (fun ctx ->
            let l = listen ctx (`Tcp (v6_loopback, 0)) in
            outcome (fun () ->
                ignore
                  (Eio.Net.connect ~sw:ctx.sw
                     ~bind_to:(`Tcp (v4_loopback, 0))
                     ctx.net (Eio.Net.listening_addr l));
                "connected"));
        case ~file ~test:"BindToOtherFamily" ~variant:"unix path to tcp"
          (expect ~macos:"EAFNOSUPPORT" ~mem:"EAFNOSUPPORT" "EINVAL")
          (fun ctx ->
            let l = listen ctx (`Tcp (v4_loopback, 0)) in
            let bind_to = stream_addr ctx Unix_stream in
            outcome (fun () ->
                ignore
                  (Eio.Net.connect ~sw:ctx.sw ~bind_to ctx.net
                     (Eio.Net.listening_addr l));
                "connected"));
      ];
    ]

let suite = Oracle.suite "review" cases
