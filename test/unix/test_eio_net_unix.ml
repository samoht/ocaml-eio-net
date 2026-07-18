(* Connected datagrams preserve one frame per datagram even when a batch spans
   several system calls. The deliberately small cap exercises chunking and the
   final partial batch without weakening the exact ordering assertion. *)
let test_connected_datagram_batch () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_batch:4 fa in
  let b = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fb in
  let payloads = List.init 17 (fun i -> Fmt.str "frame-%02d" i) in
  let got = ref [] in
  Eio.Fiber.both
    (fun () -> a.Eio_net.Frame.send_batch (List.map Rope.of_string payloads))
    (fun () ->
      let rec receive acc =
        if List.length acc = List.length payloads then List.rev acc
        else receive (List.rev_append (b.Eio_net.Frame.recv_batch ()) acc)
      in
      got := receive []);
  Alcotest.(check (list string))
    "batched datagrams retain frame boundaries" payloads !got;
  Unix.close fa;
  Unix.close fb

let suite =
  ( "eio_net_unix",
    [
      Alcotest.test_case "connected datagram batch" `Quick
        test_connected_datagram_batch;
    ] )
