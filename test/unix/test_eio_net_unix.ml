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

(* A full Darwin datagram socket reports [ENOBUFS], rather than [EAGAIN], from
   [sendmsg_x].  It is ordinary backpressure: yield to the receiver and retry,
   preserving every datagram.  Small socket buffers make the condition
   deterministic without making the test transfer much data. *)
let test_connected_datagram_backpressure () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  Unix.setsockopt_int fa Unix.SO_SNDBUF 65536;
  Unix.setsockopt_int fb Unix.SO_RCVBUF 65536;
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fa in
  let b = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fb in
  let count = 256 in
  let payload = String.make 60000 'x' in
  let frames = List.init count (fun _ -> Rope.of_string payload) in
  let received = ref 0 in
  Eio.Fiber.both
    (fun () -> a.Eio_net.Frame.send_batch frames)
    (fun () ->
      while !received < count do
        received := !received + List.length (b.Eio_net.Frame.recv_batch ())
      done);
  Alcotest.(check int) "all backpressured datagrams arrive" count !received;
  Unix.close fa;
  Unix.close fb

let suite =
  ( "eio_net_unix",
    [
      Alcotest.test_case "connected datagram batch" `Quick
        test_connected_datagram_batch;
      Alcotest.test_case "connected datagram backpressure" `Quick
        test_connected_datagram_backpressure;
    ] )
