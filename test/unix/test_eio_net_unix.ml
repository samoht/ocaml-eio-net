(* A deliberately small batch cap exercises chunking and the final partial
   batch without weakening the ordering assertion. *)
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

(* A full Darwin datagram socket reports [ENOBUFS], not [EAGAIN], from
   [sendmsg_x]: ordinary backpressure, so yield and retry. Small socket buffers
   make the condition deterministic. *)
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

let test_connected_datagram_rejects_excessive_batch () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close fa;
      Unix.close fb)
    (fun () ->
      Alcotest.check_raises "bounded native batch"
        (Invalid_argument "Eio_net_unix.Frame: max_batch > 64") (fun () ->
          ignore
            (Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_batch:65 fa)))

(* A listening socket bound where a socket address reaches, then moved to
   [path], which may be longer: a socket is reached by the file it is, not by
   the name it was bound to. *)
let listen_at ~sw path =
  let short = Filename.concat (Filename.temp_dir "listen" "") "s" in
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX short);
  Unix.listen fd 4;
  Unix.rename short path;
  Eio_unix.Net.import_socket_listening ~sw ~close_unix:true fd

(* A server at [path] answering each connection with what it read,
   prefixed. *)
let with_server ~sw path f =
  let listening = listen_at ~sw path in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec serve () =
        let flow, _ = Eio.Net.accept ~sw listening in
        let got =
          Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.take_all flow
        in
        Eio.Flow.copy_string ("echo:" ^ got) flow;
        Eio.Flow.close flow;
        serve ()
      in
      serve ());
  f ()

let exchange ~sw ~net path =
  let flow = Eio_net_unix.Local.connect ~sw ~net (Fpath.v path) in
  Eio.Flow.copy_string "ping" flow;
  Eio.Flow.shutdown flow `Send;
  Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.take_all flow

(* A directory whose path is [n] bytes or more. *)
let deep n =
  let rec go d =
    if String.length d >= n then d
    else
      let d = Filename.concat d "a-directory-name-of-some-length" in
      Unix.mkdir d 0o700;
      go d
  in
  go (Filename.temp_dir "deep" "")

(* Git reaches a socket by a path of any length (unix-socket.c). Here one of
   more than 200 bytes, which no sockaddr_un holds (104 bytes on macOS, 108
   on Linux), and a short one. *)
let test_local_connect () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let long = Filename.concat (deep 200) "socket" in
  Alcotest.(check bool) "longer than an address" true (String.length long > 200);
  with_server ~sw long (fun () ->
      Alcotest.(check string) "a long path" "echo:ping" (exchange ~sw ~net long));
  let short = Filename.concat (Filename.temp_dir "short" "") "socket" in
  with_server ~sw short (fun () ->
      Alcotest.(check string)
        "a short path" "echo:ping" (exchange ~sw ~net short))

(* A socket that is not there, under a long path or a short one, is
   Not_found; one nobody listens on is a refused connection, as
   Eio.Net.connect reports them. *)
let test_local_errors () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let missing p =
    match Eio_net_unix.Local.connect ~sw ~net (Fpath.v p) with
    | _ -> Alcotest.failf "connected to %s" p
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  in
  missing (Filename.concat (deep 200) "socket");
  missing (Filename.concat (deep 200) "absent/socket");
  missing (Filename.concat (Filename.temp_dir "short" "") "socket");
  let refused p =
    Eio.Switch.run (fun sw -> Eio.Net.close (listen_at ~sw p));
    match Eio_net_unix.Local.connect ~sw ~net (Fpath.v p) with
    | _ -> Alcotest.failf "connected to %s, which nobody listens on" p
    | exception
        Eio.Io (Eio.Net.E (Eio.Net.Connection_failure (Eio.Net.Refused _)), _)
      ->
        ()
  in
  refused (Filename.concat (deep 200) "socket");
  refused (Filename.concat (Filename.temp_dir "short" "") "socket")

let suite =
  ( "eio_net_unix",
    [
      Alcotest.test_case "a local socket by a path of any length" `Quick
        test_local_connect;
      Alcotest.test_case "a local socket that is missing or refusing" `Quick
        test_local_errors;
      Alcotest.test_case "connected datagram batch" `Quick
        test_connected_datagram_batch;
      Alcotest.test_case "connected datagram backpressure" `Quick
        test_connected_datagram_backpressure;
      Alcotest.test_case "reject excessive datagram batch" `Quick
        test_connected_datagram_rejects_excessive_batch;
    ] )
