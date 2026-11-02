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
    (fun () ->
      Eio_net.Frame.send_batch a
        (List.map (fun s -> [ Cstruct.of_string s ]) payloads))
    (fun () ->
      let rec receive acc =
        if List.length acc = List.length payloads then List.rev acc
        else receive (List.rev_append (Eio_net.Frame.recv_batch b) acc)
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
  (* Each payload is distinct, so a frame sent twice or overwritten by a later
     one shows. *)
  let payload i =
    Fmt.str "%03d" i ^ String.make 60000 (Char.chr (65 + (i mod 26)))
  in
  let payloads = List.init count payload in
  let frames = List.map (fun p -> [ Cstruct.of_string p ]) payloads in
  let received = ref [] in
  Eio.Fiber.both
    (fun () -> Eio_net.Frame.send_batch a frames)
    (fun () ->
      while List.length !received < count do
        received := List.rev_append (Eio_net.Frame.recv_batch b) !received
      done);
  Alcotest.(check (list string))
    "all backpressured datagrams arrive, in order and intact" payloads
    (List.rev !received);
  Unix.close fa;
  Unix.close fb

(* Each end sends while it receives, as a packet channel is used: a frame
   waiting to go out is not overwritten by one coming in. *)
let test_connected_datagram_duplex () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  List.iter
    (fun fd ->
      Unix.setsockopt_int fd Unix.SO_SNDBUF 65536;
      Unix.setsockopt_int fd Unix.SO_RCVBUF 65536)
    [ fa; fb ];
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_batch:1 fa in
  let b = Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_batch:1 fb in
  let count = 200 in
  let payloads side =
    List.init count (fun i -> Fmt.str "%s-%03d%s" side i (String.make 8000 '.'))
  in
  let run self side =
    let got = ref [] in
    Eio.Fiber.both
      (fun () ->
        Eio_net.Frame.send_batch self
          (List.map (fun p -> [ Cstruct.of_string p ]) (payloads side)))
      (fun () ->
        while List.length !got < count do
          got := List.rev_append (Eio_net.Frame.recv_batch self) !got
        done);
    List.rev !got
  in
  let from_b = ref [] and from_a = ref [] in
  Eio.Fiber.both (fun () -> from_b := run a "a") (fun () -> from_a := run b "b");
  Alcotest.(check (list string)) "a receives what b sent" (payloads "b") !from_b;
  Alcotest.(check (list string)) "b receives what a sent" (payloads "a") !from_a;
  Unix.close fa;
  Unix.close fb

(* Two fibers send batches on one channel at once while the socket pushes
   back: each frame arrives once and intact, each fiber's in its order. *)
let test_connected_datagram_concurrent_senders () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  Unix.setsockopt_int fa Unix.SO_SNDBUF 65536;
  Unix.setsockopt_int fb Unix.SO_RCVBUF 65536;
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_batch:4 fa in
  let b = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fb in
  let count = 100 in
  let payloads side =
    List.init count (fun i -> Fmt.str "%s-%03d%s" side i (String.make 8000 '.'))
  in
  let send side () =
    Eio_net.Frame.send_batch a
      (List.map (fun p -> [ Cstruct.of_string p ]) (payloads side))
  in
  let got = ref [] in
  Eio.Fiber.all
    [
      send "x";
      send "y";
      (fun () ->
        while List.length !got < 2 * count do
          got := List.rev_append (Eio_net.Frame.recv_batch b) !got
        done);
    ];
  let from side =
    List.filter (fun p -> String.starts_with ~prefix:side p) (List.rev !got)
  in
  Alcotest.(check (list string)) "x's frames" (payloads "x") (from "x-");
  Alcotest.(check (list string)) "y's frames" (payloads "y") (from "y-");
  Unix.close fa;
  Unix.close fb

(* A send the system refuses, to a peer that has closed its end, is an
   [Eio.Io] error, as Eio's own sockets raise it. *)
let test_connected_datagram_send_refused () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fa in
  Unix.close fb;
  (match Eio_net.Frame.send a [ Cstruct.of_string "to no one" ] with
  | () -> Alcotest.fail "a send to a closed peer went through"
  | exception Eio.Io _ -> ());
  Unix.close fa

(* A write the system refuses is an [Eio.Io] error: the read end of a pipe
   takes no write (EBADF). *)
let test_offload_write_refused () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Unix.pipe ~cloexec:true () in
  let fd = Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true r in
  Unix.close w;
  match
    Eio_net_unix.Stream.offload_single_write fd [ Cstruct.of_string "x" ]
  with
  | _ -> Alcotest.fail "a write to the read end of a pipe went through"
  | exception Eio.Io _ -> ()

(* A connected UDP socket holds a datagram from its peer and the error an
   ICMP port unreachable left when the peer had gone. A batch that receives
   the datagram and then meets the error returns the datagram, and the next
   receive reports the error: neither is lost, in whichever order the system
   reports them. *)
let test_connected_datagram_error_after_a_datagram () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket () =
    let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_DGRAM 0 in
    Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    fd
  in
  let fa = socket () and fb = socket () in
  Unix.connect fa (Unix.getsockname fb);
  Unix.connect fb (Unix.getsockname fa);
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fa in
  ignore (Unix.send_substring fb "kept" 0 4 [] : int);
  Unix.close fb;
  ignore (Unix.send_substring fa "lost" 0 4 [] : int);
  Eio.Time.sleep env#clock 0.1;
  let receive () =
    match
      Eio.Time.with_timeout_exn env#clock 1.0 (fun () ->
          Eio_net.Frame.recv_batch a)
    with
    | frames -> frames
    | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
        [ "refused" ]
  in
  let first = receive () in
  let second = receive () in
  Alcotest.(check (list string))
    "the datagram and the error, in either order" [ "kept"; "refused" ]
    (List.sort String.compare (first @ second));
  Unix.close fa

(* A datagram longer than [max_frame] is refused, as [Datagram.of_socket]
   refuses it, not delivered cut short as if it were whole. *)
let test_connected_datagram_oversized () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let a = Eio_net_unix.Frame.of_connected_datagram_fd ~sw fa in
  let b = Eio_net_unix.Frame.of_connected_datagram_fd ~sw ~max_frame:8 fb in
  let refused f =
    match f () with
    | _ -> false
    | exception Eio.Io (Eio.Exn.X (Eio_net.Datagram.Too_long { max = 8 }), _) ->
        true
  in
  Eio_net.Frame.send a [ Cstruct.of_string "123456789" ];
  Alcotest.(check bool)
    "recv refuses it" true
    (refused (fun () -> Eio_net.Frame.recv b));
  Eio_net.Frame.send a [ Cstruct.of_string "123456789" ];
  Alcotest.(check bool)
    "recv_batch refuses it" true
    (refused (fun () -> Eio_net.Frame.recv_batch b));
  Eio_net.Frame.send a [ Cstruct.of_string "12345678" ];
  Alcotest.(check (list string))
    "the channel stays usable" [ "12345678" ]
    (Eio_net.Frame.recv_batch b);
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

(* A path of [n] bytes under a short temporary directory. *)
let of_length n =
  let dir = Filename.temp_dir "len" "" in
  let pad = n - String.length dir - 1 in
  if pad < 1 then Alcotest.failf "%s is longer than %d bytes" dir n;
  Filename.concat dir (String.make pad 's')

(* A path of max_length bytes binds; one byte more is refused before any
   system call, as every sockaddr_un is. *)
let test_local_max_length () =
  let n = Eio_net_unix.Local.max_length in
  Alcotest.(check bool) "103 or 107" true (n = 103 || n = 107);
  let bind p =
    let s = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close s)
      (fun () ->
        match Unix.bind s (Unix.ADDR_UNIX p) with
        | () ->
            Unix.unlink p;
            Ok ()
        | exception Unix.Unix_error (e, _, _) -> Error e)
  in
  Alcotest.(check bool) "max_length binds" true (bind (of_length n) = Ok ());
  Alcotest.(check bool)
    "one more byte is too long" true
    (bind (of_length (n + 1)) = Error Unix.ENAMETOOLONG)

let kind_perm p =
  let s = Unix.lstat p in
  (s.st_kind, s.st_perm)

(* Local.listen makes the socket's file with [perm]: the stat taken as it
   returns reads [perm] under a process umask of 0o022, which would leave
   0o640 of 0o660. Two modes, so a hard-coded 0o600 fails one of them. The
   process's umask is the one it had, and the socket serves. *)
let test_local_listen_perm () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let check perm =
    let p = Filename.concat (Filename.temp_dir "perm" "") "s" in
    let before = Unix.umask 0o022 in
    Fun.protect
      ~finally:(fun () -> ignore (Unix.umask before))
      (fun () ->
        Eio.Switch.run @@ fun sw ->
        let listening =
          Eio_net_unix.Local.listen ~sw ~backlog:4 ~perm (Fpath.v p)
        in
        let s = Unix.lstat p in
        Alcotest.(check bool) "a socket" true (s.st_kind = Unix.S_SOCK);
        Alcotest.(check int) (Fmt.str "mode %o" perm) perm s.st_perm;
        Alcotest.(check int) "the umask restored" 0o022 (Unix.umask 0o022);
        Eio.Fiber.both
          (fun () ->
            let flow, _ = Eio.Net.accept ~sw listening in
            Eio.Flow.copy_string "served" flow;
            Eio.Flow.close flow)
          (fun () ->
            let flow = Eio_net_unix.Local.connect ~sw ~net (Fpath.v p) in
            Alcotest.(check string)
              "served" "served"
              (Eio.Buf_read.parse_exn ~max_size:64 Eio.Buf_read.take_all flow)));
    Alcotest.(check bool) "removed on release" false (Sys.file_exists p)
  in
  check 0o600;
  check 0o660

(* A socket left at the path by a listener that has gone is replaced; a file
   of any other kind is not, and the bind's EADDRINUSE is the Eio.Io error
   Eio.Net.listen would raise, the file untouched. *)
let test_local_listen_existing () =
  Eio_main.run @@ fun _env ->
  let dir = Filename.temp_dir "existing" "" in
  let stale = Filename.concat dir "stale" in
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX stale);
  Unix.close fd;
  Eio.Switch.run (fun sw ->
      ignore
        (Eio_net_unix.Local.listen ~sw ~backlog:4 ~perm:0o600 (Fpath.v stale));
      Alcotest.(check bool)
        "the stale socket replaced" true
        (match kind_perm stale with Unix.S_SOCK, 0o600 -> true | _ -> false));
  let file = Filename.concat dir "file" in
  Out_channel.with_open_bin file (fun oc -> output_string oc "kept");
  (match
     Eio.Switch.run (fun sw ->
         Eio_net_unix.Local.listen ~sw ~backlog:4 ~perm:0o600 (Fpath.v file))
   with
  | _ -> Alcotest.failf "bound over the regular file %s" file
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _)
    ->
      ());
  Alcotest.(check string)
    "the file untouched" "kept"
    (In_channel.with_open_bin file In_channel.input_all)

(* A path no socket address holds is refused as ENAMETOOLONG, an Eio.Io
   error, and leaves nothing at the path nor a changed umask. Releasing the
   switch after another file took the socket's path leaves that file. *)
let test_local_listen_errors () =
  Eio_main.run @@ fun _env ->
  let long = of_length (Eio_net_unix.Local.max_length + 1) in
  let before = Unix.umask 0o022 in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.umask before))
    (fun () ->
      (match
         Eio.Switch.run (fun sw ->
             Eio_net_unix.Local.listen ~sw ~backlog:4 ~perm:0o600 (Fpath.v long))
       with
      | _ -> Alcotest.failf "bound %s" long
      | exception
          Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENAMETOOLONG, _, _)), _)
        ->
          ());
      Alcotest.(check bool) "nothing left" false (Sys.file_exists long);
      Alcotest.(check int) "the umask restored" 0o022 (Unix.umask 0o022));
  let p = Filename.concat (Filename.temp_dir "moved" "") "s" in
  Eio.Switch.run (fun sw ->
      ignore (Eio_net_unix.Local.listen ~sw ~backlog:4 ~perm:0o600 (Fpath.v p));
      Unix.unlink p;
      Out_channel.with_open_bin p (fun oc -> output_string oc "another"));
  Alcotest.(check string)
    "another file kept" "another"
    (In_channel.with_open_bin p In_channel.input_all)

(* A TCP address in use is the Eio.Io error EADDRINUSE, where Eio's POSIX
   backend raises bind(2)'s Unix_error. *)
let test_tcp_listen_in_use () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let ip = Eio.Net.Ipaddr.V4.loopback in
  let first = Eio_net_unix.Tcp.listen ~sw ~backlog:4 net ip 0 in
  let port =
    match Eio.Net.listening_addr first with
    | `Tcp (_, port) -> port
    | a -> Alcotest.failf "bound to %a" Eio.Net.Sockaddr.pp a
  in
  match Eio_net_unix.Tcp.listen ~sw ~backlog:4 net ip port with
  | _ -> Alcotest.failf "bound port %d twice" port
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _)
    ->
      ()

(* Linking eio-net.unix registers its accept classifier: a peer that reset
   its connection before the accept, and a process or system out of
   descriptors, each leave the listener whole. *)
let test_accept_failure_is_registered () =
  let classify e = Eio_net_accept.classify (Eio_unix.Err.v e "accept" "") in
  let answer = function
    | Some `Aborted -> "aborted"
    | Some `Descriptors -> "descriptors"
    | None -> "none"
  in
  List.iter
    (fun (e, want) ->
      Alcotest.(check string) (Unix.error_message e) want (answer (classify e)))
    [
      (Unix.ECONNABORTED, "aborted");
      (Unix.EMFILE, "descriptors");
      (Unix.ENFILE, "descriptors");
      (Unix.EBADF, "none");
    ]

let suite =
  ( "eio_net_unix",
    [
      Alcotest.test_case "the accept classifier is registered" `Quick
        test_accept_failure_is_registered;
      Alcotest.test_case "a local listener's mode, from its creation" `Quick
        test_local_listen_perm;
      Alcotest.test_case "a local listener over an existing file" `Quick
        test_local_listen_existing;
      Alcotest.test_case "a local listener refused, and released" `Quick
        test_local_listen_errors;
      Alcotest.test_case "a TCP listener on a port in use" `Quick
        test_tcp_listen_in_use;
      Alcotest.test_case "the longest path an address holds" `Quick
        test_local_max_length;
      Alcotest.test_case "a local socket by a path of any length" `Quick
        test_local_connect;
      Alcotest.test_case "a local socket that is missing or refusing" `Quick
        test_local_errors;
      Alcotest.test_case "connected datagram batch" `Quick
        test_connected_datagram_batch;
      Alcotest.test_case "connected datagram duplex" `Quick
        test_connected_datagram_duplex;
      Alcotest.test_case "connected datagram concurrent senders" `Quick
        test_connected_datagram_concurrent_senders;
      Alcotest.test_case "connected datagram send refused" `Quick
        test_connected_datagram_send_refused;
      Alcotest.test_case "offloaded write refused" `Quick
        test_offload_write_refused;
      Alcotest.test_case "connected datagram error after a datagram" `Quick
        test_connected_datagram_error_after_a_datagram;
      Alcotest.test_case "connected datagram longer than max_frame" `Quick
        test_connected_datagram_oversized;
      Alcotest.test_case "connected datagram backpressure" `Quick
        test_connected_datagram_backpressure;
      Alcotest.test_case "reject excessive datagram batch" `Quick
        test_connected_datagram_rejects_excessive_batch;
    ] )
