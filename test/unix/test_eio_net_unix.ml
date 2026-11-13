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
   [sendmsg_x]: ordinary backpressure, so back off and retry. Small socket
   buffers make the condition deterministic. *)
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

(* A channel is usable until its switch finishes: a send after that raises
   the error Eio raises for a closed descriptor, as a receive does, and
   nothing reaches the peer. *)
let test_connected_datagram_send_after_switch () =
  Eio_main.run @@ fun _env ->
  let fa, fb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  let a =
    Eio.Switch.run @@ fun sw ->
    Eio_net_unix.Frame.of_connected_datagram_fd ~sw fa
  in
  let closed f =
    match f () with
    | _ -> false
    | exception Invalid_argument m ->
        String.ends_with ~suffix:"file descriptor used after calling close!" m
  in
  Alcotest.(check bool)
    "a receive raises" true
    (closed (fun () -> Eio_net.Frame.recv a));
  Alcotest.(check bool)
    "a send raises" true
    (closed (fun () ->
         Eio_net.Frame.send a [ Cstruct.of_string "after-switch" ]));
  Unix.set_nonblock fb;
  (match Unix.recv fb (Bytes.create 64) 0 64 [] with
  | n -> Alcotest.failf "the peer received %d bytes" n
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ());
  Unix.close fa;
  Unix.close fb

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

(* A write of 256 empty buffers and then a payload writes the payload: a
   call passes at most 256 iovecs, and an empty buffer takes none of them. *)
let test_offload_write_skips_empty_buffers () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock a;
  let fd = Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true a in
  let bufs =
    List.init 256 (fun _ -> Cstruct.empty) @ [ Cstruct.of_string "payload" ]
  in
  Alcotest.(check int)
    "the payload is written" 7
    (Eio_net_unix.Stream.offload_single_write fd bufs);
  Alcotest.(check int)
    "empty buffers alone write nothing" 0
    (Eio_net_unix.Stream.offload_single_write fd [ Cstruct.empty ]);
  let got = Bytes.create 16 in
  let n = Unix.read b got 0 16 in
  Alcotest.(check string)
    "the peer reads it" "payload" (Bytes.sub_string got 0 n);
  Unix.close b

(* A call reads no further into its buffers than the 256 it passes, so a
   partial write costs the same whatever follows: here the list never ends,
   and the call writes 256 one-byte buffers of it. *)
let test_offload_write_reads_a_bounded_prefix () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock a;
  let fd = Eio_unix.Fd.of_unix ~sw ~blocking:false ~close_unix:true a in
  let rec endless = Cstruct.empty :: Cstruct.of_string "x" :: endless in
  Alcotest.(check int)
    "256 bytes are written" 256
    (Eio_net_unix.Stream.offload_single_write fd endless);
  Unix.close b

(* A connected UDP socket holds a datagram from its peer and the error an
   ICMP port unreachable left when the peer had gone. A batch that receives
   the datagram and then meets the error returns the datagram, and the next
   receive reports the error: neither is lost, in whichever order the system
   reports them. Each receive waits for the socket to be readable, which the
   datagram and the error each make it. *)
let test_connected_datagram_error_after_a_datagram () =
  Eio_main.run @@ fun _env ->
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
  let receive () =
    match Eio_net.Frame.recv_batch a with
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

(* A server at [path] answering each connection with what it read, prefixed
   with [name]; [accepted] counts its connections. *)
let with_server ?(name = "echo") ?(accepted = ref 0) ~sw path f =
  let listening = listen_at ~sw path in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec serve () =
        let flow, _ = Eio.Net.accept ~sw listening in
        incr accepted;
        let got =
          Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.take_all flow
        in
        Eio.Flow.copy_string (name ^ ":" ^ got) flow;
        Eio.Flow.close flow;
        serve ()
      in
      serve ());
  f ()

let ping flow =
  Eio.Flow.copy_string "ping" flow;
  Eio.Flow.shutdown flow `Send;
  Eio.Buf_read.parse_exn ~max_size:4096 Eio.Buf_read.take_all flow

let exchange ~sw ~net path =
  ping (Eio_net_unix.Local.connect ~sw ~net (Fpath.v path))

let exchange_at ~sw ~net path =
  ping (Eio_net_unix.Local.connect_at ~sw ~net path)

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
   on Linux), reached through the directory capability that holds it, and a
   short one, reached through the network and through its directory. *)
let test_local_connect () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  let long = Filename.concat (deep 200) "socket" in
  Alcotest.(check bool) "longer than an address" true (String.length long > 200);
  with_server ~sw long (fun () ->
      Alcotest.(check string)
        "a long path" "echo:ping"
        (exchange_at ~sw ~net Eio.Path.(fs / long)));
  let short = Filename.concat (Filename.temp_dir "short" "") "socket" in
  with_server ~sw short (fun () ->
      Alcotest.(check string)
        "a short path" "echo:ping" (exchange ~sw ~net short);
      Alcotest.(check string)
        "a short path, through its directory" "echo:ping"
        (exchange_at ~sw ~net Eio.Path.(fs / short)))

(* Local.connect goes through the network it is handed and nothing else: a
   path no socket address holds is refused with ENAMETOOLONG before the
   network is asked, whether it is the host's or an in-memory one that knows
   no such socket, and the server at that path accepts nothing. *)
let test_local_connect_through_net_alone () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let long = Filename.concat (deep 200) "socket" in
  let accepted = ref 0 in
  with_server ~accepted ~sw long (fun () ->
      let refused net =
        match Eio_net_unix.Local.connect ~sw ~net (Fpath.v long) with
        | _ -> Alcotest.failf "connected to %s" long
        | exception
            Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENAMETOOLONG, _, _)), _)
          ->
            ()
      in
      refused (Eio_net_mem.v ~sw ());
      refused (Eio.Stdenv.net env);
      Eio.Fiber.yield ();
      Alcotest.(check int) "the server accepted nothing" 0 !accepted)

(* Local.connect_at resolves a path through the directory capability it is
   part of: a path that leaves a confined directory is refused, as Eio
   refuses to open one, and so is a socket's name that is a symbolic link,
   whose target the capability does not vouch for. The server outside
   accepts nothing. *)
let test_local_connect_at_is_confined () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  let base = Filename.temp_dir "confined" "" in
  let inside = Filename.concat base "inside" in
  let outside = Filename.concat base "outside" in
  Unix.mkdir inside 0o700;
  Unix.mkdir outside 0o700;
  let socket = Filename.concat outside "socket" in
  Unix.symlink socket (Filename.concat inside "link");
  let accepted = ref 0 in
  with_server ~accepted ~sw socket (fun () ->
      let dir = Eio.Path.open_dir ~sw Eio.Path.(fs / inside) in
      (match
         Eio_net_unix.Local.connect_at ~sw ~net
           Eio.Path.(dir / "../outside/socket")
       with
      | _ -> Alcotest.fail "connected outside the directory"
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
      (match Eio_net_unix.Local.connect_at ~sw ~net Eio.Path.(dir / "link") with
      | _ -> Alcotest.fail "connected through a symbolic link"
      | exception Eio.Io (Eio.Fs.E Eio.Fs.Symlink, _) -> ());
      (match
         Eio_net_unix.Local.connect_at ~sw ~net Eio.Path.(fs / inside / "link")
       with
      | _ -> Alcotest.fail "connected through a symbolic link from the root"
      | exception Eio.Io (Eio.Fs.E Eio.Fs.Symlink, _) -> ());
      Eio.Fiber.yield ();
      Alcotest.(check int) "the server accepted nothing" 0 !accepted)

(* A socket that is not there, under a long path or a short one, is
   Not_found; one nobody listens on is a refused connection, as
   Eio.Net.connect reports them. *)
let test_local_errors () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  let missing f p =
    match f p with
    | _ -> Alcotest.failf "connected to %s" p
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  in
  let connect p = Eio_net_unix.Local.connect ~sw ~net (Fpath.v p) in
  let connect_at p = Eio_net_unix.Local.connect_at ~sw ~net Eio.Path.(fs / p) in
  missing connect_at (Filename.concat (deep 200) "socket");
  missing connect_at (Filename.concat (deep 200) "absent/socket");
  missing connect (Filename.concat (Filename.temp_dir "short" "") "socket");
  missing connect_at (Filename.concat (Filename.temp_dir "short" "") "socket");
  let refused f p =
    Eio.Switch.run (fun sw -> Eio.Net.close (listen_at ~sw p));
    match f p with
    | _ -> Alcotest.failf "connected to %s, which nobody listens on" p
    | exception
        Eio.Io (Eio.Net.E (Eio.Net.Connection_failure (Eio.Net.Refused _)), _)
      ->
        ()
  in
  refused connect_at (Filename.concat (deep 200) "socket");
  refused connect (Filename.concat (Filename.temp_dir "short" "") "socket");
  refused connect_at (Filename.concat (Filename.temp_dir "short" "") "socket")

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

let file_id p =
  let s = Unix.lstat p in
  (s.st_dev, s.st_ino)

let listen ?replace ~sw ~net ~fs ?(perm = 0o600) p =
  Eio_net_unix.Local.listen ?replace ~sw ~net ~backlog:4 ~perm Eio.Path.(fs / p)

(* Local.listen makes the socket's file with [perm]: the stat taken as it
   returns reads [perm] under a process umask of 0o022, which would leave
   0o640 of 0o660. Two modes, so a hard-coded 0o600 fails one of them. The
   process's umask is the one it had, the socket serves, and the directory
   holds the socket alone. *)
let test_local_listen_perm () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let check perm =
    let dir = Filename.temp_dir "perm" "" in
    let p = Filename.concat dir "s" in
    let before = Unix.umask 0o022 in
    Fun.protect
      ~finally:(fun () -> ignore (Unix.umask before))
      (fun () ->
        Eio.Switch.run @@ fun sw ->
        let listening = listen ~sw ~net ~fs ~perm p in
        let s = Unix.lstat p in
        Alcotest.(check bool) "a socket" true (s.st_kind = Unix.S_SOCK);
        Alcotest.(check int) (Fmt.str "mode %o" perm) perm s.st_perm;
        Alcotest.(check int) "the umask unchanged" 0o022 (Unix.umask 0o022);
        Alcotest.(check (array string))
          "the socket alone" [| "s" |] (Sys.readdir dir);
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

(* A message read to its peer's end of sending comes with the descriptors
   sent beside it, a pipe's write end here, which the reader then writes
   through; past [max] bytes the read stops, too long. *)
let test_local_receive () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let p = Filename.concat (Filename.temp_dir "receive" "") "s" in
  Eio.Switch.run @@ fun sw ->
  let listening = listen ~sw ~net ~fs p in
  let send message =
    let r, w = Unix.pipe ~cloexec:true () in
    let flow = Eio.Net.connect ~sw net (`Unix p) in
    let fds = [ Eio_unix.Fd.of_unix ~sw ~close_unix:true w ] in
    Eio_unix.Net.send_msg flow ~fds [ Cstruct.of_string message ];
    Eio.Flow.shutdown flow `Send;
    r
  in
  let receive ~max =
    let flow, _ = Eio.Net.accept ~sw listening in
    Eio_net_unix.Local.receive ~sw ~max_fds:2 ~max flow
  in
  let r = send "a request" in
  (match receive ~max:64 with
  | Ok (message, [ w ]) ->
      Alcotest.(check string) "the message" "a request" message;
      Eio_unix.Fd.use_exn "w" w (fun w ->
          ignore (Unix.write_substring w "x" 0 1 : int));
      let b = Bytes.create 1 in
      ignore (Unix.read r b 0 1 : int);
      Alcotest.(check string) "through the descriptor" "x" (Bytes.to_string b);
      Unix.close r
  | Ok (_, fds) -> Alcotest.failf "%d descriptors" (List.length fds)
  | Error `Too_long -> Alcotest.fail "too long");
  let r = send (String.make 100 'a') in
  (match receive ~max:64 with
  | Error `Too_long -> ()
  | Ok _ -> Alcotest.fail "read past max");
  Unix.close r

(* A listener reports the path it was given, not the name it was bound by in
   its private directory: a client connects to the address it reports. *)
let test_local_listening_addr () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let p = Filename.concat (Filename.temp_dir "addr" "") "s" in
  Eio.Switch.run @@ fun sw ->
  let listening = listen ~sw ~net ~fs p in
  let addr = Eio.Net.listening_addr listening in
  Alcotest.(check string)
    "the path given" ("unix:" ^ p)
    (Fmt.str "%a" Eio.Net.Sockaddr.pp addr);
  Eio.Fiber.both
    (fun () ->
      let flow, _ = Eio.Net.accept ~sw listening in
      Eio.Flow.copy_string "here" flow;
      Eio.Flow.close flow)
    (fun () ->
      let flow = Eio.Net.connect ~sw net addr in
      Alcotest.(check string)
        "served at that address" "here"
        (Eio.Buf_read.parse_exn ~max_size:64 Eio.Buf_read.take_all flow))

let address_in_use f =
  match f () with
  | _ -> Alcotest.fail "bound over an existing file"
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EADDRINUSE, _, _)), _)
    ->
      ()

(* A socket already at the path, whether a listener still serves it or not,
   is replaced only when the caller asks for it: otherwise the listen fails
   with the EADDRINUSE Eio.Net.listen would raise, the socket untouched, and
   its listener keeps its clients. A file of any other kind is never
   replaced. *)
let test_local_listen_existing () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_dir "existing" "" in
  let stale = Filename.concat dir "stale" in
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX stale);
  Unix.close fd;
  let id = file_id stale in
  address_in_use (fun () ->
      Eio.Switch.run (fun sw -> listen ~sw ~net ~fs stale));
  Alcotest.(check bool) "the stale socket kept" true (file_id stale = id);
  Eio.Switch.run (fun sw ->
      ignore (listen ~replace:true ~sw ~net ~fs stale);
      Alcotest.(check bool)
        "the stale socket replaced when asked" true
        (match kind_perm stale with Unix.S_SOCK, 0o600 -> true | _ -> false));
  let live = Filename.concat dir "live" in
  Eio.Switch.run (fun sw ->
      with_server ~name:"first" ~sw live (fun () ->
          address_in_use (fun () ->
              Eio.Switch.run (fun sw -> listen ~sw ~net ~fs live));
          Alcotest.(check string)
            "the first listener keeps its clients" "first:ping"
            (exchange ~sw ~net live)));
  let file = Filename.concat dir "file" in
  Out_channel.with_open_bin file (fun oc -> output_string oc "kept");
  address_in_use (fun () ->
      Eio.Switch.run (fun sw -> listen ~replace:true ~sw ~net ~fs file));
  Alcotest.(check string)
    "the file untouched" "kept"
    (In_channel.with_open_bin file In_channel.input_all);
  Alcotest.(check (array string))
    "nothing else left" [| "file"; "live" |]
    (let a = Sys.readdir dir in
     Array.sort compare a;
     a)

(* A path no socket address holds is bound through its directory, and
   served; releasing the switch removes it. Releasing the switch after
   another file took the socket's path leaves that file. *)
let test_local_listen_long () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let long = Filename.concat (deep 200) "socket" in
  Eio.Switch.run (fun sw ->
      let listening = listen ~sw ~net ~fs long in
      Eio.Fiber.both
        (fun () ->
          let flow, _ = Eio.Net.accept ~sw listening in
          Eio.Flow.copy_string "long" flow;
          Eio.Flow.close flow)
        (fun () ->
          Alcotest.(check string)
            "served" "long"
            (Eio.Buf_read.parse_exn ~max_size:64 Eio.Buf_read.take_all
               (Eio_net_unix.Local.connect_at ~sw ~net Eio.Path.(fs / long)))));
  Alcotest.(check bool) "removed on release" false (Sys.file_exists long);
  let p = Filename.concat (Filename.temp_dir "moved" "") "s" in
  Eio.Switch.run (fun sw ->
      ignore (listen ~sw ~net ~fs p);
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

(* The Unix accept classifier reads a peer that reset its connection before
   the accept, and a process or system out of descriptors, as failures that
   leave the listener whole. Linking eio-net.unix leaves the portable
   classifier, a loop's default, as it is. *)
let test_accept_failure () =
  let answer = function
    | Some `Aborted -> "aborted"
    | Some `Descriptors -> "descriptors"
    | None -> "none"
  in
  List.iter
    (fun (e, want, portable) ->
      let exn = Eio_unix.Err.v e "accept" "" in
      Alcotest.(check string)
        (Unix.error_message e) want
        (answer (Eio_net_unix.accept_failure exn));
      Alcotest.(check string)
        (Unix.error_message e ^ ", portably")
        portable
        (answer (Eio_net_accept.classify exn)))
    [
      (Unix.ECONNABORTED, "aborted", "aborted");
      (Unix.EMFILE, "descriptors", "none");
      (Unix.ENFILE, "descriptors", "none");
      (Unix.EBADF, "none", "none");
    ]

(* The system the cases run on, as uname -s names it. *)
let system =
  lazy
    (let ic = Unix.open_process_args_in "uname" [| "uname"; "-s" |] in
     let s = String.trim (In_channel.input_all ic) in
     ignore (Unix.close_process_in ic);
     s)

(* Codes OCaml's Unix names only as EUNKNOWNERR: EPROTO and ENONET, on the
   systems that define them. *)
let unknown_codes () =
  match Lazy.force system with
  | "Linux" -> [ ("EPROTO", 71); ("ENONET", 64) ]
  | "Darwin" -> [ ("EPROTO", 100) ]
  | _ -> []

(* accept(2) on Linux passes on the network error pending on the new
   connection, which consumes it, and a BSD's clears the listener's error it
   returns: the Unix classifier reads each as an aborted accept, followed by
   the next. An error that persists on the listener, EOPNOTSUPP for a socket
   that is not a stream or EPERM from a security module, is no answer, so the
   loop raises it rather than spin. *)
let test_accept_pending_errors () =
  let answer = function
    | Some `Aborted -> "aborted"
    | Some `Descriptors -> "descriptors"
    | None -> "none"
  in
  let check name e want =
    let exn = Eio_unix.Err.v e "accept" "" in
    Alcotest.(check string) name want (answer (Eio_net_unix.accept_failure exn))
  in
  List.iter
    (fun e -> check (Unix.error_message e) e "aborted")
    Unix.[ ENETDOWN; ENOPROTOOPT; EHOSTDOWN; EHOSTUNREACH; ENETUNREACH ];
  List.iter
    (fun (name, n) -> check name (Unix.EUNKNOWNERR n) "aborted")
    (unknown_codes ());
  List.iter
    (fun e -> check (Unix.error_message e) e "none")
    Unix.[ EOPNOTSUPP; EPERM; EINVAL ]

(* The Unix connect classifier reads a refused peer and an unreachable
   network; linking eio-net.unix leaves the portable classifier as it is. *)
let test_connect_failure () =
  let answer = function
    | Some `Refused -> "refused"
    | Some `Unreachable -> "unreachable"
    | None -> "none"
  in
  List.iter
    (fun (e, want, portable) ->
      let exn = Eio_unix.Err.v e "connect" "" in
      Alcotest.(check string)
        (Unix.error_message e) want
        (answer (Eio_net_unix.connect_failure exn));
      Alcotest.(check string)
        (Unix.error_message e ^ ", portably")
        portable
        (answer (Eio_net_connect.classify exn)))
    [
      (Unix.ECONNREFUSED, "refused", "refused");
      (Unix.ENETUNREACH, "unreachable", "none");
      (Unix.EHOSTUNREACH, "unreachable", "none");
      (Unix.ETIMEDOUT, "none", "none");
    ]

(* Lets the event loop run, and take the results of the system threads, 64
   times or until [until ()]. *)
let settle ?(until = fun () -> false) () =
  for _ = 1 to 64 do
    if not (until ()) then Eio_unix.run_in_systhread ignore
  done

(* A listener whose backlog is full: on Linux a connect waits for room, and
   a timeout ends the wait, and an accept that makes room lets it through;
   on macOS a connect is refused at once. *)
let test_local_connect_at_full_backlog () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  let p = Filename.concat (Filename.temp_dir "full" "") "s" in
  let l =
    Eio_net_unix.Local.listen ~sw ~net ~backlog:1 ~perm:0o600 Eio.Path.(fs / p)
  in
  let rec fill n =
    let s = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.set_nonblock s;
    match Unix.connect s (Unix.ADDR_UNIX p) with
    | () ->
        Eio.Switch.on_release sw (fun () -> Unix.close s);
        fill (n + 1)
    | exception Unix.Unix_error ((EAGAIN | ECONNREFUSED), _, _) ->
        Unix.close s;
        n
  in
  Alcotest.(check bool) "the backlog takes some" true (fill 0 > 0);
  let connect () = Eio_net_unix.Local.connect_at ~sw ~net Eio.Path.(fs / p) in
  match Lazy.force system with
  | "Darwin" -> (
      match connect () with
      | _ -> Alcotest.fail "connected past a full backlog"
      | exception
          Eio.Io (Eio.Net.E (Eio.Net.Connection_failure (Eio.Net.Refused _)), _)
        ->
          ())
  | _ ->
      let clock = Eio_mock.Clock.make () in
      let result = ref None and stuck = ref false in
      Eio.Fiber.both
        (fun () ->
          result :=
            Some
              (Eio.Time.with_timeout clock 1.0 (fun () ->
                   Ok (ignore (connect ())))))
        (fun () ->
          settle ();
          Eio_mock.Clock.advance clock;
          settle ~until:(fun () -> Option.is_some !result) ();
          (* A connect that ignores its cancellation is let through. *)
          if Option.is_none !result then begin
            stuck := true;
            ignore (Eio.Net.accept ~sw l)
          end);
      Alcotest.(check bool) "the timeout ends the wait" false !stuck;
      Alcotest.(check bool) "timed out" true (!result = Some (Error `Timeout));
      Eio.Fiber.both
        (fun () -> ignore (connect ()))
        (fun () ->
          settle ();
          ignore (Eio.Net.accept ~sw l))

let suite =
  ( "eio_net_unix",
    [
      Alcotest.test_case "the Unix accept classifier" `Quick test_accept_failure;
      Alcotest.test_case "pending network errors of an accept" `Quick
        test_accept_pending_errors;
      Alcotest.test_case "the Unix connect classifier" `Quick
        test_connect_failure;
      Alcotest.test_case "a local listener's mode, from its creation" `Quick
        test_local_listen_perm;
      Alcotest.test_case "a local listener reports its path" `Quick
        test_local_listening_addr;
      Alcotest.test_case "a message and its descriptors" `Quick
        test_local_receive;
      Alcotest.test_case "a local listener over an existing file" `Quick
        test_local_listen_existing;
      Alcotest.test_case "a local listener at a long path, and released" `Quick
        test_local_listen_long;
      Alcotest.test_case "a TCP listener on a port in use" `Quick
        test_tcp_listen_in_use;
      Alcotest.test_case "the longest path an address holds" `Quick
        test_local_max_length;
      Alcotest.test_case "a local socket by a path of any length" `Quick
        test_local_connect;
      Alcotest.test_case "a local socket that is missing or refusing" `Quick
        test_local_errors;
      Alcotest.test_case "a local connect through its network alone" `Quick
        test_local_connect_through_net_alone;
      Alcotest.test_case "a local connect confined to its directory" `Quick
        test_local_connect_at_is_confined;
      Alcotest.test_case "a local connect to a full backlog" `Quick
        test_local_connect_at_full_backlog;
      Alcotest.test_case "connected datagram batch" `Quick
        test_connected_datagram_batch;
      Alcotest.test_case "connected datagram duplex" `Quick
        test_connected_datagram_duplex;
      Alcotest.test_case "connected datagram concurrent senders" `Quick
        test_connected_datagram_concurrent_senders;
      Alcotest.test_case "connected datagram send refused" `Quick
        test_connected_datagram_send_refused;
      Alcotest.test_case "connected datagram send after the switch" `Quick
        test_connected_datagram_send_after_switch;
      Alcotest.test_case "offloaded write refused" `Quick
        test_offload_write_refused;
      Alcotest.test_case "offloaded write skips empty buffers" `Quick
        test_offload_write_skips_empty_buffers;
      Alcotest.test_case "offloaded write reads a bounded prefix" `Quick
        test_offload_write_reads_a_bounded_prefix;
      Alcotest.test_case "connected datagram error after a datagram" `Quick
        test_connected_datagram_error_after_a_datagram;
      Alcotest.test_case "connected datagram longer than max_frame" `Quick
        test_connected_datagram_oversized;
      Alcotest.test_case "connected datagram backpressure" `Quick
        test_connected_datagram_backpressure;
      Alcotest.test_case "reject excessive datagram batch" `Quick
        test_connected_datagram_rejects_excessive_batch;
    ] )
