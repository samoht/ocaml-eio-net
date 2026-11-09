(* The expectations of the cases this harness runs are adapted from the socket
   syscall tests of gVisor, https://github.com/google/gvisor, under
   test/syscalls/linux (Apache License 2.0). Each case names the gVisor test
   and file it comes from. *)

type platform = Linux | Macos

(* macOS has no procfs. *)
let platform = if Sys.file_exists "/proc/self" then Linux else Macos
let platform_name = function Linux -> "Linux" | Macos -> "macOS"

type backend = Mem | Real

type ctx = {
  backend : backend;
  sw : Eio.Switch.t;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  mutable next : int;
}

let run ?capacity backend f =
  match backend with
  | Mem ->
      Eio_mock.Backend.run @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      f { backend; sw; net = Eio_net_mem.v ?capacity ~sw (); next = 0 }
  | Real ->
      (* Eio_main picks this backend on macOS. On Linux it is named here: the
         would-block probe below needs a backend that tries each operation
         before it waits, which io_uring does not. *)
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      f
        {
          backend;
          sw;
          net = (env#net :> [ `Generic ] Eio.Net.ty Eio.Resource.t);
          next = 0;
        }

let fresh ctx =
  ctx.next <- ctx.next + 1;
  ctx.next

(* Outcomes are compared at the level of Eio's errors: a backend's own error
   code for a failure Eio classifies is not part of the contract. *)

let errno : Unix.error -> string = function
  | EADDRINUSE -> "EADDRINUSE"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | EAFNOSUPPORT -> "EAFNOSUPPORT"
  | EDESTADDRREQ -> "EDESTADDRREQ"
  | EHOSTUNREACH -> "EHOSTUNREACH"
  | EINVAL -> "EINVAL"
  | EISCONN -> "EISCONN"
  | EMSGSIZE -> "EMSGSIZE"
  | ENETUNREACH -> "ENETUNREACH"
  | ENOBUFS -> "ENOBUFS"
  | ENOTCONN -> "ENOTCONN"
  | ETIMEDOUT -> "ETIMEDOUT"
  | e -> Unix.error_message e

let classify = function
  | End_of_file -> "EOF"
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> "reset"
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> "refused"
  | Eio.Io (Eio.Net.E (Connection_failure _), _) -> "connect failed"
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> "ENOENT"
  | Eio.Io (Eio.Exn.X Eio_net_mem.Address_in_use, _) -> "EADDRINUSE"
  | Eio.Io (Eio.Exn.X Eio_net_mem.Message_too_long, _) -> "EMSGSIZE"
  | Eio.Io (Eio.Exn.X Eio_net_mem.No_destination, _) -> "EDESTADDRREQ"
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (e, _, _)), _)
  | Unix.Unix_error (e, _, _) ->
      errno e
  | Invalid_argument _ -> "unsupported"
  | e -> Printexc.to_string e

let outcome f = match f () with s -> s | exception e -> classify e

(* One attempt that does not wait, as MSG_DONTWAIT asks: [None] when the
   operation would wait. A real socket's backend tries the system call before
   it waits for readiness, and an in-memory one completes or waits at once, so
   the operation has had its chance once every other fiber has run. *)
let now f =
  Eio.Fiber.first
    (fun () ->
      match f () with
      | v -> Some (Ok v)
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e -> Some (Error e))
    (fun () ->
      for _ = 1 to 64 do
        Eio.Fiber.yield ()
      done;
      None)

let outcome_now f =
  match now f with
  | None -> "blocked"
  | Some (Ok s) -> s
  | Some (Error e) -> classify e

(* Deterministic payloads: a byte that differs shows a corrupted or shifted
   copy. *)
let payload ?(seed = 0) n =
  String.init n (fun i -> Char.chr ((((i + seed) * 131) + 7) land 0xff))

let pp_ip = Fmt.to_to_string Eio.Net.Ipaddr.pp
let v4_loopback = Eio.Net.Ipaddr.V4.loopback
let v6_loopback = Eio.Net.Ipaddr.V6.loopback

let v4_mapped s =
  Eio.Net.Ipaddr.of_raw ("\000\000\000\000\000\000\000\000\000\000\xff\xff" ^ s)

let v4_mapped_loopback = v4_mapped "\127\000\000\001"
let v4_mapped_any = v4_mapped "\000\000\000\000"

(* A Unix-domain path in the temporary directory, or in the current one if
   that would pass sun_path's limit of 104 bytes on macOS. *)
let unix_path ctx =
  let name = Fmt.str "gv%d-%d.sock" (Unix.getpid ()) (fresh ctx) in
  let path = Filename.concat (Filename.get_temp_dir_name ()) name in
  let path = if String.length path < 100 then path else name in
  (match ctx.backend with
  | Real -> (
      (try Unix.unlink path with Unix.Unix_error _ -> ());
      (* Eio's POSIX backend leaves a datagram socket's file behind. *)
      Eio.Switch.on_release ctx.sw @@ fun () ->
      try Unix.unlink path with Unix.Unix_error _ -> ())
  | Mem -> ());
  path

(* Stream sockets. *)

type stream_kind = Tcp4 | Tcp6 | Unix_stream

let stream_name = function
  | Tcp4 -> "tcp4"
  | Tcp6 -> "tcp6"
  | Unix_stream -> "unix-stream"

type flow = [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t
type listener = [ `Generic ] Eio.Net.listening_socket_ty Eio.Resource.t

let listen ?(backlog = 16) ?(reuse_addr = false) ctx
    (addr : Eio.Net.Sockaddr.stream) : listener =
  Eio.Net.listen ~sw:ctx.sw ~backlog ~reuse_addr ctx.net addr

let stream_addr ctx = function
  | Tcp4 -> `Tcp (v4_loopback, 0)
  | Tcp6 -> `Tcp (v6_loopback, 0)
  | Unix_stream -> `Unix (unix_path ctx)

let connect ctx addr : flow = Eio.Net.connect ~sw:ctx.sw ctx.net addr

(* A connected pair: the connecting end, the accepted end and the
   listener. *)
let stream_pair ctx kind =
  let l = listen ctx (stream_addr ctx kind) in
  let c = connect ctx (Eio.Net.listening_addr l) in
  let a, _ = Eio.Net.accept ~sw:ctx.sw l in
  (c, (a :> flow), l)

let write flow s = Eio.Flow.write flow [ Cstruct.of_string s ]

let read flow n =
  let buf = Cstruct.create n in
  let got = Eio.Flow.single_read flow buf in
  Cstruct.to_string ~len:got buf

let read_exact flow n =
  let buf = Cstruct.create n in
  Eio.Flow.read_exact flow buf;
  Cstruct.to_string buf

(* Datagram sockets. *)

type dgram_kind = Udp4 | Udp6 | Unix_dgram

let dgram_name = function
  | Udp4 -> "udp4"
  | Udp6 -> "udp6"
  | Unix_dgram -> "unix-dgram"

type dgram = [ `Generic ] Eio.Net.datagram_socket_ty Eio.Resource.t

(* The address a socket was bound at. Eio reports none for a datagram socket,
   so a real one is asked for it, and an in-memory one gets a port chosen
   here. *)
let bound_addr ctx (sock : dgram) (requested : Eio.Net.Sockaddr.datagram) =
  match (ctx.backend, Eio_unix.Resource.fd_opt sock) with
  | Real, Some fd ->
      Eio_unix.Fd.use_exn "getsockname" fd (fun fd ->
          Eio_unix.Net.sockaddr_of_unix_datagram (Unix.getsockname fd))
  | _ -> requested

let datagram_socket ctx (addr : Eio.Net.Sockaddr.datagram) =
  let addr =
    match (ctx.backend, addr) with
    | Mem, `Udp (ip, 0) -> `Udp (ip, 40000 + fresh ctx)
    | _, addr -> addr
  in
  let sock : dgram = Eio.Net.datagram_socket ~sw:ctx.sw ctx.net addr in
  (sock, bound_addr ctx sock addr)

let dgram_addr ctx : dgram_kind -> Eio.Net.Sockaddr.datagram = function
  | Udp4 -> `Udp (v4_loopback, 0)
  | Udp6 -> `Udp (v6_loopback, 0)
  | Unix_dgram -> `Unix (unix_path ctx)

let dgram_socket ctx kind = datagram_socket ctx (dgram_addr ctx kind)
let send sock ~dst s = Eio.Net.send sock ~dst [ Cstruct.of_string s ]

let recv sock n =
  let buf = Cstruct.create n in
  let _, got = Eio.Net.recv sock buf in
  Cstruct.to_string ~len:got buf

(* A pair of sockets of either type, as gVisor's socket pair fixtures give
   them: [send] on the first end and [recv] on the second. *)

type kind = Stream of stream_kind | Dgram of dgram_kind

let kind_name = function Stream k -> stream_name k | Dgram k -> dgram_name k

type pair = {
  send : string -> unit;
  recv : int -> string;
  recv_all : int -> string;
}

let pair ctx = function
  | Stream k ->
      let c, a, _ = stream_pair ctx k in
      { send = write c; recv = read a; recv_all = read_exact a }
  | Dgram k ->
      let s, _ = dgram_socket ctx k in
      let r, dst = dgram_socket ctx k in
      { send = send s ~dst; recv = recv r; recv_all = recv r }

(* IPv6 loopback is there when a socket binds ::1: a host or a container may
   run without IPv6. *)
let ipv6 =
  lazy
    (let s = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
     Fun.protect
       ~finally:(fun () -> Unix.close s)
       (fun () ->
         match Unix.bind s (Unix.ADDR_INET (Unix.inet6_addr_loopback, 0)) with
         | () -> true
         | exception Unix.Unix_error _ -> false))

let needs_ipv6 = function
  | Stream Tcp6 | Dgram Udp6 -> true
  | Stream (Tcp4 | Unix_stream) | Dgram (Udp4 | Unix_dgram) -> false

(* Expectations. Linux's is the one gVisor states, as its tests pin Linux's
   behaviour; macOS's is what its kernel does where that differs, and mem's
   what eio-net.mem does. A mem outcome that matches neither real platform is
   a defect, which [mem_bug] says how to fix. *)

type expect = {
  linux : string;
  macos : string;
  mem : string;
  mem_bug : string option;
}

let expect ?macos ?mem ?mem_bug linux =
  let macos = Option.value macos ~default:linux in
  let mem = Option.value mem ~default:linux in
  { linux; macos; mem; mem_bug }

type case = {
  file : string;
  test : string;
  variant : string;
  expect : expect;
  ipv6 : bool;
  capacity : int option;
  body : ctx -> string;
}

let case ?(variant = "") ?(ipv6 = false) ?capacity ~file ~test expect body =
  { file; test; variant; expect; ipv6; capacity; body }

let real_expected e = match platform with Linux -> e.linux | Macos -> e.macos

(* A real platform's expectation may list outcomes that depend on timing,
   separated by " | ". *)
let alternatives s = List.map String.trim (String.split_on_char '|' s)
let among s expected = List.mem s (alternatives expected)

let label c =
  let test =
    if c.variant = "" then c.test else Fmt.str "%s (%s)" c.test c.variant
  in
  Fmt.str "%s %s" c.file test

let consistent c =
  let e = c.expect in
  let matches = among e.mem e.linux || among e.mem e.macos in
  match (matches, e.mem_bug) with
  | true, None | false, Some _ -> ()
  | false, None ->
      Alcotest.failf "%s: mem %S matches neither Linux %S nor macOS %S"
        (label c) e.mem e.linux e.macos
  | true, Some _ ->
      Alcotest.failf "%s: a mem bug is noted where mem matches" (label c)

let test_cases c =
  let skip_ipv6 () = c.ipv6 && not (Lazy.force ipv6) in
  let mem () =
    consistent c;
    Alcotest.(check string)
      "eio-net.mem" c.expect.mem
      (run ?capacity:c.capacity Mem c.body)
  in
  let real () =
    if skip_ipv6 () then Alcotest.skip ()
    else
      let expected = real_expected c.expect in
      let got = run Real c.body in
      if among got expected then ()
      else Alcotest.(check string) (platform_name platform) expected got
  in
  [
    Alcotest.test_case (label c ^ " [mem]") `Quick mem;
    Alcotest.test_case (label c ^ " [real]") `Quick real;
  ]

let suite name cases = (name, List.concat_map test_cases cases)

(* Eio has no shutdown for a datagram socket's type, though each backend
   implements one. *)
let dgram_shutdown (sock : dgram) cmd =
  let (Eio.Resource.T (t, ops)) = sock in
  let module S = (val Eio.Resource.get ops Eio.Flow.Pi.Shutdown) in
  S.shutdown t cmd

(* Waits until [n] bytes are queued to be read on [flow], peeking at a real
   socket's queue: gVisor sleeps instead. An in-memory flow queues a write
   before it returns. *)
let wait_queued ctx (flow : flow) n =
  match (ctx.backend, Eio_unix.Resource.fd_opt flow) with
  | Real, Some fd ->
      let buf = Bytes.create n in
      let peek fd =
        try Unix.recv fd buf 0 n [ Unix.MSG_PEEK ]
        with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> 0
      in
      let rec loop () =
        if Eio_unix.Fd.use_exn "peek" fd peek < n then begin
          Eio.Fiber.yield ();
          loop ()
        end
      in
      loop ()
  | _ -> ()
