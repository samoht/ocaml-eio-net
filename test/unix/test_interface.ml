(* getifaddrs(3) as the kernel reports it. The loopback interface's IPv4
   netmask, 255.0.0.0, is the case a BSD stack cuts after its first byte: a
   reader that takes its length as four bytes, or ignores the cut, misreads the
   prefix. bind(2) is the independent instrument for the rest: the kernel binds
   a socket to an address of its own interfaces and to no other
   (EADDRNOTAVAIL), shown on an address no host carries. *)
let binds addr =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      match
        Unix.bind fd
          (Unix.ADDR_INET
             (Unix.inet_addr_of_string (Ipaddr.V4.to_string addr), 0))
      with
      | () -> true
      | exception Unix.Unix_error (EADDRNOTAVAIL, _, _) -> false)

(* IPv6 loopback is there when a socket binds ::1: a host or a container may
   run without IPv6. *)
let ipv6_loopback () =
  match Unix.socket Unix.PF_INET6 Unix.SOCK_DGRAM 0 with
  | exception Unix.Unix_error _ -> false
  | fd ->
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          match Unix.bind fd (Unix.ADDR_INET (Unix.inet6_addr_loopback, 0)) with
          | () -> true
          | exception Unix.Unix_error _ -> false)

module Address = Eio_net_unix.Interface.Address

let test_interface_addresses () =
  let addresses = Eio_net_unix.Interface.addresses () in
  let listed prefix =
    List.exists
      (fun a ->
        Address.is_loopback a
        && Ipaddr.Prefix.to_string (Address.prefix a) = prefix)
      addresses
  in
  Alcotest.(check bool)
    "a loopback interface carries 127.0.0.1/8" true (listed "127.0.0.1/8");
  if ipv6_loopback () then
    Alcotest.(check bool)
      "a loopback interface carries ::1/128" true (listed "::1/128");
  Alcotest.(check bool)
    "no host carries 192.0.2.1, so it does not bind" false
    (binds (Ipaddr.V4.of_string_exn "192.0.2.1"));
  List.iter
    (fun a ->
      match Ipaddr.Prefix.address (Address.prefix a) with
      | Ipaddr.V4 addr ->
          Alcotest.(check bool)
            (Fmt.str "%s's %a binds" (Address.interface a) Ipaddr.V4.pp addr)
            true (binds addr)
      | Ipaddr.V6 _ -> ())
    addresses

external netlink_spoof : unit -> int = "caml_test_netlink_spoof"

(* A message a user process sends to the watch's port is no address change:
   only the kernel's count. Forging one needs CAP_NET_ADMIN on Linux, so the
   test runs only where the forgery is sent. The forged message is queued
   when the send returns, and the yields let the scheduler poll for it. *)
let test_watch_ignores_user_messages () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let w = Eio_net_unix.Interface.watch ~sw in
  if netlink_spoof () > 0 then
    match
      Eio.Fiber.first
        (fun () ->
          Eio_net_unix.Interface.changed w;
          `Changed)
        (fun () ->
          for _ = 1 to 1000 do
            Eio.Fiber.yield ()
          done;
          `Quiet)
    with
    | `Quiet -> ()
    | `Changed -> Alcotest.fail "a forged message woke the watch"

(* A stand-in builds the addresses it lists. *)
let test_address () =
  let prefix = Ipaddr.Prefix.of_string_exn "192.0.2.7/24" in
  let a = Address.v ~interface:"eth0" ~prefix ~loopback:false in
  Alcotest.(check string) "its interface" "eth0" (Address.interface a);
  Alcotest.(check bool)
    "its prefix" true
    (Ipaddr.Prefix.compare prefix (Address.prefix a) = 0);
  Alcotest.(check bool) "not a loopback" false (Address.is_loopback a);
  Alcotest.(check bool) "equal to itself" true (Address.equal a a);
  Alcotest.(check string)
    "printed" "eth0 192.0.2.7/24"
    (Fmt.str "%a" Address.pp a)

let suite =
  ( "interface",
    [
      Alcotest.test_case "the host's interface addresses" `Quick
        test_interface_addresses;
      Alcotest.test_case "the watch ignores user messages" `Quick
        test_watch_ignores_user_messages;
      Alcotest.test_case "an address" `Quick test_address;
    ] )
