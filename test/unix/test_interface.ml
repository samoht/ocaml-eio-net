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

let test_interface_addresses () =
  let addresses = Eio_net_unix.Interface.addresses () in
  let listed prefix =
    List.exists
      (fun (a : Eio_net_unix.Interface.address) ->
        a.loopback && Ipaddr.Prefix.to_string a.prefix = prefix)
      addresses
  in
  Alcotest.(check bool)
    "a loopback interface carries 127.0.0.1/8" true (listed "127.0.0.1/8");
  Alcotest.(check bool)
    "a loopback interface carries ::1/128" true (listed "::1/128");
  Alcotest.(check bool)
    "no host carries 192.0.2.1, so it does not bind" false
    (binds (Ipaddr.V4.of_string_exn "192.0.2.1"));
  List.iter
    (fun (a : Eio_net_unix.Interface.address) ->
      match Ipaddr.Prefix.address a.prefix with
      | Ipaddr.V4 addr ->
          Alcotest.(check bool)
            (Fmt.str "%s's %a binds" a.interface Ipaddr.V4.pp addr)
            true (binds addr)
      | Ipaddr.V6 _ -> ())
    addresses

let suite =
  ( "interface",
    [
      Alcotest.test_case "the host's interface addresses" `Quick
        test_interface_addresses;
    ] )
