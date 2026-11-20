let binds domain addr =
  match Unix.socket domain Unix.SOCK_DGRAM 0 with
  | exception Unix.Unix_error _ -> false
  | fd ->
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          match Unix.bind fd (Unix.ADDR_INET (addr, 0)) with
          | () -> true
          | exception Unix.Unix_error _ -> false)

let ipv4 = lazy (binds Unix.PF_INET Unix.inet_addr_loopback)
let ipv6 = lazy (binds Unix.PF_INET6 Unix.inet6_addr_loopback)
let ipv4 () = Lazy.force ipv4
let ipv6 () = Lazy.force ipv6
