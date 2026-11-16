/* Makes a connected TCP socket drop every segment it sends, so that its peer
   hears nothing more. On Linux it sends through a loose source route by
   192.0.2.1, an address no host has: a host that does not accept source
   routes, as Linux does by default, drops what reaches it, and a route
   there fails. This needs CAP_NET_RAW. On macOS it sends through stf0, an
   interface that is down. Raises Failure on any other system. */
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <netinet/in.h>
#include <sys/socket.h>

#if defined(__linux__)

value caml_measure_silence(value v_fd) {
  CAMLparam1(v_fd);
  /* IPOPT_LSRR, its length, its pointer, the hop, then IPOPT_END. */
  unsigned char lsrr[8] = {0x83, 7, 4, 192, 0, 2, 1, 0};
  if (setsockopt(Int_val(v_fd), IPPROTO_IP, IP_OPTIONS, lsrr, sizeof(lsrr)) !=
      0)
    caml_uerror("setsockopt IP_OPTIONS", Nothing);
  CAMLreturn(Val_unit);
}

#elif defined(__APPLE__)
#include <net/if.h>

value caml_measure_silence(value v_fd) {
  CAMLparam1(v_fd);
  unsigned int index = if_nametoindex("stf0");
  if (index == 0) caml_failwith("silence: no stf0 interface");
  if (setsockopt(Int_val(v_fd), IPPROTO_IP, IP_BOUND_IF, &index,
                 sizeof(index)) != 0)
    caml_uerror("setsockopt IP_BOUND_IF", Nothing);
  CAMLreturn(Val_unit);
}

#else

value caml_measure_silence(value v_fd) {
  CAMLparam1(v_fd);
  caml_failwith("silence: Linux and macOS only");
  CAMLreturn(Val_unit);
}

#endif
