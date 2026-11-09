/* Sends a forged RTM_NEWADDR from userspace to every rtnetlink socket of this
   process that joined a group. Needs CAP_NET_ADMIN; returns how many were
   sent, 0 elsewhere. */
#include <caml/mlvalues.h>

#if defined(__linux__)
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

value caml_test_netlink_spoof(value v_unit) {
  (void)v_unit;
  int sent = 0;
  int out = socket(AF_NETLINK, SOCK_RAW | SOCK_CLOEXEC, NETLINK_ROUTE);
  if (out < 0) return Val_int(0);
  for (int fd = 0; fd < 1024; fd++) {
    struct sockaddr_nl sa;
    socklen_t len = sizeof(sa);
    int proto = 0;
    socklen_t plen = sizeof(proto);
    if (fd == out) continue;
    if (getsockname(fd, (struct sockaddr *)&sa, &len) != 0 ||
        sa.nl_family != AF_NETLINK || sa.nl_groups == 0)
      continue;
    if (getsockopt(fd, SOL_SOCKET, SO_PROTOCOL, &proto, &plen) != 0 ||
        proto != NETLINK_ROUTE)
      continue;
    struct sockaddr_nl dst = {.nl_family = AF_NETLINK, .nl_pid = sa.nl_pid};
    struct {
      struct nlmsghdr h;
      struct ifaddrmsg a;
    } m;
    memset(&m, 0, sizeof m);
    m.h.nlmsg_len = sizeof m;
    m.h.nlmsg_type = RTM_NEWADDR;
    if (sendto(out, &m, sizeof m, 0, (struct sockaddr *)&dst, sizeof dst) ==
        (ssize_t)sizeof m)
      sent++;
  }
  close(out);
  return Val_int(sent);
}
#else
value caml_test_netlink_spoof(value v_unit) {
  (void)v_unit;
  return Val_int(0);
}
#endif
