#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <fcntl.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <stddef.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#if defined(__linux__)
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#else
#include <net/route.h>
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) ||     \
    defined(__NetBSD__) || defined(__DragonFly__)
#define HAVE_SA_LEN 1
#endif

/* The address bytes of [sa], read as family [family], at [*bytes], and how
 * many of them [sa] holds. A BSD netmask is cut after its last non-zero byte,
 * its sa_len saying where, and its sa_family may be left unset. */
static size_t address_bytes(const struct sockaddr *sa, int family,
                            const unsigned char **bytes)
{
    size_t off, n;
    if (family == AF_INET)
    {
        off = offsetof(struct sockaddr_in, sin_addr);
        n = 4;
    }
    else
    {
        off = offsetof(struct sockaddr_in6, sin6_addr);
        n = 16;
    }
    *bytes = (const unsigned char *)sa + off;
#ifdef HAVE_SA_LEN
    if (sa->sa_len <= off)
        return 0;
    if ((size_t)sa->sa_len - off < n)
        return (size_t)sa->sa_len - off;
#endif
    return n;
}

/* The number of leading one bits of [mask]; a missing mask is a host's. */
static int prefix_length(const struct sockaddr *mask, int family)
{
    const unsigned char *bytes;
    size_t i, n;
    int bits = 0;
    if (mask == NULL)
        return family == AF_INET ? 32 : 128;
    n = address_bytes(mask, family, &bytes);
    for (i = 0; i < n && bytes[i] == 0xff; i++)
        bits += 8;
    if (i < n)
    {
        unsigned char b = bytes[i];
        while (b & 0x80)
        {
            bits++;
            b = (unsigned char)(b << 1);
        }
    }
    return bits;
}

/* Every IPv4 and IPv6 address of the host's interfaces, as a list of
 * (name, address octets, prefix length, loopback), last interface first. A
 * KAME stack embeds a link-local address's scope in its second 16-bit word,
 * which fe80::/10 holds as zero: it is cleared. */
CAMLprim value caml_eio_net_interface_addresses(value v_unit)
{
    CAMLparam1(v_unit);
    CAMLlocal4(list, cell, entry, field);
    struct ifaddrs *ifs, *ifa;
    (void)v_unit;
    if (getifaddrs(&ifs) == -1)
        caml_uerror("getifaddrs", Nothing);
    list = Val_emptylist;
    for (ifa = ifs; ifa != NULL; ifa = ifa->ifa_next)
    {
        const unsigned char *bytes;
        unsigned char octets[16];
        size_t n;
        int family;
        if (ifa->ifa_addr == NULL)
            continue;
        family = ifa->ifa_addr->sa_family;
        if (family != AF_INET && family != AF_INET6)
            continue;
        n = family == AF_INET ? 4 : 16;
        if (address_bytes(ifa->ifa_addr, family, &bytes) < n)
            continue;
        memcpy(octets, bytes, n);
        if (family == AF_INET6 && octets[0] == 0xfe &&
            (octets[1] & 0xc0) == 0x80)
        {
            octets[2] = 0;
            octets[3] = 0;
        }
        entry = caml_alloc_tuple(4);
        field = caml_copy_string(ifa->ifa_name);
        Store_field(entry, 0, field);
        field = caml_alloc_initialized_string(n, (const char *)octets);
        Store_field(entry, 1, field);
        Store_field(entry, 2,
                    Val_int(prefix_length(ifa->ifa_netmask, family)));
        Store_field(entry, 3, Val_bool(ifa->ifa_flags & IFF_LOOPBACK));
        cell = caml_alloc(2, 0);
        Store_field(cell, 0, entry);
        Store_field(cell, 1, list);
        list = cell;
    }
    freeifaddrs(ifs);
    CAMLreturn(list);
}

/* A non-blocking socket the kernel writes a message to when an interface
 * address is added or removed: rtnetlink's address groups on Linux, a routing
 * socket elsewhere, which carries every routing message. */
CAMLprim value caml_eio_net_interface_watch(value v_unit)
{
    int fd;
    (void)v_unit;
#if defined(__linux__)
    struct sockaddr_nl sa;
    fd = socket(AF_NETLINK, SOCK_RAW | SOCK_CLOEXEC | SOCK_NONBLOCK,
                NETLINK_ROUTE);
    if (fd == -1)
        caml_uerror("socket", Nothing);
    memset(&sa, 0, sizeof(sa));
    sa.nl_family = AF_NETLINK;
    sa.nl_groups = RTMGRP_IPV4_IFADDR | RTMGRP_IPV6_IFADDR;
    if (bind(fd, (struct sockaddr *)&sa, sizeof(sa)) == -1)
    {
        int e = errno;
        close(fd);
        caml_unix_error(e, "bind", Nothing);
    }
#else
    fd = socket(PF_ROUTE, SOCK_RAW, AF_UNSPEC);
    if (fd == -1)
        caml_uerror("socket", Nothing);
    if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1 ||
        fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) == -1)
    {
        int e = errno;
        close(fd);
        caml_unix_error(e, "fcntl", Nothing);
    }
#endif
    return Val_int(fd);
}

/* Read every message queued on the watch socket [v_fd]; true when one reports
 * an address added or removed, or when the kernel dropped messages
 * (ENOBUFS), which may have. On Linux a datagram counts only from the kernel
 * (port 0): a process with CAP_NET_ADMIN can send one to the watch's port. A
 * message is read whole into a buffer that only its header needs: the rest
 * of one longer than the buffer is discarded. */
CAMLprim value caml_eio_net_interface_drain(value v_fd)
{
    int fd = Int_val(v_fd), changed = 0;
    long buffer[512];
    for (;;)
    {
#if defined(__linux__)
        struct sockaddr_nl from;
        socklen_t from_len = sizeof(from);
        memset(&from, 0, sizeof(from));
        ssize_t n = recvfrom(fd, buffer, sizeof(buffer), 0,
                             (struct sockaddr *)&from, &from_len);
#else
        ssize_t n = recv(fd, buffer, sizeof(buffer), 0);
#endif
        if (n == -1)
        {
            if (errno == EAGAIN || errno == EWOULDBLOCK)
                break;
            if (errno == EINTR)
                continue;
            if (errno == ENOBUFS)
            {
                changed = 1;
                continue;
            }
            caml_uerror("recv", Nothing);
        }
#if defined(__linux__)
        if (n > 0 && from_len >= sizeof(from) && from.nl_family == AF_NETLINK &&
            from.nl_pid == 0)
        {
            int len = (int)n;
            for (const struct nlmsghdr *h = (const struct nlmsghdr *)buffer;
                 NLMSG_OK(h, len); h = NLMSG_NEXT(h, len))
                if (h->nlmsg_type == RTM_NEWADDR ||
                    h->nlmsg_type == RTM_DELADDR)
                    changed = 1;
        }
#else
        if ((size_t)n >= offsetof(struct rt_msghdr, rtm_type) + 1)
        {
            switch (((const struct rt_msghdr *)(const void *)buffer)->rtm_type)
            {
            case RTM_NEWADDR:
            case RTM_DELADDR:
#ifdef RTM_IFANNOUNCE
            case RTM_IFANNOUNCE:
#endif
                changed = 1;
                break;
            default:
                break;
            }
        }
#endif
        if (n == 0)
            break;
    }
    return Val_bool(changed);
}
