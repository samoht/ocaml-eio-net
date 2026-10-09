#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

/* The size of a socket address's path, its terminating NUL included. */
CAMLprim value caml_eio_net_sun_path_size(value v_unit)
{
    (void)v_unit;
    struct sockaddr_un sa;
    return Val_int(sizeof(sa.sun_path));
}

#if defined(__APPLE__)
/* The calling thread's own working directory, which -1 returns to the
 * process's (libsystem_pthread, exported since macOS 10.12 and not declared
 * by the SDK headers). */
extern int pthread_fchdir_np(int fd);
#endif

/* connect(2) the stream socket [v_sock] to the socket named [v_name] in the
 * directory open as [v_dir], without changing the working directory of the
 * process: on macOS from that directory made the working directory of this
 * thread alone, as Git changes directory in unix-socket.c; connectat(2) on
 * FreeBSD; the name under the short link /proc/self/fd/<dir> on Linux. The
 * name is copied into the address before the runtime lock is released. */
CAMLprim value caml_eio_net_connect_at(value v_dir, value v_sock, value v_name)
{
    CAMLparam3(v_dir, v_sock, v_name);
    struct sockaddr_un sa;
    int dir = Int_val(v_dir), sock = Int_val(v_sock), r;
    memset(&sa, 0, sizeof(sa));
    sa.sun_family = AF_UNIX;
#if defined(__APPLE__) || defined(__FreeBSD__)
    size_t len = caml_string_length(v_name);
    if (len >= sizeof(sa.sun_path))
        caml_unix_error(ENAMETOOLONG, "connect", v_name);
    memcpy(sa.sun_path, String_val(v_name), len + 1);
    caml_enter_blocking_section();
#if defined(__APPLE__)
    r = pthread_fchdir_np(dir);
    if (r == 0)
    {
        r = connect(sock, (struct sockaddr *)&sa, sizeof(sa));
        int e = errno;
        /* A pool thread left in another directory would resolve every
         * later relative path wrongly: that is no state to go on from. */
        if (pthread_fchdir_np(-1) != 0)
            abort();
        errno = e;
    }
#else
    r = connectat(dir, sock, (struct sockaddr *)&sa, sizeof(sa));
#endif
    caml_leave_blocking_section();
    if (r < 0)
        caml_uerror("connect", v_name);
#elif defined(__linux__)
    int n = snprintf(sa.sun_path, sizeof(sa.sun_path), "/proc/self/fd/%d/%s",
                     dir, String_val(v_name));
    if (n < 0 || (size_t)n >= sizeof(sa.sun_path))
        caml_unix_error(ENAMETOOLONG, "connect", v_name);
    caml_enter_blocking_section();
    r = connect(sock, (struct sockaddr *)&sa, sizeof(sa));
    caml_leave_blocking_section();
    if (r < 0)
        caml_uerror("connect", v_name);
#else
    (void)dir;
    (void)sock;
    (void)r;
    caml_unix_error(ENOSYS, "connectat", v_name);
#endif
    CAMLreturn(Val_unit);
}
