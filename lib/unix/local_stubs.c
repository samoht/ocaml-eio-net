#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
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

/* A name of [v] the runtime may move or collect, copied so the runtime lock
 * can be released; a name with a NUL byte names no file. */
static char *name_of(value v, const char *fn)
{
    if (!caml_string_is_c_safe(v))
        caml_unix_error(EINVAL, (char *)fn, v);
    return caml_stat_strdup(String_val(v));
}

/* Raise the error of [fn] on [v_name], freeing [name] first. */
static void fail(const char *fn, value v_name, char *name, int e)
{
    caml_stat_free(name);
    caml_unix_error(e, (char *)fn, v_name);
}

enum op { OP_CONNECT, OP_BIND };

/* connect(2) or bind(2) the socket [sock] to the entry [name] of the
 * directory open as [dir], without changing the working directory of the
 * process: on macOS from that directory made the working directory of this
 * thread alone, as Git changes directory in unix-socket.c; connectat(2) and
 * bindat(2) on FreeBSD; the name under the short link /proc/self/fd/<dir> on
 * Linux. Runs without the runtime lock; -1 and errno on failure. */
static int sock_at(int dir, int sock, const char *name, enum op op)
{
    struct sockaddr_un sa;
    memset(&sa, 0, sizeof(sa));
    sa.sun_family = AF_UNIX;
#if defined(__APPLE__) || defined(__FreeBSD__)
    size_t len = strlen(name);
    if (len >= sizeof(sa.sun_path))
    {
        errno = ENAMETOOLONG;
        return -1;
    }
    memcpy(sa.sun_path, name, len + 1);
#if defined(__APPLE__)
    if (pthread_fchdir_np(dir) != 0)
        return -1;
    int r = op == OP_BIND ? bind(sock, (struct sockaddr *)&sa, sizeof(sa))
                          : connect(sock, (struct sockaddr *)&sa, sizeof(sa));
    int e = errno;
    /* A thread left in another directory would resolve every later relative
     * path wrongly: that is no state to go on from. */
    if (pthread_fchdir_np(-1) != 0)
        abort();
    errno = e;
    return r;
#else
    return op == OP_BIND
               ? bindat(dir, sock, (struct sockaddr *)&sa, sizeof(sa))
               : connectat(dir, sock, (struct sockaddr *)&sa, sizeof(sa));
#endif
#elif defined(__linux__)
    int n = snprintf(sa.sun_path, sizeof(sa.sun_path), "/proc/self/fd/%d/%s",
                     dir, name);
    if (n < 0 || (size_t)n >= sizeof(sa.sun_path))
    {
        errno = ENAMETOOLONG;
        return -1;
    }
    return op == OP_BIND ? bind(sock, (struct sockaddr *)&sa, sizeof(sa))
                         : connect(sock, (struct sockaddr *)&sa, sizeof(sa));
#else
    (void)dir;
    (void)sock;
    (void)name;
    (void)op;
    errno = ENOSYS;
    return -1;
#endif
}

/* Connect the stream socket [v_sock] to the socket named [v_name] in the
 * directory open as [v_dir]. A name that is a symbolic link is refused with
 * ELOOP, as open(2) refuses one with O_NOFOLLOW: its target is not under the
 * directory's authority. */
CAMLprim value caml_eio_net_connect_at(value v_dir, value v_sock, value v_name)
{
    CAMLparam3(v_dir, v_sock, v_name);
    int dir = Int_val(v_dir), sock = Int_val(v_sock), r, e = 0;
    char *name = name_of(v_name, "connect");
    struct stat st;
    caml_enter_blocking_section();
    r = fstatat(dir, name, &st, AT_SYMLINK_NOFOLLOW);
    if (r == 0 && S_ISLNK(st.st_mode))
    {
        r = -1;
        errno = ELOOP;
    }
    if (r == 0)
        r = sock_at(dir, sock, name, OP_CONNECT);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    if (r < 0)
        fail("connect", v_name, name, e);
    caml_stat_free(name);
    CAMLreturn(Val_unit);
}

/* Bind [v_sock] to the new entry [v_name] of the directory open as [v_dir]. */
CAMLprim value caml_eio_net_bind_at(value v_dir, value v_sock, value v_name)
{
    CAMLparam3(v_dir, v_sock, v_name);
    int dir = Int_val(v_dir), sock = Int_val(v_sock), r, e = 0;
    char *name = name_of(v_name, "bind");
    caml_enter_blocking_section();
    r = sock_at(dir, sock, name, OP_BIND);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    if (r < 0)
        fail("bind", v_name, name, e);
    caml_stat_free(name);
    CAMLreturn(Val_unit);
}

/* Make the directory [v_name] in the directory open as [v_dir] and open it: a
 * directory of this user's that no other can write, or EEXIST when the name
 * is taken. Opened without following a link and checked after the open, so a
 * directory another put in its place is refused (EPERM) rather than used. */
CAMLprim value caml_eio_net_private_dir(value v_dir, value v_name)
{
    CAMLparam2(v_dir, v_name);
    int dir = Int_val(v_dir), fd = -1, e = 0;
    char *name = name_of(v_name, "mkdirat");
    const char *fn = "mkdirat";
    struct stat st;
    caml_enter_blocking_section();
    if (mkdirat(dir, name, 0700) != 0)
        e = errno;
    else
    {
        fn = "openat";
        fd = openat(dir, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (fd < 0)
            e = errno;
        else if (fstat(fd, &st) != 0)
            e = errno;
        else if (!S_ISDIR(st.st_mode) || st.st_uid != geteuid() ||
                 (st.st_mode & 077) != 0)
            e = EPERM;
        if (e != 0)
        {
            if (fd >= 0)
                close(fd);
            unlinkat(dir, name, AT_REMOVEDIR);
        }
    }
    caml_leave_blocking_section();
    if (e != 0)
        fail(fn, v_name, name, e);
    caml_stat_free(name);
    CAMLreturn(Val_int(fd));
}

/* Set the mode of the entry [v_name] of the directory open as [v_dir]. */
CAMLprim value caml_eio_net_chmod_at(value v_dir, value v_name, value v_perm)
{
    CAMLparam3(v_dir, v_name, v_perm);
    int dir = Int_val(v_dir), r, e = 0;
    char *name = name_of(v_name, "fchmodat");
    caml_enter_blocking_section();
    r = fchmodat(dir, name, (mode_t)Int_val(v_perm), 0);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    if (r < 0)
        fail("fchmodat", v_name, name, e);
    caml_stat_free(name);
    CAMLreturn(Val_unit);
}

/* Whether the entry [v_name] of the directory open as [v_dir] is a socket,
 * and its device and inode, the entry itself if it is a link. */
CAMLprim value caml_eio_net_id_at(value v_dir, value v_name)
{
    CAMLparam2(v_dir, v_name);
    CAMLlocal3(v_id, v_dev, v_ino);
    int dir = Int_val(v_dir), r, e = 0;
    char *name = name_of(v_name, "fstatat");
    struct stat st;
    caml_enter_blocking_section();
    r = fstatat(dir, name, &st, AT_SYMLINK_NOFOLLOW);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    if (r < 0)
        fail("fstatat", v_name, name, e);
    caml_stat_free(name);
    v_dev = caml_copy_int64((int64_t)st.st_dev);
    v_ino = caml_copy_int64((int64_t)st.st_ino);
    v_id = caml_alloc_tuple(3);
    Store_field(v_id, 0, Val_bool(S_ISSOCK(st.st_mode)));
    Store_field(v_id, 1, v_dev);
    Store_field(v_id, 2, v_ino);
    CAMLreturn(v_id);
}

/* linkat(2) or renameat(2) the entry [v_old] of [v_odir] to [v_new] of
 * [v_ndir]. */
static value move_at(value v_odir, value v_old, value v_ndir, value v_new,
                     int link)
{
    CAMLparam4(v_odir, v_old, v_ndir, v_new);
    int odir = Int_val(v_odir), ndir = Int_val(v_ndir), r, e = 0;
    const char *fn = link ? "linkat" : "renameat";
    char *old = name_of(v_old, fn);
    char *new = name_of(v_new, fn);
    caml_enter_blocking_section();
    r = link ? linkat(odir, old, ndir, new, 0) : renameat(odir, old, ndir, new);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    caml_stat_free(old);
    if (r < 0)
        fail(fn, v_new, new, e);
    caml_stat_free(new);
    CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_net_link_at(value v_odir, value v_old, value v_ndir,
                                    value v_new)
{
    return move_at(v_odir, v_old, v_ndir, v_new, 1);
}

CAMLprim value caml_eio_net_rename_at(value v_odir, value v_old, value v_ndir,
                                      value v_new)
{
    return move_at(v_odir, v_old, v_ndir, v_new, 0);
}

/* Remove the entry [v_name] of the directory open as [v_dir], an empty
 * directory when [v_is_dir]. */
CAMLprim value caml_eio_net_unlink_at(value v_dir, value v_name,
                                      value v_is_dir)
{
    CAMLparam3(v_dir, v_name, v_is_dir);
    int dir = Int_val(v_dir), r, e = 0;
    int flags = Bool_val(v_is_dir) ? AT_REMOVEDIR : 0;
    char *name = name_of(v_name, "unlinkat");
    caml_enter_blocking_section();
    r = unlinkat(dir, name, flags);
    if (r < 0)
        e = errno;
    caml_leave_blocking_section();
    if (r < 0)
        fail("unlinkat", v_name, name, e);
    caml_stat_free(name);
    CAMLreturn(Val_unit);
}
