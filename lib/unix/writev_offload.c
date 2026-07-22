#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <sys/uio.h>

#define EIO_NET_IOV_MAX 256

/* Vectored write of cstruct (buffer, offset, length) triples with the
 * runtime lock released, so a systhread caller leaves its domain free to
 * run fibers while the kernel copies the batch into the socket buffer.
 * Returns the byte count, or -1 when the socket would block. Bigarray data
 * never moves, so the pointers stay valid across the blocking section. */
CAMLprim value caml_eio_net_writev(value v_fd, value v_bufs)
{
    CAMLparam2(v_fd, v_bufs);
    struct iovec iov[EIO_NET_IOV_MAX];
    int count = (int)Wosize_val(v_bufs);
    if (count <= 0 || count > EIO_NET_IOV_MAX)
        caml_invalid_argument("caml_eio_net_writev: iovec count");
    for (int i = 0; i < count; i++) {
        value triple = Field(v_bufs, i);
        iov[i].iov_base = (char *)Caml_ba_data_val(Field(triple, 0))
                          + Long_val(Field(triple, 1));
        iov[i].iov_len = (size_t)Long_val(Field(triple, 2));
    }
    int fd = Int_val(v_fd);
    caml_release_runtime_system();
    ssize_t wrote = writev(fd, iov, count);
    int saved_errno = errno;
    caml_acquire_runtime_system();
    if (wrote >= 0)
        CAMLreturn(Val_long(wrote));
    if (saved_errno == EAGAIN || saved_errno == EWOULDBLOCK)
        CAMLreturn(Val_long(-1));
    caml_unix_error(saved_errno, "writev", Nothing);
}
