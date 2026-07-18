#include <errno.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>

#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifdef __APPLE__
struct ocaml_msghdr_x {
  void *msg_name;
  socklen_t msg_namelen;
  struct iovec *msg_iov;
  int msg_iovlen;
  void *msg_control;
  socklen_t msg_controllen;
  int msg_flags;
  size_t msg_datalen;
};

extern ssize_t sendmsg_x(int, const struct ocaml_msghdr_x *, unsigned int, int)
    __attribute__((weak_import));
extern ssize_t recvmsg_x(int, struct ocaml_msghdr_x *, unsigned int, int)
    __attribute__((weak_import));
#endif

CAMLprim value caml_eio_net_sendmsg_batch(value vfd, value vbuffers,
                                           value vlengths, value vcount) {
  CAMLparam4(vfd, vbuffers, vlengths, vcount);
  int fd = Int_val(vfd);
  int count = Int_val(vcount);
  if (count <= 0) CAMLreturn(Val_int(0));

  struct iovec iov[count];
  for (int i = 0; i < count; i++) {
    value cstruct = Field(vbuffers, i);
    iov[i].iov_base =
        (char *)Caml_ba_data_val(Field(cstruct, 0)) + Long_val(Field(cstruct, 1));
    iov[i].iov_len = Long_val(Field(vlengths, i));
  }

  ssize_t sent = 0;
  caml_enter_blocking_section();
#ifdef __APPLE__
  if (sendmsg_x != NULL) {
    struct ocaml_msghdr_x messages[count];
    for (int i = 0; i < count; i++) {
      messages[i] = (struct ocaml_msghdr_x){0};
      messages[i].msg_iov = &iov[i];
      messages[i].msg_iovlen = 1;
    }
    sent = sendmsg_x(fd, messages, (unsigned int)count, 0);
  } else
#endif
  {
    while (sent < count) {
      ssize_t len = send(fd, iov[sent].iov_base, iov[sent].iov_len, 0);
      if (len < 0) break;
      sent++;
    }
  }
  int error = errno;
  caml_leave_blocking_section();

  if (sent < 0) {
    if (error == EAGAIN || error == EWOULDBLOCK || error == ENOBUFS)
      sent = 0;
    else {
      errno = error;
      uerror("sendmsg_batch", Nothing);
    }
  }
  CAMLreturn(Val_int(sent));
}

CAMLprim value caml_eio_net_recvmsg_batch(value vfd, value vbuffers,
                                           value vlengths, value voffset,
                                           value vcount) {
  CAMLparam5(vfd, vbuffers, vlengths, voffset, vcount);
  int fd = Int_val(vfd);
  int offset = Int_val(voffset);
  int count = Int_val(vcount);
  if (offset >= count) CAMLreturn(Val_int(0));

  struct iovec iov[count];
  size_t lengths[count];
  for (int i = offset; i < count; i++) {
    value cstruct = Field(vbuffers, i);
    iov[i].iov_base =
        (char *)Caml_ba_data_val(Field(cstruct, 0)) + Long_val(Field(cstruct, 1));
    iov[i].iov_len = Long_val(Field(cstruct, 2));
    lengths[i] = 0;
  }

  ssize_t received = 0;
  caml_enter_blocking_section();
#ifdef __APPLE__
  if (recvmsg_x != NULL) {
    struct ocaml_msghdr_x messages[count - offset];
    for (int i = offset; i < count; i++) {
      messages[i - offset] = (struct ocaml_msghdr_x){0};
      messages[i - offset].msg_iov = &iov[i];
      messages[i - offset].msg_iovlen = 1;
    }
    received =
        recvmsg_x(fd, messages, (unsigned int)(count - offset), MSG_DONTWAIT);
    if (received > 0) {
      for (int i = 0; i < received; i++)
        lengths[offset + i] = messages[i].msg_datalen;
    }
  } else
#endif
  {
    while (received < count - offset) {
      ssize_t len = recv(fd, iov[offset + received].iov_base,
                         iov[offset + received].iov_len, MSG_DONTWAIT);
      if (len < 0) break;
      lengths[offset + received] = (size_t)len;
      received++;
    }
  }
  int error = errno;
  caml_leave_blocking_section();

  if (received < 0) {
    if (error == EAGAIN || error == EWOULDBLOCK) received = 0;
    else {
      errno = error;
      uerror("recvmsg_batch", Nothing);
    }
  }
  for (int i = 0; i < received; i++)
    Store_field(vlengths, offset + i, Val_long(lengths[offset + i]));
  CAMLreturn(Val_int(received));
}
