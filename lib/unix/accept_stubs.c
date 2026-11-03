#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>

/* The network errors a TCP accept(2) on Linux passes on from the new
 * connection (accept(2), NOTES), as Unix.error values: OCaml's Unix names
 * EPROTO and ENONET only as EUNKNOWNERR of the system's own code. ENONET is
 * Linux's alone. */
CAMLprim value caml_eio_net_pending_accept_errors(value v_unit)
{
    CAMLparam1(v_unit);
    CAMLlocal3(list, err, cell);
    static const int codes[] = {
        ENETDOWN, EPROTO,       ENOPROTOOPT, EHOSTDOWN,
#ifdef ENONET
        ENONET,
#endif
        EHOSTUNREACH, ENETUNREACH,
    };
    (void)v_unit;
    list = Val_emptylist;
    for (int i = (int)(sizeof(codes) / sizeof(codes[0])) - 1; i >= 0; i--)
    {
        err = caml_unix_error_of_code(codes[i]);
        cell = caml_alloc_small(2, Tag_cons);
        Field(cell, 0) = err;
        Field(cell, 1) = list;
        list = cell;
    }
    CAMLreturn(list);
}
