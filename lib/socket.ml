open! Hermetic

type answer = [ `Descriptors | `Memory | `Unsupported | `Denied ]

let classify = function
  | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> Some `Denied
  | e -> Errno.socket e
