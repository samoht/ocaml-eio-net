external writev : Unix.file_descr -> (Cstruct.buffer * int * int) array -> int
  = "caml_eio_net_writev"

let iov_max = 256

(* The iovecs of the first [iov_max] non-empty buffers of [bufs], in one pass
   that reads no further. *)
let iovecs bufs =
  let rec take n acc = function
    | _ when n = iov_max -> acc
    | [] -> acc
    | { Cstruct.len = 0; _ } :: rest -> take n acc rest
    | { Cstruct.buffer; off; len } :: rest ->
        take (n + 1) ((buffer, off, len) :: acc) rest
  in
  Array.of_list (List.rev (take 0 [] bufs))

let offload_single_write fd bufs =
  let iov = iovecs bufs in
  let rec attempt () =
    let wrote =
      try
        Eio_unix.Fd.use_exn "Eio_net_unix.Stream.offload_single_write" fd
          (fun unix_fd ->
            Eio_unix.run_in_systhread ~label:"eio-net-writev" (fun () ->
                writev unix_fd iov))
      with Unix.Unix_error (code, name, arg) ->
        raise (Eio_unix.Err.v code name arg)
    in
    if wrote >= 0 then wrote
    else begin
      Eio_unix.Fd.use_exn "Eio_net_unix.Stream.offload_single_write" fd
        Eio_unix.await_writable;
      attempt ()
    end
  in
  if Array.length iov = 0 then 0 else attempt ()

let offload_writer flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> None
  | Some fd -> Some (offload_single_write fd)
