external writev : Unix.file_descr -> (Cstruct.buffer * int * int) array -> int
  = "caml_eio_net_writev"

let iov_max = 256

let offload_single_write fd bufs =
  let bufs = List.filteri (fun i _ -> i < iov_max) bufs in
  let iov =
    Array.of_list
      (List.map
         (fun c -> (c.Cstruct.buffer, c.Cstruct.off, c.Cstruct.len))
         bufs)
  in
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
  attempt ()

let offload_writer flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> None
  | Some fd -> Some (offload_single_write fd)
