type answer = [ `Aborted | `Descriptors ]

let portable = function
  | Eio.Io (Eio.Net.E (Connection_reset _), _) -> Some `Aborted
  | _ -> None

let registered = ref None

let classify exn =
  match !registered with Some (_, f) -> f exn | None -> portable exn

let register ~name f =
  match !registered with
  | Some (held, _) ->
      Fmt.invalid_arg "Eio_net_accept.register: %s is registered, %s refused"
        held name
  | None -> registered := Some (name, f)
