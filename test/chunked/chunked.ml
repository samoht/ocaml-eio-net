type t = string list ref

let single_read t dst =
  match !t with
  | [] -> raise End_of_file
  | chunk :: rest ->
      let length = min (Cstruct.length dst) (String.length chunk) in
      Cstruct.blit_from_string chunk 0 dst 0 length;
      t :=
        if length = String.length chunk then rest
        else String.sub chunk length (String.length chunk - length) :: rest;
      length

let handler =
  Eio.Flow.Pi.source
    (module struct
      type nonrec t = t

      let read_methods = []
      let single_read = single_read
    end)

let split sizes s =
  let rec go offset acc = function
    | [] -> go offset acc sizes
    | size :: rest ->
        if offset >= String.length s then List.rev acc
        else
          let length = min size (String.length s - offset) in
          go (offset + length) (String.sub s offset length :: acc) rest
  in
  go 0 [] sizes

let reader config chunks =
  Eio.Buf_read.of_flow
    ~max_size:(Eio_net_framing.Config.buffer_size config)
    (Eio.Resource.T (ref (List.filter (( <> ) "") chunks), handler))
