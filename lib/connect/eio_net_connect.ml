open! Hermetic

type answer = [ `Refused | `Unreachable ]

let portable = function
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> Some `Refused
  | _ -> None

(* The classifiers in registration order. [register] runs at module
   initialisation, before any fiber classifies. *)
let registered = ref []

let classify exn =
  match !registered with
  | [] -> portable exn
  | classifiers -> List.find_map (fun f -> f exn) classifiers

let register f = registered := !registered @ [ f ]
