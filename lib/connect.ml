open! Hermetic

type answer = [ `Refused | `Unreachable ]

let classify = function
  | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) -> Some `Refused
  | _ -> None
