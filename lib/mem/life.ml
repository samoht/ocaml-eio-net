type t = Open of int | Closing of int | Closed

let v = Open 0
let is_open = function Open _ -> true | Closing _ | Closed -> false
let is_closed = function Closed -> true | Open _ | Closing _ -> false

let enter = function
  | Open n -> Some (Open (n + 1))
  | Closing _ | Closed -> None

let leave = function
  | Open n -> (Open (n - 1), false)
  | Closing 1 -> (Closed, true)
  | Closing n -> (Closing (n - 1), false)
  | Closed -> (Closed, false)

let close = function
  | Open 0 -> (Closed, `Now)
  | Open n -> (Closing n, `Deferred)
  | (Closing _ | Closed) as t -> (t, `Again)
