open! Hermetic

type t =
  | Short of int
  | Stall of float
  | Hang
  | Reset
  | Eof
  | Refuse
  | Timeout
  | Unreachable
  | No_descriptor
  | Drop
  | Dup
  | Reorder
  | Garble of int
  | Cancel of int

let pp ppf = function
  | Short n ->
      let n = max 1 n in
      Fmt.pf ppf "moves %d byte%s" n (Text.plural n)
  | Stall d -> Fmt.pf ppf "stalls %gs" d
  | Hang -> Fmt.string ppf "hangs"
  | Reset -> Fmt.string ppf "is reset"
  | Eof -> Fmt.string ppf "ends"
  | Refuse -> Fmt.string ppf "is refused"
  | Timeout -> Fmt.string ppf "times out"
  | Unreachable -> Fmt.string ppf "finds its host unreachable"
  | No_descriptor -> Fmt.string ppf "finds no descriptor left"
  | Drop -> Fmt.string ppf "is lost"
  | Dup -> Fmt.string ppf "is duplicated"
  | Reorder -> Fmt.string ppf "is reordered"
  | Garble n -> Fmt.pf ppf "is replaced with %d bytes of noise" n
  | Cancel n when n <= 0 -> Fmt.string ppf "is cancelled before it starts"
  | Cancel n -> Fmt.pf ppf "is cancelled after %d turn%s" n (Text.plural n)

let key = function
  | Short n -> Fmt.str "short%d" n
  | Stall d -> Fmt.str "stall%g" d
  | Hang -> "hang"
  | Reset -> "reset"
  | Eof -> "eof"
  | Refuse -> "refuse"
  | Timeout -> "timeout"
  | Unreachable -> "unreachable"
  | No_descriptor -> "no_descriptor"
  | Drop -> "drop"
  | Dup -> "dup"
  | Reorder -> "reorder"
  | Garble n -> Fmt.str "garble%d" n
  | Cancel n -> Fmt.str "cancel%d" (max 0 n)
