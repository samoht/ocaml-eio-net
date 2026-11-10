open! Hermetic

type t =
  | Short of int
  | Stall of float
  | Hang
  | Reset
  | Eof
  | Refuse
  | Timeout
  | Emfile
  | Drop
  | Dup
  | Reorder
  | Garble of int

let pp ppf = function
  | Short n -> Fmt.pf ppf "moves %d byte(s)" (max 1 n)
  | Stall d -> Fmt.pf ppf "stalls %gs" d
  | Hang -> Fmt.string ppf "hangs"
  | Reset -> Fmt.string ppf "is reset"
  | Eof -> Fmt.string ppf "ends"
  | Refuse -> Fmt.string ppf "is refused"
  | Timeout -> Fmt.string ppf "times out"
  | Emfile -> Fmt.string ppf "fails with EMFILE"
  | Drop -> Fmt.string ppf "is lost"
  | Dup -> Fmt.string ppf "is duplicated"
  | Reorder -> Fmt.string ppf "is reordered"
  | Garble n -> Fmt.pf ppf "is replaced with %d bytes of noise" n

let key = function
  | Short n -> Fmt.str "short%d" n
  | Stall d -> Fmt.str "stall%g" d
  | Hang -> "hang"
  | Reset -> "reset"
  | Eof -> "eof"
  | Refuse -> "refuse"
  | Timeout -> "timeout"
  | Emfile -> "emfile"
  | Drop -> "drop"
  | Dup -> "dup"
  | Reorder -> "reorder"
  | Garble n -> Fmt.str "garble%d" n
