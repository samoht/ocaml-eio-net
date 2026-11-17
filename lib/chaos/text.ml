open! Hermetic

let plural n = if n = 1 then "" else "s"

let pp_count ppf n =
  let s = string_of_int n in
  let len = String.length s in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Fmt.char ppf ',';
      Fmt.char ppf c)
    s
