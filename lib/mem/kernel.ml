type os = Linux | Macos | Windows

let of_arg = function
  | None | Some `Linux -> Linux
  | Some `Macos -> Macos
  | Some `Windows -> Windows
