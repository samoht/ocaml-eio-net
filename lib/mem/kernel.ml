type os = Linux | Macos

let of_arg = function None | Some `Linux -> Linux | Some `Macos -> Macos
