open! Hermetic
open Kernel
open Address

let is_space = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let is_digit c = c >= '0' && c <= '9'
let ulong_max = "18446744073709551615"

(* The leading spaces of [service], its sign and its digits: whether it is
   negative, and where the digits start and end. *)
let scan service =
  let n = String.length service in
  let rec skip i = if i < n && is_space service.[i] then skip (i + 1) else i in
  let i = skip 0 in
  let neg, i =
    if i < n && (service.[i] = '+' || service.[i] = '-') then
      (service.[i] = '-', i + 1)
    else (false, i)
  in
  let rec digits j =
    if j < n && is_digit service.[j] then digits (j + 1) else j
  in
  let j = digits i in
  (neg, i, j)

(* [service] read as strtoul(3) reads it in base 10, the whole string or
   nothing: leading spaces, a sign, digits, its value modulo 2^64 or
   ULONG_MAX past it. The empty string reads as 0. *)
let strtoul service =
  let n = String.length service in
  let neg, i, j = scan service in
  if n = 0 then Some (false, false, 0L)
  else if j = i || j <> n then None
  else
    let ds = String.sub service i (j - i) in
    let rec strip k =
      if k < String.length ds - 1 && ds.[k] = '0' then strip (k + 1) else k
    in
    let ds = String.sub ds (strip 0) (String.length ds - strip 0) in
    let overflow =
      String.length ds > 20
      || (String.length ds = 20 && String.compare ds ulong_max > 0)
    in
    (* The low 32 bits are all a port needs. *)
    let low =
      String.fold_left
        (fun acc c ->
          Int64.(logand (add (mul acc 10L) (of_int (Char.code c - 48))))
            0xffff_ffffL)
        0L ds
    in
    Some (neg, overflow, low)

(* glibc keeps strtoul's result in an int: a negative one is no number, and
   the port is its low 16 bits. *)
let port service =
  match strtoul service with
  | None -> None
  | Some (_, true, _) -> None
  | Some (neg, false, low) ->
      let v =
        if neg then Int64.(logand (sub 0x1_0000_0000L low) 0xffff_ffffL)
        else low
      in
      if Int64.compare v 0x8000_0000L >= 0 then None
      else Some (Int64.to_int v land 0xffff)

(* A part of an inet_aton(3) address: hexadecimal after 0x, octal after 0,
   decimal otherwise. A bare 0x is no number. *)
let part s =
  let n = String.length s in
  let value base digits =
    if digits = "" then None
    else
      String.fold_left
        (fun acc c ->
          match acc with
          | None -> None
          | Some v ->
              let d =
                match c with
                | '0' .. '9' -> Char.code c - 48
                | 'a' .. 'f' -> Char.code c - 87
                | 'A' .. 'F' -> Char.code c - 55
                | _ -> 99
              in
              if d >= base then None
              else
                let v = Int64.(add (mul v (of_int base)) (of_int d)) in
                if Int64.compare v 0xffff_ffffL > 0 then None else Some v)
        (Some 0L) digits
  in
  if n >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
    value 16 (String.sub s 2 (n - 2))
  else if n >= 1 && s.[0] = '0' then value 8 s
  else value 10 s

(* macOS's port: strtoul(3)'s value, which must be at most 65535, so a
   negative one is no port but -0. *)
let port_macos service =
  let n = String.length service in
  let neg, i, j = scan service in
  if n = 0 then Some 0
  else if j = i || j <> n then None
  else
    let rec strip k =
      if k < j - 1 && service.[k] = '0' then strip (k + 1) else k
    in
    let k = strip i in
    if j - k > 5 then None
    else
      let v = int_of_string (String.sub service k (j - k)) in
      if neg then if v = 0 then Some 0 else None
      else if v <= 65535 then Some v
      else None

(* An IPv4 address as inet_aton(3) reads it: one to four parts, the last
   filling the bytes left. macOS's reads a bare 0x as 0 but in the last
   part. *)
let inet_aton ~os s =
  let parts = String.split_on_char '.' s in
  let last = List.length parts - 1 in
  let parts =
    List.mapi
      (fun i p ->
        if os = Macos && i < last && String.lowercase_ascii p = "0x" then
          Some 0L
        else part p)
      parts
  in
  if List.mem None parts then None
  else
    let parts = List.map Option.get parts in
    let k = List.length parts in
    let rec fold acc = function
      | [] -> Some acc
      | [ last ] ->
          let bits = 8 * (5 - k) in
          if k > 4 || Int64.compare last (Int64.shift_left 1L bits) >= 0 then
            None
          else Some Int64.(logor (shift_left acc bits) last)
      | x :: rest ->
          if Int64.compare x 255L > 0 then None
          else fold Int64.(logor (shift_left acc 8) x) rest
    in
    match fold 0L parts with
    | None -> None
    | Some v ->
        Some
          (Eio.Net.Ipaddr.of_raw
             (String.init 4 (fun i ->
                  Char.chr
                    (Int64.to_int
                       Int64.(logand (shift_right_logical v (8 * (3 - i))) 255L)))))

(* /etc/hosts names the loopback addresses "localhost", in any case. With
   no name server, "localhost." is another name. *)
let localhost name = String.lowercase_ascii name = "localhost"

(* An IPv6 address, in glibc's form, with no brackets. *)
let ipv6 s =
  if String.length s > 0 && s.[0] = '[' then None
  else
    match Ipaddr.V6.of_string s with
    | Ok ip -> Some (Eio.Net.Ipaddr.of_raw (Ipaddr.V6.to_octets ip))
    | Error _ -> None

(* A scope glibc takes after an IPv6 address: a number of 32 bits, or the
   name of an interface, which only the loopback one has, for an address
   that needs a scope. *)
let scope ~os ip s =
  (os = Macos && s <> "")
  ||
  let value =
    String.fold_left
      (fun v c ->
        if Int64.compare v 0xffff_ffffL > 0 then v
        else Int64.(add (mul v 10L) (of_int (Char.code c - 48))))
      0L s
  in
  (s <> "" && String.for_all is_digit s && Int64.compare value 0xffff_ffffL <= 0)
  || (s = "lo" && needs_scope ip)

(* Windows reads a dotted quad only, an IPv6 address in brackets too, and
   a numeric scope only. *)
let number_windows name =
  let n = String.length name in
  if n > 2 && name.[0] = '[' && name.[n - 1] = ']' then
    ipv6 (String.sub name 1 (n - 2))
  else if List.length (String.split_on_char '.' name) = 4 then
    match inet_aton ~os:Windows name with
    | Some ip -> Some ip
    | None -> ipv6 name
  else
    match String.index_opt name '%' with
    | None -> ipv6 name
    | Some i ->
        let s = String.sub name (i + 1) (n - i - 1) in
        if s <> "" && String.for_all is_digit s then ipv6 (String.sub name 0 i)
        else None

let number ~os name =
  if os = Windows then number_windows name
  else
    match inet_aton ~os name with
    | Some ip -> Some ip
    | None -> (
        match String.index_opt name '%' with
        | None -> ipv6 name
        | Some i -> (
            match ipv6 (String.sub name 0 i) with
            | Some ip
              when scope ~os ip
                     (String.sub name (i + 1) (String.length name - i - 1)) ->
                Some ip
            | Some _ | None -> None))

(* The addresses of a host: a number as it reads, the loopback addresses
   for "localhost" and the empty name, IPv6 first as glibc and macOS order
   them on a dual-stack host, and nothing for any other name. *)
let host ~os name : Eio.Net.Ipaddr.v4v6 list =
  let loopback =
    [
      (Eio.Net.Ipaddr.V6.loopback :> Eio.Net.Ipaddr.v4v6);
      (Eio.Net.Ipaddr.V4.loopback :> Eio.Net.Ipaddr.v4v6);
    ]
  in
  if name = "" || localhost name then loopback
  else Option.to_list (number ~os name)
