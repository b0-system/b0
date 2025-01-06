(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Char

module Ascii = struct

  (* Characters *)

  let min = '\x00'
  let max = '\x7F'

  (* Predicates *)

  let is_valid = function '\x00' .. '\x7F' -> true | _ -> false
  let is_upper = function 'A' .. 'Z' -> true | _ -> false
  let is_lower = function 'a' .. 'z' -> true | _ -> false
  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
  let is_alphanum = function
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
  let is_blank = function ' ' | '\t' -> true | _ -> false
  let is_graphic = function '!' .. '~' -> true | _ -> false
  let is_print = function ' ' .. '~' -> true | _ -> false
  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  (* Decimal digits *)

  let is_digit = function '0' .. '9' -> true | _ -> false
  let digit_to_int = function
    | '0' .. '9' as c -> code c - 0x30
    | c -> invalid_arg (escaped c ^ ": not a decimal digit")

  let digit_of_int n = unsafe_chr (0x30 + abs (n mod 10))

  (* Hexadecimal digits *)

  let is_hex_digit = function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false

  let hex_digit_to_int = function
    | '0' .. '9' as c -> code c - 0x30
    | 'A' .. 'F' as c -> 10 + code c - 0x41
    | 'a' .. 'f' as c -> 10 + code c - 0x61
    | c -> invalid_arg (escaped c ^ ": not a hexadecimal digit")

  let lower_hex_digit_of_int n =
    let d = abs (n mod 16) in
    unsafe_chr (if d < 10 then 0x30 + d else 0x57 + d)

  let upper_hex_digit_of_int n =
    let d = abs (n mod 16) in
    unsafe_chr (if d < 10 then 0x30 + d else 0x37 + d)

  (* Casing transforms *)

  let lowercase = lowercase_ascii
  let uppercase = uppercase_ascii
end
