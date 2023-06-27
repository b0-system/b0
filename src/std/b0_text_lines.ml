(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let of_string s = (* adapted from the stdlib's String.split_on_char *)
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = '\n' then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := if i <> 0 && String.unsafe_get s (i - 1) = '\r' then i - 1 else i
    end
  done;
  String.sub s 0 !j :: !r

let fail n fmt = Fmt.failwith_notrace ("%d:" ^^ fmt) n
let file_error ?(file = Fpath.dash) e = Fmt.error "%a:%s" Fpath.pp file e

let fold ?(file = Fpath.dash) data f acc =
  if String.equal data "" then Ok acc else
  let rec loop f acc n = function
  | [] -> acc
  | l :: ls -> loop f (f n l acc) (n + 1) ls
  in
  try Ok (loop f acc 1 (of_string data)) with Failure e -> file_error ~file e
