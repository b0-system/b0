(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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

let err n fmt = Fmt.failwith_notrace ("%d:" ^^ fmt) n
let err_file file e = Fmt.error "%a:%s" Fpath.pp_unquoted file e

let fold ?(file = Os.File.dash) data f acc =
  if String.equal data "" then Ok acc else
  let rec loop f acc n = function
  | [] -> acc
  | l :: ls -> loop f (f n l acc) (n + 1) ls
  in
  try Ok (loop f acc 1 (of_string data)) with Failure e -> err_file file e

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
