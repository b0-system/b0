(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

type 'a t = { id : string }

let v ~id = { id }
let magic = Printf.sprintf "b0-%%VERSION%%-ocaml-%s-" Sys.ocaml_version
let magic c = magic ^ c.id

let write : type a. a t -> B0_fpath.t -> a -> unit result =
fun c f v ->
  let write oc v =
    try
      output_string oc (magic c);
      output_value oc v;
      flush oc;
      Ok ()
    with Sys_error e -> R.error_msgf "%a: %s" B0_fpath.pp f e
  in
  R.join @@ B0_os.File.with_oc f write v

let read : type a. a t -> B0_fpath.t -> a result =
fun c f ->
  let read ic () =
    let magic = magic c in
    let m = really_input_string ic (String.length magic) in
    match m = magic with
    | true -> Ok (input_value ic : a)
    | false ->
        R.error_msgf
          "%a: invalid magic number %S, expected %S" B0_fpath.pp f m magic
  in
  R.join @@ B0_os.File.with_ic f read ()

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
