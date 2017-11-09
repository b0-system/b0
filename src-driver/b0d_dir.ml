(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Defaults *)

type defaults =
  { mutable variant : string option;
    mutable variant_scheme : string option; }

let defaults_none = { variant = None; variant_scheme = None }
let codec = Codec.v ~id:"defaults"

(* B0 directory *)

type t =
  { b0_dir : Fpath.t;
    defaults : defaults Lazy.t; }

let exists d =
  (OS.Dir.exists d.b0_dir)
  |> Log.on_error_msg ~level:Log.Warning ~use:(fun _ -> false)

let must_exist d = match exists d with
| false -> R.error_msgf "b0 directory %a does not exist." Fpath.pp d.b0_dir
| true -> Ok ()

let dir d = d.b0_dir
let variant_dir d = Fpath.(d.b0_dir / "v")

(* Defaults *)

let defaults_file d = Fpath.(d.b0_dir / "defaults")
let get_defaults d = Lazy.force d.defaults

let write_defaults d =
  must_exist d >>= fun () ->
  Codec.write codec (defaults_file d) (get_defaults d)

let read_defaults d =
  begin
    let file = defaults_file d in
    OS.File.exists file >>= function
    | true -> Codec.read codec file
    | false -> Ok defaults_none
  end
  |> Log.on_error_msg ~level:Log.Warning ~use:(fun _ -> defaults_none)

let v ~b0_dir =
  let rec d = { b0_dir; defaults = lazy (read_defaults d) } in
  d


let default_variant_name d = match (get_defaults d).variant with
| None | Some "" -> None
| Some v -> Some v

let set_default_variant_name d n =
  (get_defaults d).variant <- n;
  write_defaults d

let default_variant_scheme_name d = (get_defaults d).variant_scheme
let set_default_variant_scheme_name d n =
  (get_defaults d).variant_scheme <- n;
  write_defaults d


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
