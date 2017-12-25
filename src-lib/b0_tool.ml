(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* Env vars *)

type env_vars = string list
let tmp_vars = ["TMPDIR"; "TEMP"; "TMP"]

(* Tool *)

type t =
  { name : B0_fpath.t;
    env_vars : env_vars;
    internal : env_vars;
    unit : B0_unit.t option; }

let v ?(internal = tmp_vars) ?(env_vars = []) name =
  match B0_os.Cmd.exe_is_path name with
  | false -> invalid_arg (B0_string.strf "%S: not a tool name" name)
  | true -> { name = B0_fpath.v name; env_vars; internal; unit = None }

let of_file ?(internal = tmp_vars) ?(env_vars = []) name =
  { name; env_vars; internal; unit = None }

let of_unit_file u ?(internal = tmp_vars) ?(env_vars = []) name =
  { name; env_vars; internal; unit = Some u }

let name t = t.name
let env_vars t = t.env_vars
let internal t = t.internal
let unit t = t.unit

let lookup_env t env =
  let add_var acc var = match B0_string.Map.find var env with
  | v -> B0_string.Map.add var v acc
  | exception Not_found -> acc
  in
  let ext = List.fold_left add_var B0_string.Map.empty t.env_vars in
  let all = List.fold_left add_var ext t.internal in
  all, ext

(* Configuration *)

let discover ~internal ~env_vars ~tools name env aim k c =
  let tools = match tools with None -> [B0_fpath.v name] | Some ts -> ts in
  B0_env.tool env aim tools >>|
  function name -> { name; env_vars; internal; unit = None }

let conv ~internal ~env_vars =
  let dec s =
    B0_fpath.of_string s >>| fun name ->
    { name; env_vars; internal; unit = None }
  in
  let enc t = Ok (B0_fpath.to_string t.name) in
  let pp ppf t = B0_fpath.pp ppf t.name in
  let codec = B0_conv.codec (dec, enc) in
  let text = B0_conv.text (dec, pp) in
  B0_conv.v ~docv:"TOOL" ~codec text

let key
    ?loc ?doc ?group ?(internal = tmp_vars) ?(env_vars = []) ?tools name
  =
  let discover = discover ~internal ~env_vars ~tools name in
  let default = B0_conf.discover ~store:false discover in
  let conv = conv ~internal ~env_vars in
  B0_conf.key ?loc ?doc ?group name conv ~default

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
