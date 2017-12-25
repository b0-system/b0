(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let variant_has_scheme v s = Variant.Scheme.equal s (Variant.scheme v)

let all_variants dir =
  (Variant.list ~dir)
  |> Log.on_error_msg ~level:Log.Warning ~use:(fun _ -> [])
  |> List.map Variant.of_load

let find_schemes names =
  let log = Log.Error in
  let kind = Variant.Scheme.value_kind in
  let list = Variant.Scheme.list in
  let get_or_suggest = Variant.Scheme.get_or_suggest in
  B0b_cli.find_named_values ~log ~kind ~list ~get_or_suggest names

let show_scheme =
  let short = Variant.Scheme.pp_name in
  let normal = Variant.Scheme.pp_synopsis in
  (* TODO ext, used-by: *)
  let long = Variant.Scheme.pp_info in
  B0b_cli.show_value ~short ~normal ~long

(* Actions *)

let list_action =
  B0b_cli.list_values_action ~find:find_schemes ~show:show_scheme

let info_action =
  B0b_cli.value_info_action ~list_action

let variants_action ~b0_dir out_fmt = function
| [] -> B0b_cli.err_missing_arg ~kind:Variant.Scheme.value_kind; Ok `Cli_error
| gs ->
    let ss, exit = find_schemes gs in
    let vs = all_variants (B0_dir.variant_dir b0_dir) in
    let vs = List.filter (fun v -> List.exists (variant_has_scheme v) ss) vs in
    let vs = List.sort Variant.compare_by_name vs in
    List.iter (B0b_cli.show_variant out_fmt) vs;
    Ok exit

let scheme action out_fmt args setup =
  let b0_dir = B0_driver.Driver.b0_dir setup in
  begin match action with
  | `Variants -> variants_action b0_dir  out_fmt args
  | `List -> list_action out_fmt args
  | `Info -> info_action out_fmt args
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg
    [ ("list", `List); ("info", `Info); ("variants", `Variants) ]

let doc = "Operate on variant schemes"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on variant schemes. For extended
        documentation about the behavour of variant schemes, consult
        corresponding their definition in the API documentation.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,SCHEME)]...",
        "List all or given variant schemes.");
    `I ("$(b,info) [$(i,SCHEME)]...",
        "Show information about all or given variant schemes.");
    `I ("$(b,variants) $(i,SCHEME)...",
        "List variants using the given variant schemes.");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure scheme $ action $ B0_driver.Cli.out_fmt $ B0b_cli.action_pos_args),
  Term.info "scheme" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
  `Instance

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
