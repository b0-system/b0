(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let err_missing_preset () =
  B0b_cli.err_missing_arg ~kind:Conf.Preset.value_kind; Ok `Cli_error

let find_presets =
  let log = Log.Error in
  let kind = Conf.Preset.value_kind in
  let list = Conf.Preset.list in
  let get_or_suggest = Conf.Preset.get_or_suggest in
  B0b_cli.find_named_values ~log ~kind ~list ~get_or_suggest

let show_preset =
  let short = Conf.Preset.pp_name in
  let normal = Conf.Preset.pp_synopsis in
  let long = Conf.Preset.pp_info in
  B0b_cli.show_value ~short ~normal ~long

let show_keys out_fmt p =
  let show out_fmt def =
    let Conf.Preset.B (k, v) = Conf.Preset.def_binding def in
    let pp_value = Conf.pp_value @@ (Conv.print @@ Conf.Key.conv k) in
    let ext ppf _ =
      Fmt.cut ppf ();
      Fmt.field "preset" pp_value ppf v
    in
    B0b_cli.show_key ~ext out_fmt (Conf.Key.of_typed k)
  in
  List.iter (show out_fmt) (Conf.Preset.defs p)

(* Actions *)

let list_action =
  B0b_cli.list_values_action ~find:find_presets ~show:show_preset

let info_action = B0b_cli.value_info_action ~list_action

let keys_action out_fmt = function
| [] -> err_missing_preset ()
| ps ->
    let presets, exit = find_presets ps in
    List.iter (show_keys out_fmt) presets;
    Ok exit

let apply_action out_fmt = function
| [] -> err_missing_preset ()
| ps -> failwith "TODO"

let preset action out_fmt args setup =
  begin match action with
  | `List -> list_action out_fmt args
  | `Info -> info_action out_fmt args
  | `Keys -> keys_action out_fmt args
  | `Apply -> apply_action out_fmt args
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg
    [ ("list", `List); ("info", `Info); ("keys", `Keys); ("apply", `Apply)]

let doc = "Operate on configuration presets"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on configuration presets.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,PRESET)]...",
        "List all or given presets.");
    `I ("$(b,info) [$(i,PRESET)]...",
        "Show information about all or given presets.");
    `I ("$(b,keys) $(i,PRESET)...",
        "Show keys defined in the given presets.");
    `I ("$(b,apply) $(i,PRESET)...",
        "Apply the given presets to the next configuration. If
         more than one preset sets the same key, the value defined
         by the rightmost one takes over. For more control
         over which keys are set use $(b,key set).");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure preset $ action $ B0_driver.Cli.out_fmt $ B0b_cli.action_pos_args),
  Term.info "preset" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
  `Instance

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
