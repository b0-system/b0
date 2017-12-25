(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let find_groups names =
  let log = Log.Error in
  let kind = Conf.Group.value_kind in
  let list = Conf.Group.list in
  let get_or_suggest = Conf.Group.get_or_suggest in
  B0b_cli.find_named_values ~log ~kind ~list ~get_or_suggest names

let show_group =
  let short = Conf.Group.pp_name in
  let normal = Conf.Group.pp_synopsis in
  let long = Conf.Group.pp_info in
  B0b_cli.show_value ~short ~normal ~long

let show_group_keys out_fmt g =
  List.iter (B0b_cli.show_key out_fmt) (Conf.Group.keys g)

(* Actions *)

let list_action =
  B0b_cli.list_values_action ~find:find_groups ~show:show_group

let info_action = B0b_cli.value_info_action ~list_action

let keys_action out_fmt = function
| [] -> B0b_cli.err_missing_arg ~kind:Conf.Group.value_kind; Ok `Cli_error
| gs ->
    let groups, exit = find_groups gs in
    List.iter (show_group_keys out_fmt) groups;
    Ok exit

let group action out_fmt args setup =
  begin match action with
  | `List -> list_action out_fmt args
  | `Info -> info_action out_fmt args
  | `Keys -> keys_action out_fmt args
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg [ "list", `List; "info", `Info; "keys", `Keys; ]

let doc = "Operate on configuration key groups"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on configuration key groups.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,GROUP)]...",
        "List all or given key groups.");
    `I ("$(b,info) [$(i,GROUP)]...",
        "Show information about all or given groups.");
    `I ("$(b,keys) $(i,GROUP)...",
        "Show keys belonging to the given groups.");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure group $ action $ B0_driver.Cli.out_fmt $ B0b_cli.action_pos_args),
  Term.info "group" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
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
