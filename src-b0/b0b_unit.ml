(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let find_units = B0b_cli.find_units ~log:Log.Error
let show_unit = B0b_cli.show_unit

let edit_units names =
  let add_unit acc u = match Def.Loc.find_file @@ Unit.loc u with
  | Some f -> f :: acc
  | None ->
      Log.err (fun m -> m "%a: Can't find description file." Unit.pp_name u);
      acc
  in
  let fs = List.(rev @@ fold_left add_unit [] names) in
  B0b_cli.edit_files fs

(* Actions *)

let list_action =
  B0b_cli.list_values_action ~find:find_units ~show:show_unit

let info_action = B0b_cli.value_info_action ~list_action

let edit_action names =
  (* FIXME add a flag that looks up the failed unit names in the last
     build outcome *)
  match find_units names with
  | [], ret -> Ok ret
  | names, ret ->
      let ret code = match code with 0 -> Ok ret | n -> Ok `Some_error in
      edit_units names >>= fun code -> ret code

let unit action out_fmt args setup =
  begin match action with
  | `List -> list_action out_fmt args
  | `Info -> info_action out_fmt args
  | `Edit -> edit_action args
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg [ "list", `List; "info", `Info; "edit", `Edit]

let doc = "Operate on build units"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]
let envs =
  [ Term.env_info "EDITOR" ~doc:"The editor used to edit unit descriptions."; ]

let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on build units.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,UNIT)]...",
        "List all or given units.");
    `I ("$(b,info) [$(i,UNIT)]...",
        "Show information about all or given units.");
    `I ("$(b,edit) $(i,EDIT)...",
        "Open the descripition file(s) of the given units in your $(b,EDITOR)");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure unit $ action $ B0_driver.Cli.out_fmt $ B0b_cli.action_pos_args),
  Term.info "unit" ~doc ~sdocs ~exits:B0b_cli.exits ~envs ~man ~man_xrefs,
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
