(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let find_pkgs = B0b_cli.find_pkgs ~log:(Some Log.Error)

(* Actions *)

let list_action =
  B0b_cli.list_values_action ~find:find_pkgs ~show:B0b_cli.show_pkg

let info_action = B0b_cli.value_info_action ~list_action
let units_action out_fmt names = match find_pkgs names with
| _, (`Unknown_name as exit) -> Ok exit
| pkgs, `Ok ->
    let units = B0b_cli.units_of_pkgs pkgs in
    List.iter (B0b_cli.show_unit out_fmt) units;
    Ok `Ok

let pkg action out_fmt args setup =
  begin match action with
  | `List -> list_action out_fmt args
  | `Info -> info_action out_fmt args
  | `Units -> units_action out_fmt args
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg [ "list", `List; "info", `Info; "units", `Units]

let doc = "Operate on packages"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]

let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on packages.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,PKG)]...",
        "List all or given packages.");
    `I ("$(b,info) [$(i,PKG)]...",
        "Show information about all or given packages.");
    `I ("$(b,units) $(i,PKG)...",
        "List the units of all or given packages.");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure pkg $ action $ B0_driver.Cli.out_fmt $ B0b_cli.action_pos_args),
  Term.info "pkg" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
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
