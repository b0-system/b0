(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let pp_scheme_hook_error ~hook v ppf (`Msg m) =
  Fmt.pf ppf "@[<v 2>Variant %a: scheme %a %s hook error:@,%s@]"
    Variant.pp_name v Variant.Scheme.pp_name (Variant.scheme v) hook m

let find_variants dir =
  let log = Log.Error in
  let kind = Variant.value_kind in
  let list () = (Variant.list ~dir) |> Log.on_error_msg ~use:(fun _ -> []) in
  let get_or_suggest n =
    Variant.get_or_suggest ~dir n |> Log.on_error_msg ~use:(fun _ -> Error [])
  in
  B0b_cli.find_named_values ~log ~kind ~list ~get_or_suggest

let show_variant out_fmt l =
  let v = match l with
  | Ok v -> v
  | Error u ->
      Log.warn (fun m -> m "%a" Variant.pp_unknown_scheme u);
      Variant.of_unknown_scheme u
  in
  B0b_cli.show_variant out_fmt v

(* Handling the default variant *)

let default_clear ~b0_dir =
  B0_dir.set_default_variant_name b0_dir None >>= fun () -> Ok `Ok

let default_set ~b0_dir v =
  B0_dir.set_default_variant_name b0_dir (Some v) >>= fun () ->
  let dir = B0_dir.variant_dir b0_dir in
  (* TODO Warn if the effective variant is different *)
  Variant.get_or_suggest ~dir v >>= function
  | Ok v -> Ok `Ok
  | Error sugg ->
      B0b_cli.did_you_mean ~log:Log.Warning ~kind:Variant.value_kind
        ~pre:"Default set but" ~post:" unknown" (v, sugg);
      Ok `Ok

let default_get ~b0_dir which =
  match B0b_cli.get_default_variant_name ~log:Log.Error which b0_dir with
  | Ok v -> Log.app (fun m -> m "%a" Variant.pp_name_str v); Ok `Ok
  | Error exit -> Ok exit

(* Actions *)

let list_action ~b0_dir =
  let dir = B0_dir.variant_dir b0_dir in
  B0b_cli.list_values_action ~find:(find_variants dir) ~show:show_variant

let info_action ~b0_dir =
  B0b_cli.value_info_action ~list_action:(list_action ~b0_dir)

let create_action ~b0_dir name ~no_preset ~create_only args =
  let log = Log.Error in
  let make_default = not create_only in
  let preset = not no_preset in
  let parse_scheme_name = function
  | [] -> Ok None
  | [s] -> Ok (Some s)
  | args -> B0b_cli.err_too_many_args args; Error `Cli_error
  in
  begin
    parse_scheme_name args
    >>= fun scheme_name -> B0b_cli.get_variant_scheme ~log scheme_name
    >>= fun scheme ->
    B0b_cli.variant_create ~log ~b0_dir ~name ~scheme ~preset ~make_default
    >>= fun _ -> Ok `Ok
  end
  |> B0b_cli.join_exit

let get_action ~b0_dir which args = match args <> [] with
| true -> B0b_cli.err_too_many_args args; Ok `Cli_error
| false -> default_get ~b0_dir which

let set_action ~b0_dir clear args = match clear with
| true ->
    begin match args <> [] with
    | true -> B0b_cli.err_too_many_args args; Ok `Cli_error
    | false -> default_clear ~b0_dir
    end
| false ->
    match args with
    | [v] -> default_set ~b0_dir v
    | [] -> B0b_cli.err_missing_arg ~kind:"variant"; Ok `Cli_error
    | args -> B0b_cli.err_too_many_args args; Ok `Cli_error

let delete_action ~b0_dir force all = function
| [] when not all ->
    B0b_cli.err_missing_arg ~kind:Variant.value_kind; Ok `Cli_error
| vs ->
    let pp_hint ppf () = Fmt.pf ppf "@[Not deleted, use --force to ignore@]" in
    let dir = B0_dir.variant_dir b0_dir in
    let vs = if all then [] (* all for find_variants *) else vs in
    let vs, exit = find_variants dir vs in
    let delete exit = function
    | Error (`Unknown_scheme _ as u) when not force ->
        Log.err
          (fun m -> m "@[<v>%a@,%a@]" Variant.pp_unknown_scheme u pp_hint ());
        `Some_error
    | l ->
        let v = Variant.of_load l in
        match Variant.delete ~force v with
        | Ok () -> exit
        | Error (`Msg _) as e -> Log.on_error_msg e ~use:(fun _ -> `Some_error)
        | Error (`Scheme (`Msg _ as msg)) ->
            Log.err
              (fun m -> m "@[<v>%a@,%a@]"
                  (pp_scheme_hook_error ~hook:"deletion" v) msg pp_hint ());
            `Hook_error
    in
    Ok (List.fold_left delete exit vs)

let path_action ~b0_dir out_fmt path_which args =
  let dir = B0_dir.variant_dir b0_dir in
  let vs = match args with
  | [] ->
      let log = Log.Error in
      B0b_cli.get_default_variant_name ~log `Effective b0_dir
      >>= fun name -> B0b_cli.load_variant ~log ~dir name >>| fun v -> [v]
  | vs ->
      let vs, exit = find_variants dir vs in
      match exit with
      | `Unknown_name as e -> Error e
      | `Ok -> Ok vs
  in
  let normal show v = Log.app (fun m -> m "%a" show v) in
  let long show v = Log.app (fun m -> m "%a: %a" Variant.pp_name v show v) in
  vs >>= fun vs ->
  let show_path = match path_which with
  | `Variant_dir -> fun ppf v -> Fpath.pp ppf (Variant.path v)
  | `Build_dir -> fun ppf v -> Fpath.pp ppf (Variant.build_path v)
  in
  let show = match out_fmt, vs with
  | `Normal, [v] -> normal show_path
  | `Normal, vs -> long show_path
  | `Short, _ -> normal show_path
  | `Long, _ -> long show_path
  in
  List.iter (fun v -> show (Variant.of_load v)) vs; Ok `Ok

let variant
    action out_fmt get_which path_which set_clear create_only no_preset force
    all name args setup
  =
  let b0_dir = Driver.b0_dir setup in
  begin match action with
  | `List -> list_action ~b0_dir out_fmt args
  | `Info -> info_action ~b0_dir out_fmt args
  | `Create -> create_action ~b0_dir name ~no_preset ~create_only args
  | `Delete -> delete_action ~b0_dir force all args
  | `Get -> get_action ~b0_dir get_which args
  | `Set -> set_action ~b0_dir set_clear args
  | `Path -> (path_action ~b0_dir out_fmt path_which args) |> B0b_cli.join_exit
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg_with_default ~default:`Get
    [ ("list", `List); ("info", `Info); ("create", `Create);
      ("delete", `Delete); ("get", `Get); ("set", `Set); ("path", `Path) ]

let all =
  let doc = "Delete all variants." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let force =
  let doc = "Force deletion even if the variant scheme's delete hook fails." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let create_only =
  let doc =
    "Create only, do not make the created variant the default variant."
  in
  Arg.(value & flag & info ["c"; "create-only"] ~doc)

let no_preset =
  let doc = "Do not apply the scheme's preset on variant creation." in
  Arg.(value & flag & info ["no-preset" ] ~doc)

let set_clear =
  let doc = "Clear the stored default variant ($(b,set) action)." in
  Arg.(value & flag & info ["clear"] ~doc)

let get_which =
  let effective =
    let doc =
      "Get the effective default variant. Either the value of the
       env $(b,B0_VARIANT) or the description default variant.";
    in
    Arg.info ["effective"] ~doc
  in
  let stored =
    let doc = "Get the default variant stored in the _b0 directory." in
    Arg.info ["stored"] ~doc
  in
  Arg.(value & vflag `Effective [`Effective, effective; `Stored, stored])

let path_which =
  let build_dir =
    let doc = "Show the build directory path of the variant" in
    Arg.info ["b"; "build-dir"] ~doc
  in
  Arg.(value & vflag `Variant_dir [`Build_dir, build_dir])

let vname =
  let doc = "Name $(docv) of the created variant." in
  Arg.(value & opt (some string) None & info ["n"; "name"] ~doc ~docv:"VARIANT")

let doc = "Operate on build variants"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "key" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on build variants.";
    `S "ACTIONS";
    `I ("$(b,list) [$(i,VARIANT)]...",
        "List all or given variants.");
    `I ("$(b,info) [$(i,VARIANT)]...",
        "Show information about all or given variants.");
    `I ("$(b,create) [$(i,SCHEME)] [-n $(i,VARIANT)]",
        "Create variant named $(i,VARIANT) with scheme $(i,SCHEME). If
         $(i,VARIANT) is unspecified it is derived from the variant
         name. If no scheme is specified the description's default scheme
         is used.");
    `I ("$(b,delete) [$(b,--all) | $(i,VARIANT)...]",
         "Delete given variants.");
    `I ("$(b,get) [$(b,--effective) | $(b,--stored)]",
        "Get the default variant. If no option is specified
         $(b,--effective) is used.");
    `I ("$(b,path) [$(b,-b)] [$(i,VARIANT)]...",
         "Show the path to the directory of default or given variants.
          Use $(b,-b) to show the build directory of the variant.");
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure variant $ action $ B0_driver.Cli.out_fmt $
        get_which $ path_which $ set_clear $ create_only $
        no_preset $ force $ all $ vname $ B0b_cli.action_pos_args),
  Term.info "variant" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
  `Instance

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

   Permission to use, copy, modi<fy, and/or distribute this software for any
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
