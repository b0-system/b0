(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver
open Cmdliner

(* Did you mean *)

let did_you_mean ?(pre = "Unknown") ?(post = "") ~log ~kind (n, hints) =
  match hints with
  | [] -> Log.maybe log (fun m -> m "%s %s '%s'%s." pre kind n post)
  | hints ->
      Log.maybe log
        (fun m -> m "@[%s %s '%s'%s.@ Did you mean %a ?@]"
            pre kind n post Format.pp_print_text (Cmdliner.Arg.doc_alts hints))

(* Exits *)

type exit =
  [ `Ok | `Unknown_name | `Undefined_key | `No_outcome | `Discover_error
  | `Some_error | `Cli_error | `No_default_variant | `No_variant | `Hook_error
  | `No_b0_dir | `Code of int ]

let exit_unknown_name = 1
let exit_undefined_key = 2
let exit_no_outcome = 3
let exit_discover_error = 4
let exit_no_default_variant = 5
let exit_missing_default_variant = 6
let exit_hook_error = 7
let exit_no_variant = 8
let exit_no_b0_dir = 9

let to_exit_code exit = exit >>| function
| `Ok -> 0
| `Code exit -> exit
| `Unknown_name -> exit_unknown_name
| `Undefined_key -> exit_undefined_key
| `No_outcome -> exit_no_outcome
| `Discover_error -> exit_discover_error
| `Some_error -> Cli.exit_some_error
| `Cli_error -> Cmdliner.Term.exit_status_cli_error
| `No_default_variant -> exit_no_default_variant
| `Missing_default_variant -> exit_missing_default_variant
| `Hook_error -> exit_hook_error
| `No_variant -> exit_no_variant
| `No_b0_dir -> exit_no_b0_dir

(* FIXME this function should not exist. *)
let join_exit = function Ok _ as v -> v | Error exit -> Ok exit

let exits =
  Term.exit_info exit_unknown_name
    ~doc:"on unknown named entity." ::
  Term.exit_info exit_undefined_key
    ~doc:"on an undefined stored key lookup." ::
  Term.exit_info exit_no_outcome
    ~doc:"on missing build outcome." ::
  Term.exit_info exit_discover_error
    ~doc:"on failing key discovery." ::
  Term.exit_info exit_no_default_variant
    ~doc:"on missing default variant." ::
  Term.exit_info exit_no_variant
    ~doc:"on missing variant." ::
  Term.exit_info exit_hook_error
    ~doc:"on hook error." ::
  Term.exit_info exit_no_b0_dir
    ~doc:"on missing b0 directory." ::
  Cli.driver_default_exits

(* Arguments *)

let err_too_many_args args =
  Log.err
    (fun m -> m "Too many arguments, don't know what to do with %s"
        (String.concat ", " (List.map Cmdliner.Arg.doc_quote args)))

let err_missing_arg ~kind =
  Log.err (fun m -> m "Missing %s argument" kind)

let synopsis_cmd_with_action =
  `Blocks
    [ `S Manpage.s_synopsis;
      `P "$(mname) $(tname) $(i,ACTION) [$(i,OPTION)]... [$(i,ARG)]..." ]

let action_arg actions =
  let alts = Arg.doc_alts_enum actions in
  let action = Arg.enum actions in
  let doc = strf "The action to perform. $(docv) must one of %s" alts in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let action_arg_with_default ~default actions =
  let alts = Arg.doc_alts_enum actions in
  let action = Arg.enum actions in
  let doc = strf "The action to perform. $(docv) must one of %s" alts in
  Arg.(value & pos 0 action default & info [] ~doc ~docv:"ACTION")

let action_pos_args =
  let doc = "Positional arguments for the action." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let no_pager =
  let doc = "Do not pipe the output into a pager. This automatically
             happens if the TERM environment variable is 'dumb' or undefined
             or if a structured serialization format is requested."
  in
  Arg.(value & flag & info ["no-pager"] ~doc)

(* B0 directory *)

let b0_dir_must_exist ~log b0_dir = match B0_dir.must_exist b0_dir with
| Error (`Msg msg) -> Log.maybe log (fun m -> m "%s" msg); Error `No_b0_dir
| Ok _ as v -> v

(* Variants *)

let msg_no_stored_variant : ('a, 'b) Log.msgf = fun m ->
  m "@[%a@]" Fmt.text "No default variant, set one with 'b0 variant set'"

let msg_no_effective_variant : ('a, 'b) Log.msgf = fun m ->
  m "@[%a@]" Fmt.text
    "No default variant, set one with 'b0 variant set' or define the \
     B0_VARIANT env var."

let msg_need_variant : ('a, 'b) Log.msgf = fun m ->
  m "@[%a@]" Fmt.text
    "No variant specified. Use the -w option or set a default variant \
     with 'b0 variant set' or define the B0_VARIANT environment variable"

let get_default_variant_name ~log which b0_dir = match which with
| `Stored ->
    begin match B0_dir.default_variant_name b0_dir with
    | None | Some "" ->
        Log.maybe log msg_no_stored_variant; Error `No_default_variant
    | Some v -> Ok v
    end
| `Effective -> (* FIXME abstract var name *)
    let name = match OS.Env.var "B0_VARIANT" with
    | None -> B0_dir.default_variant_name b0_dir
    | Some _ as v -> v
    in
    match name with
    | None | Some "" ->
        Log.maybe log msg_no_effective_variant;
        Error `No_default_variant
    | Some v -> Ok v

let find_variant_name ~cli b0_dir = match cli with
| Some _ as v -> v
| None -> B0_dir.default_variant_name b0_dir

let find_variant_scheme_name ~cli b0_dir = match cli with
| Some _ as v -> v
| None -> B0_dir.default_variant_scheme_name b0_dir

let get_variant_scheme ~log scheme = match scheme with
| None | Some "" -> Ok (Variant.Scheme.nop)
| Some s ->
    match Variant.Scheme.get_or_suggest s with
    | Ok v -> Ok v
    | Error sugg ->
        let kind = Variant.Scheme.value_kind in
        did_you_mean ~log:(Some Log.Error) ~kind (s, sugg);
        Error `Unknown_name

let variant_suggest_name ~dir scheme =
  Variant.list ~dir >>= fun ls ->
  let vs = List.rev_map Variant.of_load ls in
  let exists n = List.exists (fun v -> Variant.name v = n) vs in
  let reword _ = `Msg "Could not create a unique variant name." in
  let name = Variant.Scheme.name scheme in
  R.reword_error_msg reword (String.unique ~exists name)

let need_variant_name ~log = function
| None | Some "" -> Log.maybe log msg_need_variant; Error `No_variant
| Some n -> Ok n

let find_variant ~log ~dir = function
| None -> Log.maybe log (fun m -> m "No variant specified."); Ok None
| Some name ->
    match log with
    | None -> Variant.find ~dir name
    | Some level ->
        Variant.get_or_suggest ~dir name >>| function
        | Ok v -> Some v
        | Error sugg ->
            did_you_mean ~log ~kind:Variant.value_kind (name, sugg);
            None

let load_variant ~log ~dir name =
  begin
    Variant.get_or_suggest ~dir name >>| function
    | Ok _ as v -> v
    | Error sugg ->
        did_you_mean ~log ~kind:Variant.value_kind (name, sugg);
        Error `Unknown_name
  end
  |> Log.on_error_msg ~use:(fun _ -> Error `Some_error)

let get_variant ~log ~cli ~b0_dir =
  b0_dir_must_exist ~log b0_dir >>= fun () ->
  let variant = find_variant_name ~cli b0_dir in
  let dir = B0_dir.variant_dir b0_dir in
  need_variant_name ~log variant >>= fun variant ->
  load_variant ~log ~dir variant

let log_scheme_preset_errs ~log = function
| [] -> ()
| errs ->
    let pp_err ppf (k, (`Msg e)) =
      Fmt.pf ppf "%a: @[%a@]" Conf.Key.pp_name_str k Fmt.text e
    in
    Log.maybe
      log (fun m -> m "@[<v>Scheme preset errors:@,%a@]" (Fmt.list pp_err) errs)

let variant_create ~log ~b0_dir ~name ~scheme ~preset ~make_default =
  let dir = B0_dir.variant_dir b0_dir in
  let name = match name with
  | Some name -> Ok name
  | None -> variant_suggest_name ~dir scheme
  in
  begin
    name
    >>= fun name -> Variant.create ~preset ~dir name scheme
    >>= fun (v, errs) ->
    log_scheme_preset_errs ~log errs;
    begin match make_default with
    | false -> Ok ()
    | true -> B0_dir.set_default_variant_name b0_dir (Some name)
    end
    >>= fun () ->
    Log.app
      (fun m ->
         m "Created variant %a with scheme %a%s."
           Variant.pp_name v Variant.Scheme.pp_name scheme
           (if make_default then " and set as default" else ""));
    Ok (Ok v)
  end
  |> Log.on_error_msg ~use:(fun _ -> Error `Some_error)

let variant_load_conf ~log v =
  let conf_dec_err (k, err) = match err with
  | `Unknown ->
      Log.maybe log
        (fun m -> m "Decoding stored key %a: unknown key"
            Conf.Key.pp_name_str k)
  | `Msg msg ->
      Log.maybe log
        (fun m -> m "Decoding stored key %a: %s" Conf.Key.pp_name_str k msg)
  in
  let file = Variant.conf_path v in
  OS.File.exists file >>= function
  | false -> Ok (Conf.empty)
  | true ->
      Conf.load file >>= fun (conf, errs) ->
      List.iter conf_dec_err errs;
      Ok conf

let variant_load_outcome v =
  let file = Variant.outcome_path v in
  OS.File.exists file >>= function
  | false -> Ok None
  | true ->  Outcome.read file >>| fun o -> Some o

let msg_did_you_forget_to_build : ('a, 'b) Log.msgf = fun m ->
  m "@[%a@]" Fmt.text "No build outcome found. Did you forget to build ?"

let variant_get_outcome ~log v =
  let file = Variant.outcome_path v in
  begin OS.File.exists file >>= function
  | false -> Log.maybe log msg_did_you_forget_to_build; Ok (Error `No_outcome)
  | true -> Outcome.read file >>= fun o -> Ok (Ok o)
  end
  |> Log.on_error_msg ?level:log ~use:(fun _ -> Error `Some_error)


let variant_load_last_conf v =
  variant_load_outcome v >>| function
  | None -> None
  | Some o -> Some (Outcome.conf o)

let variant_save_conf ?(exit = `Ok) ~log v conf =
  let conf_enc_err (k, err) = match err with
  | `Msg msg ->
      Log.maybe log
        (fun m -> m "Encoding stored key %a: %s, not stored"
            Conf.Key.pp_name_str k msg)
  in
  begin
    let file = Variant.conf_path v in
    Conf.save conf file >>= fun errs ->
    List.iter conf_enc_err errs;
    Ok exit
  end
  |> B0.Log.on_error_msg ~use:(fun _ -> `Some_error)

(* Cache *)

let get_cache_dir ~b0_dir ~cache_dir = match cache_dir with
| None -> Fpath.(b0_dir // Cli.default_cache_dir)
| Some dir -> dir

(* Generic operations on named values *)

let find_named_values ~log ~kind ~list ~get_or_suggest = function
| [] -> list (), `Ok
| names ->
  let add (vs, exit) n = match get_or_suggest n with
  | Ok v -> v :: vs, exit
  | Error sugg -> did_you_mean ~log ~kind (n, sugg); vs, `Unknown_name
  in
  let names = List.sort_uniq String.compare names in
  let vs, exit = List.fold_left add ([], `Ok) names in
  List.rev vs, exit

let show_value ~short ~normal ~long out_fmt v =
  let out = match out_fmt with
  | `Normal -> normal | `Short -> short | `Long -> long
  in
  Log.app (fun m -> m "%a" out v)

let list_values_action ~find ~show out_fmt vs =
  let vs, exit = find vs in
  List.iter (show out_fmt) vs;
  Ok exit

let value_info_action ~list_action out_fmt vs =
  let out_fmt = match out_fmt with `Normal -> `Long | f -> f in
  list_action out_fmt vs

(* Showing keys *)

let show_key ?(ext = fun _ _ -> ()) =
  let short = Conf.Key.pp_name in
  let normal = Conf.Key.pp_synopsis in
  let long = Conf.Key.pp_info_ext ext in
  show_value ~short ~normal ~long

let show_variant ?(ext = fun _ _ -> ()) =
  let short = Variant.pp_name in
  let normal = Variant.pp_synopsis in
  let long = Variant.pp_info_ext ext in
  show_value ~short ~normal ~long

(* Editing *)

let edit_files fs = match OS.Env.var "EDITOR" with
| None -> R.error_msg "EDITOR environment variable undefined."
| Some editor ->
    Cmd.of_string editor
    >>= fun editor -> OS.Cmd.exists editor
    >>= function
    | false -> R.error_msgf "Editor %a not in search path" Cmd.pp editor
    | true ->
        OS.Cmd.(run_status Cmd.(editor %% of_values p fs)) >>= function
        | `Exited n | `Signaled n -> Ok n

let find_pager ~don't = match don't with
| true -> Ok None
| false ->
    match OS.Env.var "TERM" with
    | Some "dumb" | None -> Ok None
    | _ ->
        let cmds = ["less"; "more"] in
        let cmds = match OS.Env.var "PAGER" with
        | None -> cmds | Some c -> c :: cmds
        in
        let rec loop = function
        | [] -> Ok None
        | cmd :: cmds ->
            OS.Cmd.which (Cmd.v cmd) >>= function
            | Some _ as v  -> Ok v
            | None -> loop cmds
        in
        loop cmds

(* Units *)

let units ~doc =
  Arg.(value & opt_all string [] & info ["u"; "unit"] ~doc ~docv:"UNIT")

let find_units ~log names =
  let kind = Unit.value_kind in
  let list = Unit.list in
  let get_or_suggest = Unit.get_or_suggest in
  find_named_values ~log ~kind ~list ~get_or_suggest names

let show_unit =
  let short = Unit.pp_name in
  let normal = Unit.pp_synopsis in
  let long = Unit.pp_info in
  show_value ~short ~normal ~long

let units_of_pkgs pkgs =
  let pkgs = Pkg.Set.of_list pkgs in
  let mem_pkgs u = match Unit.pkg u with
  | None -> false | Some p -> Pkg.Set.mem p pkgs
  in
  List.filter mem_pkgs (Unit.list ())

(* Packages *)

let pkgs ~doc =
  Arg.(value & opt_all string [] & info ["p"; "pkg"] ~doc ~docv:"PKG")

let find_pkgs ~log names =
  let kind = Pkg.value_kind in
  let list = Pkg.list in
  let get_or_suggest = Pkg.get_or_suggest in
  find_named_values ~log ~kind ~list ~get_or_suggest names


let show_pkg_units ppf pkg =
  let in_pkg u = match Unit.pkg u with
  | None -> false | Some p -> Pkg.equal p pkg
  in
  let units = List.filter in_pkg (Unit.list ()) in
  Fmt.cut ppf ();
  Fmt.field "units" Fmt.(list ~sep:comma Unit.pp_name) ppf units;
  ()

let show_pkg =
  let short = Pkg.pp_name in
  let normal = Pkg.pp_synopsis in
  let long = Pkg.pp_info_ext show_pkg_units in
  show_value ~short ~normal ~long

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
