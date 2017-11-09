(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let pp_key_val k ppf v = (Conv.print @@ Conf.Key.conv k) ppf v

let show_binding kind out_fmt b = (* FIXME move the printers to Conf *)
  let short ppf (Conf.B (k, v)) = (pp_key_val k) ppf v in
  let normal ppf (Conf.B (tk, v)) =
    let k = Conf.Key.of_typed tk in
    Fmt.pf ppf "%a: @[%a@]" Conf.Key.pp_name k (pp_key_val tk) v
  in
  let long ppf (Conf.B (tk, v)) =
    let k = Conf.Key.of_typed tk in
    let ext ppf _ =
      Fmt.cut ppf ();
      Fmt.field kind (pp_key_val tk) ppf v;
    in
    (Conf.Key.pp_info_ext ext) ppf k
  in
  B0b_cli.show_value ~short ~normal ~long out_fmt b

let get_variant ~b0_dir variant =
  let dir = B0_dir.variant_dir b0_dir in
  B0b_cli.find_variant ~log:(Some Log.Warning) ~dir variant >>| function
  | None -> None
  | Some l -> Some (Variant.of_load l)

let get_variant_confs variant =
  let err_conf _ = R.msgf "Could not get variant configurations" in
  let log = Some Log.Error in
  let none = None, (None, None) in
  begin
    Lazy.force variant >>= function
    | None -> Ok none
    | Some v ->
        match Variant.Scheme.kind (Variant.scheme v) with
        | `Proxy p -> R.error_msgf "proxy scheme TODO"
        | `Direct d ->
            Variant.Scheme.direct_env d ()
            >>= fun env -> B0b_cli.variant_load_conf ~log v
            >>= fun conf -> B0b_cli.variant_load_last_conf v
            >>= fun last_conf ->
            Ok (Some env, (last_conf, Some conf))
  end
  |> R.reword_error_msg ~replace:false err_conf
  |> Log.on_error_msg ~use:(fun _ -> none)

(* Looking up keys *)

let no_keys key_sets keys = match key_sets, keys with
| `Key_sets [], [] -> true | _ -> false

let get_keys ?no_keys:(none = `Key_sets []) confs key_sets keys =
  let log = Some Log.Error in
  let conf_keys (ks, exit) c = List.rev_append (Conf.keys c) ks, exit in
  let group_keys (ks, exit) g = match Conf.Group.get_or_suggest g with
  | Ok g -> (List.rev_append (Conf.Group.keys g) ks), exit
  | Error sugg ->
      let kind = Conf.Group.value_kind in
      B0b_cli.did_you_mean ~log ~kind (g, sugg); (ks, `Unknown_name)
  in
  let preset_keys (ks, exit) p = match Conf.Preset.get_or_suggest p with
  | Ok p -> (List.rev_append (Conf.Preset.keys p) ks), exit
  | Error sugg ->
      let kind = Conf.Preset.value_kind in
      B0b_cli.did_you_mean ~log ~kind (p, sugg); (ks, `Unknown_name)
  in
  let stored_keys (ks, exit as acc) last = match last with
  | Some c -> conf_keys acc c
  | None ->
      Log.err (fun m -> m "Cannot lookup stored keys: No build variant.");
      (ks, `No_variant)
  in
  let last_keys (ks, exit as acc) last = match last with
  | Some c -> conf_keys acc c
  | None ->
      Log.err (fun m -> m "Cannot lookup last keys: No build outcome.");
      (ks, `No_outcome)
  in
  let add_key (ks, exit) k = match Conf.Key.get_or_suggest k with
  | Ok k -> k :: ks, exit
  | Error sugg ->
      let kind = Conf.Key.value_kind in
      B0b_cli.did_you_mean ~log ~kind (k, sugg); (ks, `Unknown_name)
  in
  let key_sets = if no_keys key_sets keys then none else key_sets in
  match key_sets with
  | `All -> Conf.Key.list (), `Ok
  | `Key_sets specs ->
      let add_spec acc = function
      | `Group g -> group_keys acc g
      | `Preset p -> preset_keys acc p
      | `Stored -> stored_keys acc (snd @@ snd (Lazy.force confs))
      | `Last -> last_keys acc (fst @@ snd (Lazy.force confs))
      in
      let acc = List.fold_left add_spec ([], `Ok) specs in
      let (keys, exit) = List.fold_left add_key acc keys in
      List.sort_uniq Conf.Key.compare_by_name keys, exit

(* Getting key values *)

let get_def (bs, exit) k = function
| Ok v -> Conf.B (k, v) :: bs, exit
| Error (`Msg e) ->
    let k = Conf.Key.of_typed k in
    Log.err (fun m -> m "Key %a discovery: %s" Conf.Key.pp_name k e);
    bs, `Discover_error

let get_stored_conf confs = match snd @@ snd (Lazy.force confs) with
| None -> Conf.empty
| Some c -> c

let get_default confs key_sets keys =
  let no_keys = `Key_sets [`Stored] in
  let keys, exit = get_keys ~no_keys confs key_sets keys in
  let stored = get_stored_conf confs in
  let env, aim = match fst (Lazy.force confs) with
  | None -> assert false
  | Some env -> env, `Host_os (* FIXME *)
  in
  let add acc (Conf.Key.V k) =
    get_def acc k (Conf.get_default env aim k stored)
  in
  List.fold_left add ([], exit) (List.rev keys)

let get_effective confs key_sets keys =
  let no_keys = `Key_sets [`Stored] in
  let keys, exit = get_keys ~no_keys confs key_sets keys in
  let stored = get_stored_conf confs in
  let env, aim = match fst (Lazy.force confs) with
  | None -> assert false
  | Some env -> env, `Host_os (* FIXME *)
  in
  let add acc (Conf.Key.V k) =
    get_def acc k (Conf.get_effective env aim k stored >>| snd)
  in
  List.fold_left add ([], exit) (List.rev keys)

let get_stored confs key_sets keys =
  let no_keys = `Key_sets [`Stored] in
  let keys, exit = get_keys ~no_keys confs key_sets keys in
  let stored = get_stored_conf confs in
  let add (bs, exit) (Conf.Key.V k) =  match Conf.get_or_suggest k stored with
  | Ok v -> (Conf.B (k, v) :: bs), exit
  | Error sugg ->
      let n = Conf.Key.(name (of_typed k)) in
      let log = Some Log.Error in
      let kind = Conf.Key.value_kind in
      B0b_cli.did_you_mean ~log ~pre:"No stored" ~kind (n, sugg);
      bs, `Undefined_key
  in
  List.fold_left add ([], exit) (List.rev keys)

let get_preset preset confs key_sets keys =
  let log = Some Log.Error in
  match Conf.Preset.get_or_suggest preset with
  | Error sugg ->
      B0b_cli.did_you_mean ~log ~kind:Conf.Preset.value_kind (preset, sugg);
      [], `Unknown_name
  | Ok p ->
      let no_keys = `Key_sets [`Preset preset ] in
      let keys, exit = get_keys ~no_keys confs key_sets keys in
      let stored = get_stored_conf confs in
      let env, aim = match fst (Lazy.force confs) with
      | None -> assert false
      | Some env -> env, `Host_os (* FIXME *)
      in
      let add (bs, exit) (Conf.Key.V k) =
        let n = Conf.Key.(name (of_typed k)) in
        match Conf.Preset.get_or_suggest_def n p with
        | Error sugg ->
            let post = strf " in preset %s" preset in
            B0b_cli.did_you_mean ~log ~kind:Conf.Key.value_kind ~post (n, sugg);
            bs, `Unknown_name
        | Ok def ->
            let Conf.Preset.B (k, def_v) = Conf.Preset.def_binding def in
            match Conf.value env aim k stored def_v with
            | Ok v -> Conf.B (k, v) :: bs, exit
            | Error (`Msg e) ->
                Log.err
                  (fun m -> m "Discovery of preset value for %a: %s"
                      Conf.Key.pp_name Conf.Key.(of_typed k) e);
                bs, `Discover_error
      in
      List.fold_left add ([], exit) (List.rev keys)

let get_last confs key_sets keys = match fst @@ snd (Lazy.force confs) with
| None ->
    Log.err (fun m -> m "No build outcome found.");
    [], (`No_outcome)
| Some c ->
    let no_keys = `Key_sets [`Last] in
    let keys, exit = get_keys ~no_keys confs key_sets keys in
    let add (bs, exit) (Conf.Key.V k) = match Conf.get_or_suggest k c with
    | Error sugg ->
        let kind, post = Conf.Key.value_kind, " in last configuration" in
        let log = Some Log.Error in
        let k = Conf.Key.(name (of_typed k)) in
        B0b_cli.did_you_mean ~log ~kind ~post (k, sugg); (bs, `Unknown_name)
      | Ok v -> Conf.B (k, v) :: bs, exit
    in
    List.fold_left add ([], exit) (List.rev keys)

(* Parse cli key value bindings *)

let parse_bindings args =
  let rec loop bs exit = function
  | [] -> if exit <> `Ok then [], exit else List.rev bs, exit
  | k :: v :: args ->
      begin match Conf.Key.get_or_suggest k with
      | Ok (Conf.Key.V tk as k) ->
          begin match Conv.parse (Conf.Key.conv tk) v with
          | Ok v -> loop (Conf.B (tk, v) :: bs) exit args
          | Error (`Msg e) ->
              Log.err (fun m -> m "Key %a: %s" Conf.Key.pp_name k e);
              loop bs `Cli_error args
          end
      | Error sugg ->
          let log = Some Log.Error in
          B0b_cli.did_you_mean ~log ~kind:Conf.Key.value_kind (k, sugg);
          (bs, `Unknown_name)
      end
  | [k] ->
      Log.err (fun m -> m "Missing value for key %a." Conf.Key.pp_name_str k);
      [], `Cli_error
  in
  loop [] `Ok args

(* Actions *)

let list_action out_fmt confs key_sets keys =
  let keys, exit = get_keys ~no_keys:`All confs key_sets keys in
  List.iter (B0b_cli.show_key out_fmt) keys;
  Ok exit

let get_action out_fmt confs get key_sets keys =
  let kind, (bindings, exit) = match get with
  | `Default -> "default", get_default confs key_sets keys
  | `Effective -> "effective", get_effective confs key_sets keys
  | `Stored -> "stored", get_stored confs key_sets keys
  | `Preset p -> "preset", get_preset p confs key_sets keys
  | `Last -> "last", get_last confs key_sets keys
  in
  List.iter (show_binding kind out_fmt) bindings;
  Ok exit

let set_action confs set key_sets args =
  let bindings, exit = match set with
  | `Default -> get_default confs key_sets args
  | `Effective -> get_effective confs key_sets args
  | `Preset p -> get_preset p confs key_sets args
  | `Last -> get_last confs key_sets args
  | `Arg_val -> parse_bindings args
  in
  let set conf (Conf.B (k, v)) = Conf.add k v conf in
  let stored = get_stored_conf confs in
  Ok (List.fold_left set stored bindings, exit)

let clear_action confs key_sets keys = match no_keys key_sets keys with
| true ->
    Log.err (fun m -> m "No key specified.");
    Error `Cli_error
| false ->
    let keys, exit = get_keys confs key_sets keys in
    let rem_key conf (Conf.Key.V k) = Conf.rem k conf in
    let stored = get_stored_conf confs in
    Ok (List.fold_left rem_key stored (List.rev keys), exit)

let key action variant key_sets out_fmt pos_args setup =
  begin
    let b0_dir = Driver.b0_dir setup in
    let variant = B0b_cli.find_variant_name ~cli:variant b0_dir in
    let variant = lazy (get_variant ~b0_dir variant) in
    let confs = lazy (get_variant_confs variant) in
    let log = Some Log.Error in
    match action with
    | `List -> list_action out_fmt confs key_sets pos_args
    | `Info ->
        let out_fmt = match out_fmt with `Normal -> `Long | f -> f in
        list_action out_fmt confs key_sets pos_args
    | `Get get -> get_action out_fmt confs get key_sets pos_args
    | `Set set ->
        begin match set_action confs set key_sets pos_args with
        | Error exit -> Ok exit
        | Ok (conf, exit) ->
            Lazy.force variant >>= function
            | None -> assert false (* FIXME *)
            | Some v -> Ok (B0b_cli.variant_save_conf ~log v conf ~exit)
        end
    | `Clear ->
        begin match clear_action confs key_sets pos_args with
        | Error exit -> Ok exit
        | Ok (conf, exit) ->
            Lazy.force variant >>= function
            | None -> assert false (* FIXME *)
            | Some v -> Ok (B0b_cli.variant_save_conf ~log v conf ~exit)
        end
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let specify_key_value_get_docs = "SPECIFYING KEY VALUES TO GET"
let get =
  let docs = specify_key_value_get_docs in
  let stored =
    let doc = "Get the stored configuration key value (if any)." in
    Arg.info ["stored"] ~doc ~docs
  in
  let default =
    let doc = "Get the default key value (constant or discovered)." in
    Arg.info ["default"] ~doc ~docs
  in
  let effective =
    let doc = "Get the effective key value (default)." in
    Arg.info ["effective"] ~doc ~docs
  in
  let last =
    let doc = "Get the last configuration key value (if any)." in
    Arg.info ["l"; "last"] ~doc ~docs
  in
  let flags =
    [ Some `Effective, effective; Some `Stored, stored; Some `Default, default;
      Some `Last, last]
  in
  let from_other = Arg.(value & vflag None flags) in
  let from_preset =
    let doc = "Get key value in preset $(docv)." in
    let docv = "PRESET" in
    Arg.(value & opt (some string) None & info ["preset"] ~doc ~docs ~docv)
  in
  let get from_other from_preset = match from_other, from_preset with
  | Some v, None -> `Ok (Some v)
  | None, Some p -> `Ok (Some (`Preset p))
  | None, None -> `Ok None
  | Some _, Some _ ->
      `Error (true, "cannot use `--preset` with other 'get' action options.")
  in
  Term.(ret (const get $ from_other $ from_preset))

let specify_key_value_set_docs = "SPECIFYING KEY VALUES TO SET"
let set =
  let docs = specify_key_value_set_docs in
  let to_other =
    let default =
      let doc = "Set keys to their default value." in
      Arg.info ["to-default"] ~doc ~docs
    in
    let effective =
      let doc = "Set keys to their effective value." in
      Arg.info ["to-effective"] ~doc ~docs
    in
    let last =
      let doc = "Set keys to their value in the last configuration." in
      Arg.info ["to-last"] ~doc ~docs
    in
    let flags =
      [ Some `Default, default; Some `Effective, effective; Some `Last, last]
    in
    Arg.(value & vflag None flags)
  in
  let to_preset =
    let doc = "Set keys to their value in preset $(docv)." in
    let docv = "PRESET" in
    Arg.(value & opt (some string) None & info ["to-preset"] ~doc ~docs ~docv)
  in
  let set to_other to_preset = match to_other, to_preset with
  | Some v, None -> `Ok (Some v)
  | None, Some p -> `Ok (Some (`Preset p))
  | None, None -> `Ok None
  | Some _, Some _ ->
      `Error (true, "cannot use '--to-preset' with other 'set' action options.")
  in
  Term.(ret (const set $ to_other $ to_preset))

let action =
  let action =
    B0b_cli.action_arg
      [ ("list", `List); ("info", `Info); ("get", `Get); ("set", `Set);
        ("clear", `Clear) ]
  in
  let act action get set = match action, get, set with
  | `Get, g, None -> `Ok (`Get (match g with None -> `Effective | Some f -> f))
  | `Set, None, s -> `Ok (`Set (match s with None -> `Arg_val | Some s -> s))
  | (`List | `Info | `Clear as act), None, None -> `Ok act
  | _, _, Some _ ->
      `Error (true, "cannot use 'set' action options with this action")
  | _, Some _, _ ->
      `Error (true, "cannot use 'get' action options with this action")
  in
  Term.(ret (const act $ action $ get $ set))

let specify_key_sets_docs = "SPECIFYING SETS OF KEYS"
let key_sets =
  let docs = specify_key_sets_docs in
  let all_keys =
    let doc = "Add all keys known to the description to the set." in
    Arg.(value & flag & info ["a"; "all-keys"] ~doc ~docs)
  in
  let group_keys =
    let doc = "Add keys of group $(docv) to the set." in
    let docv = "GROUP" in
    Arg.(value & opt_all string [] & info ["g"; "group-keys"] ~doc ~docs ~docv)
  in
  let preset_keys =
    let doc = "Add keys of preset $(docv) to the set." in
    let docv = "PRESET" in
    Arg.(value & opt_all string [] & info ["preset-keys"] ~doc ~docs ~docv)
  in
  let stored_keys =
    let doc = "Add keys stored in the stored configuration to the set." in
    Arg.(value & flag & info ["stored-keys"] ~doc ~docs)
  in
  let last_keys =
    let doc = "Add keys of the last configuration to the set." in
    Arg.(value & flag & info ["last-keys"] ~doc ~docs)
  in
  let key_sets all_keys group_keys preset_keys stored_keys last_keys =
    if all_keys then `All else
    let ks = [] in
    let ks = List.fold_left (fun l g -> `Group g :: l) ks group_keys in
    let ks = List.fold_left (fun l p -> `Preset p :: l) ks preset_keys in
    let ks = if stored_keys then `Stored :: ks else ks in
    let ks = if last_keys then `Last :: ks else ks in
    `Key_sets ks
  in
  Term.(const key_sets $ all_keys $ group_keys $ preset_keys $ stored_keys $
        last_keys)

let doc = "Operate on configuration keys"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "build"; `Cmd "group"; `Cmd "preset" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command gets, sets and clears configuration keys.
        By default operates on the current build variant or deployment.";
    `S "ACTIONS";
    `P "Sets of keys can be specified using options instead of
        explicitely naming them as positional arguments, see next section.";
    `I ("$(b,list) [$(i,KEY)]...",
        "List all or given keys.");
    `I ("$(b,info) [$(i,KEY)]...",
        "Show information about all or given keys.");
    `I ("$(b,get) [$(b,--default) | $(b,--effective) | $(b,--stored) | \
         $(b,--preset) $(i,PRESET) | $(b,--last)] \
         [$(i,KEY)]...",
        "Get the default, effective, stored, preset or last value of given
         keys. The default key set is $(b,--stored-keys), except with option
         $(b,--preset) where $(b,--preset-keys) $(i,PRESET) is used and option
         $(b,--last) where $(b,--last-keys) is used.");
    `I ("$(b,set) $(i,KEY) $(i,VAL)...",
        "Set the $(i,KEY) to $(i,VAL) in the stored configuration.
         Key value pairs can be repeated.");
    `I ("$(b,set) [$(b,--to-default) | $(b,--to-effective) | \
         $(b,--to-preset) $(i,PRESET) | $(b,--to-last)] [$(i,KEY)]...",
        "Set given keys in the stored configuration to their default, effective,
         preset or last value. By default the effective value is used.
         The default key set is $(b,--stored-keys), except with option
         $(b,--to-preset) where $(b,--preset-keys) $(i,PRESET) is used and
         option $(b,--last) where $(b,--last-keys) is used.");
    `I ("$(b,clear) [$(i,-n)] [$(i,KEY)]...",
        "Clears the value of given keys in the stored configuration.
         There is no default key set.");
    `S specify_key_sets_docs;
    `P "Actions that take a set of keys as argument can use these
        options and arguments to specify the key set:";
    `I ("$(i,KEY) positional argument", "Adds $(i,KEY) to the set.");
    `S specify_key_value_get_docs;
    `P "By default the action $(b,get) returns the effective value of
        a key. The following mutually exclusive options allow to lookup
        the value somewhere else:";
    `S specify_key_value_set_docs;
    `P "By default the action $(b,set) sets a value specified on the command
        line. The following mutually exclusive options allow to lookup
        the value to set somewhere else:";
    B0_driver.Cli.common_man ]

let cmd =
  Term.(pure key $ action $ Cli.variant $ key_sets $ Cli.out_fmt $
        B0b_cli.action_pos_args),
  Term.info "key" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
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
