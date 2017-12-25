(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let get_cache variant ~b0_dir ~cache_dir =
  let b0_dir = B0_dir.dir b0_dir in
  let dir = B0b_cli.get_cache_dir ~b0_dir ~cache_dir in
  let index_file = Variant.cache_index_path variant  in
  Cache.load ~index_file ~dir

let need_variant_creation ~b0_dir create =
  let dir = B0_dir.variant_dir b0_dir in
  (Variant.list_empty dir >>| function empty -> Ok (empty || create))
  |> Log.on_error_msg ~use:(fun _ -> Error `Some_error)

let create_variant ~b0_dir variant scheme =
  let scheme = B0b_cli.find_variant_scheme_name ~cli:scheme b0_dir in
  let log = Log.Error in
  match B0b_cli.get_variant_scheme ~log scheme with
  | Error _ as e -> e
  | Ok scheme ->
      (* If B0_VARIANT is defined here we are using its name, ideally
         we'd like not to but due to cmdliner limitations we cannot
         know if variant was spec'd on the cli or in the env var. *)
      B0b_cli.variant_create
        ~log ~b0_dir ~name:variant ~scheme ~preset:true ~make_default:true

let get_variant ~log ~b0_dir ~create variant scheme =
  need_variant_creation ~b0_dir create >>= function
  | true -> create_variant ~b0_dir variant scheme
  | false ->
      let variant = B0b_cli.find_variant_name ~cli:variant b0_dir in
      let dir = B0_dir.variant_dir b0_dir in
      B0b_cli.need_variant_name ~log variant >>= function name ->
      match B0b_cli.load_variant ~log ~dir name with
      | Error _ as error -> error
      | Ok (Ok v) -> Ok v
      | Ok Error _ ->
          Log.err (fun m -> m "TODO unknown scheme cannot run");
          Error `Some_error

let proxy_conf_of_setup setup v =
  let root_dir = match Def.Loc.get_root () (* FIXME api *) with
  | None -> assert false | Some root -> root
  in
  let b0_dir = B0_dir.dir (Driver.b0_dir setup) in
  let variant_dir = Variant.path v in
  Variant.Scheme.proxy_conf ~root_dir ~b0_dir ~variant_dir ()

let build_variant setup v cache ctrl units =
  let log = Log.Error in
  let scheme = Variant.scheme v in
  match Variant.Scheme.kind scheme with
  | `Proxy p ->
      let conf = proxy_conf_of_setup setup v in
      begin match
        Log.app (fun m -> m "Proxy build %a" Variant.Scheme.pp_name scheme);
        Variant.Scheme.proxy_run p conf Cmd.(v "b0") (* FIXME for now *)
      with
      | Error (`Msg msg) ->
          Log.err (fun m -> m "proxy: %s" msg); Ok `Some_error
      | Ok (`Signaled _ as s) ->
          Log.err (fun m -> m "proxy: %a" OS.Cmd.pp_status s); Ok `Some_error
      | Ok (`Exited c) -> Ok (`Code c)
      end
  | `Direct d ->
      let build_dir = Variant.build_path v in
      let outcome = Variant.outcome_path v in
      Log.time (fun _ m -> m "Reading build metadata and building")
      (fun () ->
        let prev_outcome = OS.File.exists outcome >>= function
          | false -> Ok None
          | true -> Outcome.read outcome >>| fun o -> Some o
        in
        let fmeta = Fpath.Meta_map.empty in
        Log.info (fun m -> m "Building variant %a" Variant.pp_name v);
        prev_outcome
        >>= fun prev_outcome -> B0b_cli.variant_load_conf ~log v
        >>= fun conf -> Variant.Scheme.direct_env d ()
        >>= fun env ->
        Build.create ?prev_outcome cache ctrl env conf fmeta ~dir:build_dir
          ~universe:units units
        >>= fun b -> Build.start b; Build.finish b
        >>= fun () -> Ok b) ()
      >>= fun b ->
      Log.time (fun _ m -> m "Writing build metadata")
        (fun () ->
           Outcome.write outcome (Build.outcome b)
           >>= fun () -> Cache.save (Build.cache b)
           >>= fun () ->
           Ok (B0b_cli.variant_save_conf ~log v (Build.stored_conf b))) ()

(* Units *)

let find_units pkgs units =
  let find_units units = B0b_cli.find_units ~log:Log.Error units in
  match pkgs with
  | [] -> find_units units
  | pkgs ->
      let pkgs, err = B0b_cli.find_pkgs ~log:Log.Error pkgs in
      match err with
      | `Unknown_name as e -> [], e
      | `Ok ->
          let units, err = match units with
          | [] -> [], `Ok
          | us -> find_units us
          in
          List.rev_append (B0b_cli.units_of_pkgs pkgs) units, err

(* Action *)

let build ctrl cache_dir create variant scheme units pkgs setup =
  match Driver.exec setup with
  | `Driver -> Cli.no_description_found ()
  | `Instance ->
      let b0_dir = Driver.b0_dir setup in
      begin
        let log = Log.Error in
        match get_variant ~log ~b0_dir ~create variant scheme with
        | Error exit -> Ok exit
        | Ok v ->
            let units, err = find_units pkgs units in
            match err with
            | `Unknown_name as e -> Ok e
            | `Ok when units = [] ->
                Log.err (fun m -> m "No build unit found.");
                Ok `Some_error
            | `Ok ->
                get_cache v ~b0_dir ~cache_dir
                >>= fun cache -> build_variant setup v cache ctrl units
      end
      |> B0b_cli.to_exit_code |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let create_variant =
  let doc = "Create a new variant and make it default. If specified, options
             $(b,--variant) and $(b,--scheme) are used to specify
             the variant name and scheme, otherwise creation follows the
             conventions of $(mname) $(b,variant create)."
  in
  Arg.(value & flag & info ["c"; "create-variant"] ~doc)

let units = B0b_cli.units ~doc:"Build unit $(docv) (and deps)."
let pkgs = B0b_cli.pkgs ~doc:"Build units of package $(docv) (and deps)."

let doc = "Run the build"
let sdocs = Manpage.s_common_options
let exits =
  Term.exit_info 1 ~doc:"on build failure" ::
  Term.exit_info 2 ~doc:"on empty builds" :: Cli.driver_default_exits

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command runs the build.";
    `P "If no variant exists, one is automatically created and made default
        using the variant scheme and made default.";
    `S Manpage.s_common_options;
    `S Cli.s_driver_opts;
    `S Manpage.s_environment;
    (* FIXME add that to all pages ? *)
    Manpage.s_environment_intro;
    `I ("$(b,B0_FORCE_)$(i,VAR)", "Forces the environment variable $(i,VAR) to
        be present with the given value in the environment of all programs
        run in the build.");
    `I ("$(b,B0_C_)$(i,KEY)", "Sets the configuration key $(i,KEY) to the
         given value for the program run.");
    `I ("$(b,B0_HOST_C_)$(i,KEY)", "Sets the build host configuration key
         $(i,KEY) to the given value for the program run."); ]

let cmd =
  Term.(pure build $ Cli.ctrl $ Cli.cache_dir $ create_variant $ Cli.variant $
        Cli.variant_scheme $ units $ pkgs),
  Term.info "build" ~doc ~sdocs ~exits ~man ~man_xrefs,
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
