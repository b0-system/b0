(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let is_exe f m = Fpath.Meta.flag B0_care.exe m

(* FIXME find a notation that specifies executable starting
   from unit names, we don't care about what leads us to
   the build dir. Maybe simply remove prefix to build dir. *)

let exec_name (f, _m) = Fpath.to_string f
let exec_exact_fname name (f, _m) = Fpath.filename f = name
let exec_matches name f =
  let f_base, ext = Fpath.split_ext f in
  let f_base = Fpath.to_string f_base in
  let f = Fpath.to_string f in
  String.(is_suffix ~affix:name f_base || is_suffix ~affix:name f)

let lookup_exec name o =
  let ambiguous l =
    R.error_msgf "Ambiguous matches for %s, could match any of %a"
      name Fmt.(list ~sep:(unit ", ") Fpath.pp) (List.map fst l)
  in
  let candidate f m = is_exe f m && exec_matches name f in
  let execs = Fpath.Map.filter candidate (Outcome.fpath_meta o) in
  match Fpath.Map.bindings execs with
  | [] -> R.error_msgf "Not matches for %s in build outcome." name
  | [f, _] ->
      (* FIXME we need to lookup which unit the executable is in to find
         the right build aim. Also FIXME we need a Env.execv
         this could allow to specify a context to run cross-compiled
         binaries. *)
      Ok (f, `Host_os)
  | l ->
      match List.find (exec_exact_fname name) l with
      | exception Not_found -> ambiguous l
      | (f, _) -> Ok (f, `Host_os)

let list_execs o =
  let execs = Fpath.Map.filter is_exe (Outcome.fpath_meta o) in
  let out exec = Log.app (fun m -> m "%s" (exec_name exec)) in
  List.iter out (Fpath.Map.bindings execs);
  Ok `Ok

let warn_unknown unknown noenv = match unknown with
| None -> noenv
| Some unknown ->
    Log.warn
      (fun m -> m
          "%s: unknown variant scheme, the current environment will be used,
           this may fail." unknown);
    true

let run_exec noenv v o exec args =
  lookup_exec exec o >>= fun (exec, aim) ->
  match Variant.Scheme.kind (Variant.scheme v) with
  | `Proxy p -> R.error_msgf "Proxy variant TODO"
  | `Direct d ->
      let env = match noenv with
      | true -> OS.Env.current ()
      | false -> Variant.Scheme.direct_env d () >>| fun env -> Env.env env aim
      in
      env >>= fun env ->
      let exec = Fpath.to_string exec in
      let env = Array.of_list @@ OS.Env.to_assignments env in
      let args = Array.of_list (exec :: args) in
      OS.Cmd.execve_raw exec args ~env >>= fun () -> assert false

(* Action *)

let run variant cmd noenv setup =
  begin
    let b0_dir = Driver.b0_dir setup in
    let log = Log.Error in
    match B0b_cli.get_variant ~log ~cli:variant ~b0_dir with
    | Error err -> Ok err
    | Ok load ->
        let unknown_scheme, variant = match load with
        | Ok v -> None, v
        | Error (`Unknown_scheme (s, v)) -> Some s, v
        in
        match B0b_cli.variant_get_outcome ~log variant with
        | Error exit -> Ok exit
        | Ok o ->
            match cmd with
            | `List -> list_execs o
            | `Run (exec, args) ->
                let noenv = warn_unknown unknown_scheme noenv in
                run_exec noenv variant o exec args
  end
  |> B0b_cli.to_exit_code |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let args =
  let doc = "Arguments given to the executable. Always needs to be specified
             after a -- token to make sure the command line options
             do not get interpreted by the $(tname) itself."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let exec =
  let doc = "Either 'list' to list the available executables or a unit
             name with a single executable or an executable name with
             or without its extension and possibly prefixed by a unit
             name. FIXME precisely define and document lookup procedure."
  in
  let docv = "EXEC" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let cmd =
  let err args =
    let args = String.concat ", " args in
    strf "too many arguments, don't know what to do with %s" args
  in
  let cmd exec args = match exec, args with
  | "list", [] -> `Ok `List
  | "list", args -> `Error (true, err args)
  | exec, args -> `Ok (`Run (exec, args))
  in
  Term.(ret (const cmd $ exec $ args))

let noenv =
  let doc = "Do not use the variant environment to run the tool. Run it in the
             process environment of $(mname)."
  in
  Arg.(value & flag & info ["no-env"] ~doc)

let doc = "Run build outcome executables."
let sdocs = Manpage.s_common_options

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) $(i,[OPTION])... list"; `Noblank;
    `P "$(mname) $(tname) $(i,[OPTION])... -- $(i,EXEC) [$(i,ARG)]...";
    `S Manpage.s_description;
    `P "The $(tname) command lists or runs executables of the current
        build outcome. These are the built files with B0_care.executable
        metadata.";
    `S Manpage.s_common_options;
    `S Cli.s_driver_opts;
    `S Manpage.s_environment; ]

let cmd =
  Term.(pure run $ Cli.variant $ cmd $ noenv),
  Term.info "run" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
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
