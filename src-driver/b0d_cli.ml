(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner
open B0

(* Default dirs *)

let default_driver_dir = Fpath.v "i"
let default_cache_dir = Fpath.v "cache"
let default_b0_dir = Fpath.v "_b0"

(* Exit codes *)

let exit_no_description = 121
let exit_driver_setup_err = 122
let exit_some_error = 123

let driver_default_exits =
  Term.exit_info
    exit_no_description ~doc:"on missing build description" ::
  Term.exit_info
    exit_driver_setup_err ~doc:"on driver setup error" ::
  Term.exit_info
    exit_some_error ~doc:"on indiscriminate errors reported on stderr." ::
  Term.default_exits

let handle_error = function
| Ok 0 -> if Log.err_count () > 0 then exit_some_error else 0
| Ok n -> n
| Error _ as r -> Log.on_error_msg ~use:(fun _ -> exit_some_error) r

let no_description_found () =
  Log.err (fun m -> m "No b0 description found.");
  exit_no_description

(* Argument converters *)

let path_arg = Cmdliner.Arg.conv Fpath.(of_string, pp)

(* Arguments *)

let root =
  let doc = "Use $(docv) as the root directory. $(docv) itself doesn't
             need to contain a description file."
  in
  let env = Arg.env_var "B0_ROOT" in
  let docv = "DIR" in
  let none = "automatically determined" in
  Arg.(value & opt (some ~none path_arg) None &
       info ["root"] ~env ~doc ~docs:Manpage.s_common_options ~docv)

let cwd =
  let doc = "Change to directory $(docv) before doing anything. Relative
             path command line arguments are interpreted relative to this
             directory."
  in
  let docv = "DIR" in
  Arg.(value & opt (some path_arg) None &
       info ["C"] ~doc ~docs:Manpage.s_common_options ~docv)

let b0_dir =
  let doc = "Use $(docv) for the b0 directory." in
  let docs = Cmdliner.Manpage.s_common_options in
  let env = Arg.env_var "B0_DIR" in
  let none = Fpath.to_string default_b0_dir in
  Arg.(value & opt (some ~none path_arg) None &
       info ["b0-dir"] ~env ~doc ~docs ~docv:"DIR")

let color =
  let docs = Manpage.s_common_options in
  let enum = ["auto", None; "always", Some Tty.Ansi; "never", Some Tty.None] in
  let color = Arg.enum enum in
  let env = Arg.env_var "B0_COLOR" in
  let enum_alts = Arg.doc_alts_enum enum in
  let doc = strf "Colorize the output. $(docv) must be %s." enum_alts in
  let docv = "WHEN" in
  Arg.(value & opt color None & info ["color"] ~env ~doc ~docv ~docs)

let verbosity =
  let docs = Manpage.s_common_options in
  let env = Arg.env_var "B0_VERBOSITY" in
  let vopts =
    let doc = "Increase verbosity. Repeatable, but more than twice does
               not bring more."
    in
    Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docs)
  in
  let verbosity =
    let enum =
      [ "warning", None; (* Hack for the option's absent rendering *)
        "quiet", Some None;
        "error", Some (Some Log.Error);
        "warning", Some (Some Log.Warning);
        "info", Some (Some Log.Info);
        "debug", Some (Some Log.Debug); ]
    in
    let log_level = Arg.enum enum in
    let enum_alts = Arg.doc_alts_enum List.(tl enum) in
    let doc = strf "Be more or less verbose. $(docv) must be %s. Takes over
                    $(b,-v)." enum_alts
    in
    Arg.(value & opt log_level None &
         info ["verbosity"] ~env ~docv:"LEVEL" ~doc ~docs)
  in
  let quiet =
    let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
  in
  let choose quiet verbosity vopts =
    if quiet then None else match verbosity with
    | Some verbosity -> verbosity
    | None ->
        match List.length vopts with
        | 0 -> Some Log.Warning
        | 1 -> Some Log.Info
        | n -> Some Log.Debug
  in
  Term.(const choose $ quiet $ verbosity $ vopts)

(* More common arguments *)

let cache_dir =
  let doc = "Use $(docv) for the cache directory." in
  let docs = Cmdliner.Manpage.s_common_options in
  let env = Arg.env_var "B0_CACHE_DIR" in
  let none = strf "$(b,%a) in b0 directory" Fpath.pp default_cache_dir in
  Arg.(value & opt (some ~none path_arg) None &
       info ["cache-dir"] ~env ~doc ~docs ~docv:"DIR" )

let cache_index =
  let doc = "Use $(docv) for the cache index." in
  let docs = Cmdliner.Manpage.s_common_options in
  let none = strf "file index in variant directory" in
  Arg.(value & opt (some ~none path_arg) None &
       info ["cache-index"] ~doc ~docs ~docv:"FILE")

let variant_scheme_env = "B0_VARIANT_SCHEME"
let variant_scheme =
  let doc = "Use variant scheme $(docv) on variant creation." in
  let docv = "SCHEME" in
  let env = Arg.env_var variant_scheme_env in
  let none = "description default" in
  Arg.(value & opt (some ~none string) None &
       info ["s"; "scheme"] ~env ~doc ~docv)

let variant_env = "B0_VARIANT"
let variant =
  let doc = "Act on build variant $(docv)." in
  let docs = Cmdliner.Manpage.s_common_options in
  let docv = "VARIANT" in
  let env = Arg.env_var variant_env in
  let none = "value of $(b,b0 variant default get)" in
  Arg.(value & opt (some ~none string) None &
       info ["w"; "variant"] ~env ~doc ~docs ~docv)

(* Output format options *)

type out_fmt = [ `Normal | `Short | `Long ]
let out_fmt_docs = "OUTPUT FORMAT OPTIONS"
let out_fmt =
  let docs = out_fmt_docs in
  let short =
    let doc = "Short output. Line-based output with only relevant data." in
    Arg.info ["short"] ~doc ~docs
  in
  let long =
    let doc = "Long output. Outputs as much information as possible." in
    Arg.info ["long"] ~doc ~docs
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])

(* Arguments *)

let file_kind =
  let kind = [
    `All, Arg.info ["all"] ~doc:"Select all files (default).";
    `Roots, Arg.info ["roots"] ~doc:"Select root (non-built) files.";
    `Built, Arg.info ["built"] ~doc:"Select built files."; ]
  in
  Arg.(value & vflag `All kind)

let ctrl =
  let jobs =
    let env = Arg.env_var "B0_JOBS" in
    let doc = "Maximal number of commands to spawn concurrently." in
    let docv = "COUNT" in
    Arg.(value & opt (some int) None & info ["j"; "jobs"] ~env ~doc ~docv)
  in
  let ctrl jobs = Build.ctrl ?max_spawn:jobs () in
  Term.(const ctrl $ jobs)

(* Driver arguments *)

let s_driver_opts = "OPTIONS FOR COMPILING DRIVER INSTANCE"
let docs = s_driver_opts

(* TODO document B0_DRIVER_LIBDIR *)

let driver_dir =
  let doc = "Use $(docv) for building and storing drivers instances." in
  let env = Arg.env_var "B0_DRIVER_DIR" in
  Arg.(value & opt (some ~none:"d in b0 directory" path_arg) None &
       info ["d-dir"] ~env ~doc ~docs ~docv:"DIR")

let driver_force =
  let doc = "Force driver instance to recompile (if instance is needed)." in
  Arg.(value & flag & info ["d-force"] ~doc ~docs)

let driver_only =
  let doc = "Do not run the instance, only compile it and stop." in
  Arg.(value & flag & info ["d-only"] ~doc ~docs)

let driver_trust =
  let doc = "Do not verify the instance validity, trust it to be up-to-date." in
  Arg.(value & flag & info ["d-trust"] ~doc ~docs)

let driver_ocamlc =
  let doc = "Use $(docv) compiling the driver instance to byte code." in
  let env = Arg.env_var "B0_D_OCAMLC" in
  Arg.(value & opt (some ~none:"ocamlc" string) None &
       info ["d-ocamlc"] ~env ~doc ~docs ~docv:"BIN")

let driver_ocamlopt =
  let doc = "Use $(docv) compiling the driver instance to native code." in
  let env = Arg.env_var "B0_D_OCAMLOPT" in
  Arg.(value & opt (some ~none:"ocamlopt.opt" string) None &
       info ["d-ocamlopt"] ~env ~doc ~docs ~docv:"BIN")

let driver_compile_kind =
  let kind = ["byte", `Byte; "native", `Native; "auto", `Auto; ] in
  let alts = Arg.doc_alts_enum kind in
  let doc = strf "The kind of compilation, must be one of %s." alts in
  let kind = Arg.enum kind in
  let env = Arg.env_var "B0_D_COMPILE_KIND" in
  Arg.(value & opt (some ~none:"auto" kind) None &
       info ["d-compile-kind"] ~env ~doc ~docs ~docv:"KIND")

let driver_compile =
  let doc = "Add $(docv) to the compilation flags of the instance. For a flag
             with an argument use twice, e.g. $(opt) -I $(opt) dir."
  in
  Arg.(value & opt_all string [] & info ["d-compile"] ~doc ~docs ~docv:"ARG")

let driver_link =
  let doc = "Add $(docv) to the link flags of the instance. For a flag
             with an argument use twice e.g. $(opt) -cclib $(opt) opt."
  in
  Arg.(value & opt_all string [] & info ["d-link"] ~doc ~docs ~docv:"ARG")

let common_man =
  `Blocks [
    `S Manpage.s_common_options;
    `S s_driver_opts;
    `S Manpage.s_see_also;
    `P "The B0 manual has more conceptual information."; `Noblank;
    `P "$(i,%%PKG_HOMEPAGE%%/doc/B0.html#manual) or $(b,odig doc b0),"; ]

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
