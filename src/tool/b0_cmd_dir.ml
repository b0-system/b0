(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

type kind = Root | B0 | Cache | Build | Unit

let get_kind ~kind ~units = match kind with
| None -> if units = [] then Build else Unit
| Some kind when kind <> Unit && units <> [] ->
    let pp_args = Fmt.(list ~sep:sp code) in
    Log.warn (fun m -> m "Ignoring positional arguments @[%a@]" pp_args units);
    kind
| Some kind -> kind

let output_dir dir =
  Fmt.pr "@[%a@]@." Fpath.pp (Fpath.drop_trailing_dir_sep dir);
  Os.Exit.ok

let output_dirs ~kind ~units conf = match get_kind ~kind ~units with
| Root -> B0_cmd_root.output_dir conf
| B0 -> output_dir (B0_driver.Conf.b0_dir conf)
| Cache -> output_dir (B0_driver.Conf.cache_dir conf)
| Unit -> B0_cmd_unit.output_dirs ~units conf
| Build ->
    let b0_dir = B0_driver.Conf.b0_dir conf in
    let build_dir =
      B0_build.B0_dir.build_dir ~b0_dir ~variant:"user" (* FIXME *)
    in
    output_dir build_dir

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let kind =
  let kinds = [
    Some Root,
    Arg.info ["root"] ~doc:"The root directory. See also $(tool) $(b,root).";
    Some B0,
    Arg.info ["b0"] ~doc:"The b0 directory. See $(b,--b0-dir).";
    Some Cache,
    Arg.info ["cache"] ~doc:"The cache directory. See $(b,--cache-dir).";
    Some Build,
    Arg.info ["build"]
      ~doc:"The build root directory. Default if there is no \
            positional argument.";
    Some Unit,
    Arg.info ["unit"]
      ~doc:"The directory of build unit(s). Default if there is a \
            positional argument. If this is specified and there is \
            no argument all of them are output. \
            See also $(tool) $(b,unit dir)"; ]
  in
  Arg.(value & vflag None kinds)

let units =
  let doc =
    "The $(docv) to act on. If $(b,--unit) is specified, this is \
     all of them if unspecified."
  in
  B0_cli.act_on_units_posn ~doc ~first:0 ()

let cmd =
  let doc = "Output the path to b0 directories" in
  let descr = `Blocks [
    `P "$(cmd) outputs the path to directories used by $(tool). The \
        directories do not necessarily exist on the file system.";
    `Pre "$(cmd)           # Path to the build directory";
    `Noblank;
    `Pre "$(cmd) $(b,myunit)    # Path to the build directory of $(b,myunit)";
    `Noblank;
    `Pre "$(cmd) $(b,--cache)   # Path to the cache directory";
    `P "Without arguments default to $(b,--build). With only positional \
        arguments it defaults $(b,--unit).";
    `Noblank;
  ]
  in
  B0_tool_cli.cmd_with_b0_file "dir" ~doc ~descr @@
  let+ kind and+ units in
  output_dirs ~kind ~units
