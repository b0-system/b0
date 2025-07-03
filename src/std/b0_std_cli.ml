(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner
open Cmdliner.Term.Syntax

(* FIXME try to get rid of this in favour of More_cli. *)

(* Exit *)

module Exit = struct
  let e c doc = Cmd.Exit.info (Os.Exit.get_code c) ~doc
  let infos =
    e Os.Exit.no_such_name "if a specified name does not exist." ::
    Cmd.Exit.defaults

  (* FIXME remove this once we release Cmdliner with
     Cmd.eval_value' *)

  let of_eval_result ?(term_error = Os.Exit.cli_error) = function
  | Ok (`Ok e) -> e
  | Ok _ -> Os.Exit.ok
  | Error `Term -> Os.Exit.cli_error
  | Error `Parse -> Os.Exit.cli_error
  | Error `Exn -> Os.Exit.internal_error
end

(* Argument converters *)

let cmd =
  let parser = B0_std.Cmd.of_string and pp = B0_std.Cmd.pp in
  let completion = Arg.Completion.make ~files:true () in
  Arg.Conv.make ~docv:"CMD" ~parser ~pp ~completion ()

let fpath =
  let parser = Fpath.of_string and pp = Fpath.pp_unquoted in
  let completion = Arg.Completion.make ~dirs:true ~files:true () in
  Arg.Conv.make ~docv:"PATH" ~parser ~pp ~completion ()

(* Specifying output detail *)

let s_output_format_options = "OUTPUT FORMAT OPTIONS"

type output_format = [ `Normal | `Short | `Long ]
let output_format
    ?(docs = s_output_format_options) ?(short_opts = ["s"; "short"])
    ?(long_opts = ["l"; "long"]) ()
  =
  let short =
    let doc = "Short output. Line based output with only relevant data." in
    Arg.info short_opts ~doc ~docs
  in
  let long =
    let doc = "Long output. Outputs as much information as possible." in
    Arg.info long_opts ~doc ~docs
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])


let get_styler c = match Option.join c with
| Some c -> c | None -> Fmt.styler ()

let get_log_level level = Option.value ~default:Log.Warning level

let setup_log level ~log_spawns =
  Log.set_level level;
  if level >= log_spawns
  then Os.Cmd.set_spawn_tracer (Log.spawn_tracer log_spawns)

let setup styler level ~log_spawns =
  Fmt.set_styler styler; setup_log level ~log_spawns

(* Cli argumements *)

let styler_of_string s = match String.trim s with
| "" | "auto" -> Ok None
| "always" -> Ok (Some Fmt.Ansi)
| "never" -> Ok (Some Fmt.Plain)
| e ->
    let pp_cap = Fmt.code in
    let kind = Fmt.any "color behaviour" in
    let dom = ["auto"; "always"; "never"] in
    Fmt.error "%a" Fmt.(unknown' ~kind pp_cap ~hint:must_be) (e, dom)

let color ?(docs = Manpage.s_common_options) ?env () =
  let parse s = Result.map_error (fun e -> `Msg e) (styler_of_string s) in
  let pp ppf c = Fmt.string ppf @@ match c with
    | None -> "auto" | Some Fmt.Ansi -> "always" | Some Fmt.Plain -> "never"
  in
  let color = Arg.conv ~docv:"WHEN" (parse, pp) in
  let doc =
    "Colorize the output. $(docv) must be $(b,auto), $(b,always) \
     or $(b,never)."
  in
  let docv = "WHEN" and none = None in
  Arg.(value & opt (some' ~none color) None &
       info ["color"] ?env ~doc ~docv ~docs)


(* Logging *)

let log_level_assoc =
  [ "quiet", Log.Quiet;
    "stdout", Log.Stdout;
    "stderr", Log.Stderr;
    "error", Log.Error;
    "warning", Log.Warning;
    "info", Log.Info;
    "debug", Log.Debug ]

let log_level = Arg.enum ~docv:"LEVEL" log_level_assoc

let vincr ~docs =
  let doc =
    "Increase verbosity. Repeatable, but more than twice does not bring \
     more. Takes over $(b,--verbosity)."
    (* The reason for taking over verbosity is due to cmdliner
       limitation: we cannot distinguish in choose below if verbosity
       was set via an env var. And cli args should always take over env
       var. So verbosity set through the env var would take over -v
       otherwise. *)
  in
  Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docs)

let verbosity ?env ~docs ~none () =
  let doc_alts = Arg.doc_alts_enum log_level_assoc in
  let doc = Fmt.str "Set log verbosity to $(docv). Must be %s" doc_alts in
  let level = Arg.some' ~none log_level in
  Arg.(value & opt level None & info ["verbosity"] ?env ~doc ~docs)

let quiet ~docs =
  let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
  Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)


let log_level
    ?(none = Log.Warning) ?(docs = Manpage.s_common_options) ?env () =
  let choose quiet verbosity vopts =
    if quiet then Some Log.Quiet else match vopts with
    | [] -> verbosity
    | [_] -> Some Log.Info
    | _ -> Some Log.Debug
  in
  Term.(const choose $ quiet ~docs $ verbosity ?env ~none ~docs () $
        vincr ~docs)

let log_setup
    ?(spawns = Log.Debug) ?(absent = Log.Warning)
    ?(docs = Manpage.s_common_options) ?env ()
  =
  let setup ~quiet ~verbosity ~vincr =
    let level =
      if quiet then Log.Quiet else
      match vincr with
      | [] -> Option.value ~default:absent verbosity
      | [_] -> if absent = Log.Info then Log.Debug else Log.Info
      | _ -> Log.Debug
    in
    Log.set_level level;
    if level >= spawns
    then Os.Cmd.set_spawn_tracer (Log.spawn_tracer spawns)
  in
  let env = match env with
  | None -> Cmd.Env.info "LOG_LEVEL"
  | Some env -> env
  in
  let+ vincr = vincr ~docs
  and+ verbosity = verbosity ~env ~none:absent ~docs ()
  and+ quiet = quiet ~docs in
  setup ~quiet ~verbosity ~vincr
