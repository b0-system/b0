(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Cmdliner

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
  | Error `Term -> term_error
  | Error `Parse -> Os.Exit.cli_error
  | Error `Exn -> Os.Exit.internal_error
end

(* Argument converters *)

let cmd = Arg.conv' ~docv:"CMD" (B0_std.Cmd.of_string, B0_std.Cmd.pp_dump)
let fpath = Arg.conv' ~docv:"PATH" (Fpath.of_string, Fpath.pp_unquoted)

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


let get_tty_cap c = match Option.join c with
| Some c -> c | None -> Tty.(cap (of_fd Unix.stdout))

let get_log_level level = Option.value ~default:Log.Warning level

let setup cap level ~log_spawns =
  Fmt.set_tty_cap ~cap ();
  Log.set_level level;
  if level >= log_spawns
  then Os.Cmd.set_spawn_tracer (Log.spawn_tracer log_spawns)

(* Cli argumements *)

let tty_cap_of_string s = match String.trim s with
| "" | "auto" -> Ok None
| "always" -> Ok (Some `Ansi)
| "never" -> Ok (Some `None)
| e ->
    let pp_cap = Fmt.code in
    let kind = Fmt.any "color behaviour" in
    let dom = ["auto"; "always"; "never"] in
    Fmt.error "%a" Fmt.(unknown' ~kind pp_cap ~hint:must_be) (e, dom)

let tty_cap ?(docs = Manpage.s_common_options) ?env () =
  let parse s = Result.map_error (fun e -> `Msg e) (tty_cap_of_string s) in
  let pp ppf c = Fmt.string ppf @@ match c with
    | None -> "auto" | Some `Ansi -> "always" | Some `None -> "never"
  in
  let color = Arg.conv ~docv:"WHEN" (parse, pp) in
  let doc =
    "Colorize the output. $(docv) must be $(b,auto), $(b,always) \
       or $(b,never)."
  in
  let docv = "WHEN" and none = None in
  Arg.(value & opt (some' ~none color) None &
       info ["color"] ?env ~doc ~docv ~docs)

let log_level
    ?(none = Log.Warning) ?(docs = Manpage.s_common_options) ?env () =
  let vopts =
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
  in
  let verbosity =
    let parse s = Result.map_error (fun e -> `Msg e) (Log.level_of_string s)in
    let level = Arg.conv ~docv:"LEVEL" (parse, Log.pp_level) in
    let doc =
      "Be more or less verbose. $(docv) must be $(b,quiet), $(b,app), \
       $(b,error), $(b,warning), $(b,info) or $(b,debug)."
      in
      Arg.(value & opt (some ~none:"warning" level) None &
           info ["verbosity"] ?env ~docv:"LEVEL" ~doc ~docs)
  in
  let quiet =
      let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
      Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
  in
  let choose quiet verbosity vopts =
    if quiet then Some Log.Quiet else match vopts with
      | (_ :: []) -> Some Log.Info
      | ( _:: _ :: _) -> Some Log.Debug
      | [] -> verbosity
  in
  Term.(const choose $ quiet $ verbosity $ vopts)
