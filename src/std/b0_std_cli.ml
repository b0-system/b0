(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner
open Cmdliner.Term.Syntax

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

let path =
  let parser = Fpath.of_string and pp = Fpath.pp_unquoted in
  let completion = Arg.Completion.complete_paths in
  Arg.Conv.make ~docv:"PATH" ~parser ~pp ~completion ()

let filepath =
  let parser = Fpath.of_string and pp = Fpath.pp_unquoted in
  let completion = Arg.Completion.complete_files in
  Arg.Conv.make ~docv:"FILE" ~parser ~pp ~completion ()

let dirpath =
  let parser = Fpath.of_string and pp = Fpath.pp_unquoted in
  let completion = Arg.Completion.complete_dirs in
  Arg.Conv.make ~docv:"DIR" ~parser ~pp ~completion ()

let cmd =
  let parser = B0_std.Cmd.of_string and pp = B0_std.Cmd.pp in
  let completion = Arg.Completion.complete_files in
  Arg.Conv.make ~docv:"CMD" ~parser ~pp ~completion ()

(* ANSI styling *)

let no_color_var =
  let doc = "See $(opt). Enabled if set to anything but the empty string." in
  Cmd.Env.info ~doc "NO_COLOR"

let no_color
    ?(docs = Manpage.s_common_options) ?(env = (Some no_color_var)) ()
  =
  let doc = "Disable ANSI text styling." in
  (* We can't use Arg.flag here because it doesn't parse
     like https://no-color.org wants. *)
  let no_color = [true, Arg.info ["no-color"] ?env ~doc ~docs] in
  let+ no_color = Arg.(value & vflag false no_color)
  and+ env = Term.env in
  let env_no_color = function None | Some "" -> false | Some _ -> true in
  no_color || env_no_color (env "NO_COLOR")

let set_no_color ?docs ?env () =
  let set no_color = if no_color then Fmt.set_styler Fmt.Plain in
  let+ no_color = no_color ?docs ?env () in
  set no_color

(* Logging *)

let log_level_assoc =
  [ "quiet", Log.Quiet; "stdout", Log.Stdout; "stderr", Log.Stderr;
    "error", Log.Error; "warning", Log.Warning; "info", Log.Info;
    "debug", Log.Debug ]

let log_level_conv = Arg.enum ~docv:"LEVEL" log_level_assoc
let log_level_var = Cmd.Env.info "LOG_LEVEL"
let log_level
    ?(docs = Manpage.s_common_options) ?(absent = Log.Warning)
    ?(env = Some log_level_var) ()
  =
  let choose ~quiet ~verbose ~log_level =
    if quiet then Log.Quiet else match verbose with
    | [] -> Option.value ~default:absent log_level
    | [_] -> if absent = Log.Info then Log.Debug else Log.Info
    | _ -> Log.Debug
  in
  let+ quiet =
    let doc = "Be quiet. Takes over $(b,-v) and $(b,--log-level)." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
  and+ verbose =
    let doc =
      "Repeatable. Increase log verbosity. Once sets the log level to \
       $(b,info), twice to $(b,debug), more does not bring more. Takes over \
       $(b,--log-level)."
      (* The reason for taking over --log-level is due to cmdliner
         limitation: we cannot distinguish in choose below if
         verbosity was set via an env var. And cli args should always
         take over env var. So verbosity set through the env var would
         take over -v otherwise. *)
    in
    Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docs)
  and+ log_level =
    let doc_alts = Arg.doc_alts_enum log_level_assoc in
    let doc = Fmt.str "Set log level to $(docv). Must be %s." doc_alts in
    let level = Arg.some' ~none:absent log_level_conv in
    Arg.(value & opt level None & info ["log-level"] ?env ~doc ~docs)
  in
  choose ~quiet ~log_level ~verbose

let set_log_level ?docs ?absent ?env () =
  let+ log_level = log_level ?docs ?absent ?env () in
  B0_std.Log.set_level log_level

(* Specifying output level of details *)

type output_details = [ `Short | `Normal | `Long ]

let s_output_details_options = "OUTPUT DETAILS OPTIONS"
let output_details ?(docs = s_output_details_options) () =
  let short =
    let doc = "Short line-based output with essential details." in
    Arg.info ["s"; "short"] ~doc ~docs
  in
  let long =
    let doc = "Long output with as much details as possible." in
    Arg.info ["l"; "long"] ~doc ~docs
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])

(* Networking options *)

let socket_endpoint_conv ~default_port =
  let parse s = Os.Socket.Endpoint.of_string ~default_port s in
  Arg.conv' (parse, Os.Socket.Endpoint.pp)

let socket_endpoint_listener
    ?(opts = ["l"; "listen"]) ?docs ~default_port
    ?(default_endpoint = `Host ("localhost", default_port)) () =
  let doc =
    Printf.sprintf
      "Listen for connections on address $(i,ADDR) and port $(i,PORT) \
       (defaults to $(b,%d), if $(b,0) is specified one is chosen by \
       the OS) or the Unix domain socket $(i,PATH)."
      default_port
  in
  let docv = "ADDR[:PORT]|PATH" in
  let lconv = socket_endpoint_conv ~default_port in
  Arg.(value & opt lconv default_endpoint & info opts ?docs ~doc ~docv)
