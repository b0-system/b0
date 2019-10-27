(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

(* Setup *)

let get_tty_cap c = match Option.join c with
| Some c -> c | None -> Tty.(cap (of_fd Unix.stdout))

let get_log_level level = Option.value ~default:Log.Warning level

let log_spawn_tracer level =
  if level = Log.Quiet then B0_std.Os.Cmd.spawn_tracer_nop else
  let header = function
  | None -> "EXECV"
  | Some pid -> "EXEC:" ^ string_of_int (Os.Cmd.pid_to_int pid)
  in
  let pp_env ppf = function
  | None -> () | Some env -> Fmt.pf ppf "%a@," (Fmt.list String.pp_dump) env
  in
  fun pid env ~cwd cmd ->
    Log.msg level (fun m ->
        m ~header:(header pid) "@[<v>%a%a@]" pp_env env Cmd.pp_dump cmd)

let setup cap level ~log_spawns =
  Fmt.set_tty_styling_cap cap;
  Log.set_level level;
  if level >= log_spawns
  then Os.Cmd.set_spawn_tracer (log_spawn_tracer log_spawns)

(* Argument converters *)

open Cmdliner

let err_msg of_string s = Result.map_error (fun e -> `Msg e) (of_string s)
let fpath = Arg.conv ~docv:"PATH" (err_msg Fpath.of_string, Fpath.pp_quoted)
let cmd = Arg.conv ~docv:"CMD" (err_msg Cmd.of_string, Cmd.pp_dump)

(* Cli argumements *)

let tty_cap_of_string s = match String.trim s with
| "" | "auto" -> Ok None
| "always" -> Ok (Some `Ansi)
| "never" -> Ok (Some `None)
| e ->
    let pp_cap = Fmt.(bold string) in
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
    "Colorize the output. $(docv) must be $(b,auto), $(b,always) or $(b,never)."
  in
  let docv = "WHEN" in
  Arg.(value & opt (some color) None & info ["color"] ?env ~doc ~docv ~docs)

let log_level
    ?(none = Log.Warning) ?(docs = Manpage.s_common_options) ?env ()
  =
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
    let none = Log.level_to_string none in
    Arg.(value & opt (some ~none level) None &
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

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
