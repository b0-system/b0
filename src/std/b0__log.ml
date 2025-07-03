(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Reporting levels *)

type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug
let _level = ref Warning
let level () = !_level
let set_level l = _level := l
let level_to_string = function
| Quiet -> "quiet" | Stdout -> "stdout" | Stderr -> "stderr"
| Error -> "error" | Warning -> "warning"
| Info -> "info" | Debug -> "debug"

let level_of_string s = match String.trim s with
| "quiet" -> Ok Quiet
| "stdout" -> Ok Stdout
| "stderr" -> Ok Stderr
| "error" -> Ok Error
| "warning" -> Ok Warning
| "info" ->  Ok Info
| "debug" ->  Ok Debug
| e ->
    let pp_level = B0__fmt.code in
    let kind = B0__fmt.any "log level" in
    let dom =
      ["quiet"; "stdout"; "stderr"; "error"; "warning"; "info"; "debug"]
    in
    B0__fmt.error "%a"
      B0__fmt.(unknown' ~kind pp_level ~hint:must_be) (e, dom)

(* Reporting *)

let stdout_style = [`Fg `Cyan]
let stderr_style = [`Fg `Cyan]
let err_style = [`Fg `Red]
let warn_style = [`Fg `Yellow]
let info_style = [`Fg `Blue]
let debug_style = [`Faint; `Fg `Magenta]

let pp_level_str level ppf v = match level with
| Stdout -> B0__fmt.st stdout_style ppf v
| Stderr -> B0__fmt.st stderr_style ppf v
| Error -> B0__fmt.st err_style ppf v
| Warning -> B0__fmt.st warn_style ppf v
| Info -> B0__fmt.st info_style ppf v
| Debug -> B0__fmt.st debug_style ppf v
| Quiet -> assert false

let pp_level ppf level = match level with
| Stdout | Stderr -> ()
| Error -> B0__fmt.st (`Bold :: err_style) ppf "Error"
| Warning -> B0__fmt.st (`Bold :: warn_style) ppf "Warning"
| Info -> B0__fmt.st (`Bold :: info_style) ppf "Info"
| Debug -> B0__fmt.st (`Bold :: debug_style) ppf "Debug"
| Quiet -> assert false

let pp_header =
  let x = match Array.length Sys.argv with
  | 0 -> Filename.basename Sys.executable_name
  | n -> Filename.basename Sys.argv.(0)
  in
  let pp_header ppf (l, h) = match h with
  | None ->
      if l = Stderr || l = Stdout then () else
      B0__fmt.pf ppf "%s: %a: " x pp_level l
  | Some "" -> ()
  | Some h -> B0__fmt.pf ppf "%s: [%a] " x (pp_level_str l) h
  in
  pp_header

(* Log functions *)

let _err_count = ref 0
let err_count () = !_err_count

let _warn_count = ref 0
let warn_count () = !_warn_count

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type 'a log = ('a, unit) msgf -> unit
type 'a func = { log : 'a. 'a log }

let log func = func.log

type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

let report level k msgf =
  msgf @@ fun ?header fmt ->
  let k _ = k () in
  let ppf =
    if level = Stdout then Format.std_formatter else Format.err_formatter
  in
  Format.kfprintf k ppf ("@[%a" ^^ fmt ^^ "@]@.") pp_header (level, header)

let kmsg_nop = let kmsg k level msgf = k () in { kmsg }
let kmsg_default =
  let kmsg k level msgf = match !_level with
  | Quiet -> k ()
  | level' when level > level' || level = Quiet ->
      (if level = Error then incr _err_count else
       if level = Warning then incr _warn_count else ());
      (k ())
  | _ ->
      (if level = Error then incr _err_count else
       if level = Warning then incr _warn_count else ());
      report level k msgf
  in
  { kmsg }

let _kmsg = ref kmsg_default
let set_kmsg kmsg = _kmsg := kmsg

let kunit _ = ()
let msg level msgf = !_kmsg.kmsg kunit level msgf
let quiet msgf = !_kmsg.kmsg kunit Quiet msgf
let stdout msgf = !_kmsg.kmsg kunit Stdout msgf
let stderr msgf = !_kmsg.kmsg kunit Stderr msgf
let err msgf = !_kmsg.kmsg kunit Error msgf
let warn msgf = !_kmsg.kmsg kunit Warning msgf
let info msgf = !_kmsg.kmsg kunit Info msgf
let debug msgf = !_kmsg.kmsg kunit Debug msgf
let kmsg k level msgf = !_kmsg.kmsg k level msgf

(* Logging result errors *)

let if_error ?(level = Error) ?header ~use = function
| Ok v -> v
| Error msg ->
    !_kmsg.kmsg (fun _ -> use) level @@ fun m ->
    m ?header "@[%a@]" B0__fmt.lines msg

let if_error' ?(level = Error) ?header ~use = function
| Ok _ as v -> v
| Error msg ->
    !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m ->
    m ?header "@[%a@]" B0__fmt.lines msg

let if_error_pp ?(level = Error) ?header pp ~use = function
| Ok v -> v
| Error e ->
    !_kmsg.kmsg (fun _ -> use) level @@ fun m -> m ?header "@[%a@]" pp e

let if_error_pp' ?(level = Error) ?header pp ~use = function
| Ok _ as v -> v
| Error e ->
    !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m -> m ?header "@[%a@]" pp e

(* Timing logging *)

let time ?(level = Info) m f =
  let time = B0__os.Mtime.counter () in
  let r = f () in
  let span = B0__os.Mtime.count time in
  !_kmsg.kmsg (fun () -> r) level
    (fun w ->
       let header = Format.asprintf "%a" B0__mtime.Span.pp span in
       m r (w ~header))

(* Spawn logging *)

let spawn_tracer level =
  if level = Quiet then B0__os.Cmd.spawn_tracer_nop else
  let header = function
  | None -> "EXECV"
  | Some pid -> "EXEC:" ^ string_of_int (B0__os.Cmd.pid_to_int pid)
  in
  let pp_env ppf = function
  | None -> () |
    Some env ->
      B0__fmt.pf ppf "%a@," (B0__fmt.list B0__fmt.OCaml.string) env
  in
  fun pid env ~cwd cmd ->
    msg level (fun m ->
        m ~header:(header pid) "@[<v>%a%a@]" pp_env env B0__cmd.pp_dump cmd)

(* Exit logging *)

let () =
  B0__os.Exit.log_error :=
    (fun e -> err (fun m -> m "@[%a@]" B0__fmt.lines e))
