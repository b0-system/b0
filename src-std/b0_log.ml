(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Reporting levels *)

type level = Quiet | App | Error | Warning | Info | Debug
let _level = ref Warning
let level () = !_level

let set_level l = _level := l

let level_to_string = function
| Quiet -> "quiet" | App -> "app" | Error -> "error" | Warning -> "warning"
| Info -> "info" | Debug -> "debug"

let level_of_string = function
| "quiet" -> Ok Quiet
| "app" -> Ok App
| "error" -> Ok Error
| "warning" -> Ok Warning
| "info" -> Ok Info
| "debug" -> Ok Debug
| l -> Error (`Msg (Printf.sprintf "%S: unknown log level" l))

(* Reporting *)

let app_style = [`Fg `Cyan]
let err_style = [`Fg `Red]
let warn_style = [`Fg `Yellow]
let info_style = [`Fg `Blue]
let debug_style = [`Faint; `Fg `Green]

let pp_level_str level ppf v = match level with
| App -> B0_tty.pp_str app_style ppf v
| Error -> B0_tty.pp_str err_style ppf v
| Warning -> B0_tty.pp_str warn_style ppf v
| Info -> B0_tty.pp_str info_style ppf v
| Debug -> B0_tty.pp_str debug_style ppf v
| Quiet -> assert false

let pp_level ppf level = match level with
| App -> ()
| Error -> B0_tty.pp_str err_style ppf "ERROR"
| Warning -> B0_tty.pp_str warn_style ppf "WARNING"
| Info -> B0_tty.pp_str info_style ppf "INFO"
| Debug -> B0_tty.pp_str debug_style ppf "DEBUG"
| Quiet -> assert false

let pp_header =
  let x = match Array.length Sys.argv with
  | 0 -> Filename.basename Sys.executable_name
  | n -> Filename.basename Sys.argv.(0)
  in
  let pp_header ppf (l, h) = match h with
  | None -> if l = App then () else B0_fmt.pf ppf "%s: [%a] " x pp_level l
  | Some h -> B0_fmt.pf ppf "%s: [%a] " x (pp_level_str l) h
  in
  pp_header

(* Log functions *)

let _err_count = ref 0
let err_count () = !_err_count
let incr_err_count () = incr _err_count

let _warn_count = ref 0
let warn_count () = !_warn_count
let incr_warn_count () = incr _warn_count

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
    if level = App then Format.std_formatter else Format.err_formatter
  in
  Format.kfprintf k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, header)

let nop_kmsg =
  let kmsg k level msgf = k () in
  { kmsg }

let default_kmsg =
  let kmsg k level msgf = match !_level with
  | Quiet -> k ()
  | level' when level > level' ->
      (if level = Error then incr _err_count else
       if level = Warning then incr _warn_count else ());
      (k ())
  | _ ->
      (if level = Error then incr _err_count else
       if level = Warning then incr _warn_count else ());
      report level k msgf
  in
  { kmsg }

let _kmsg = ref default_kmsg
let set_kmsg kmsg = _kmsg := kmsg

let kunit _ = ()
let msg level msgf = !_kmsg.kmsg kunit level msgf
let app msgf = !_kmsg.kmsg kunit App msgf
let err msgf = !_kmsg.kmsg kunit Error msgf
let warn msgf = !_kmsg.kmsg kunit Warning msgf
let info msgf = !_kmsg.kmsg kunit Info msgf
let debug msgf = !_kmsg.kmsg kunit Debug msgf
let kmsg k level msgf = !_kmsg.kmsg k level msgf

(* Logging result errors *)

let on_error ?(level = Error) ?header ~pp ~use = function
| Ok v -> v
| Error e ->
    !_kmsg.kmsg (fun () -> use e) level @@ fun m ->
    m ?header "@[%a@]" pp e

let on_error_msg ?(level = Error) ?header ~use = function
| Ok v -> v
| Error (`Msg msg) ->
    !_kmsg.kmsg use level @@ fun m ->
    m ?header "@[%a@]" B0_fmt.lines msg

(* Logging timings *)

let time ?(level = Info) m f v =
  let time = B0_time.counter () in
  let r = f v in
  let span = B0_time.count time in
  let header = Format.asprintf "%a" B0_time.pp_span span in
  !_kmsg.kmsg (fun () -> r) level (fun w -> m r (w ~header))

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
