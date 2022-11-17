(*---------------------------------------------------------------------------
   Copyright (c) 2022 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* FIXME potentially add these things to Fmt/fpath *)

let pp_cli_arg fmt = Fmt.tty [`Underline] fmt
let fpath_pp_high_suffix pre ppf p = match Fpath.strip_prefix pre p with
| None -> (Fmt.code Fpath.pp) ppf p
| Some p ->
    Fpath.pp ppf pre;
    (if not (Fpath.is_dir_path pre) then Fmt.char ppf Fpath.dir_sep_char);
    (Fmt.code Fpath.pp) ppf p

type t =
  { base : Fpath.t;
    prefix : Fpath.t;
    time : Os.Mtime.counter;
    vcs : B00_vcs.t }

let make ?vcs ?prefix env ~base =
  let scope_dir = B0_cmdlet.Env.scope_dir env in
  let prefix = Option.value ~default:scope_dir prefix in
  let* vcs = match vcs with
  | None -> B00_vcs.get ~dir:scope_dir () | Some vcs -> Ok vcs
  in
  let time = Os.Mtime.counter () in
  let base = Fpath.(prefix // base) in
  Ok { base; prefix; time; vcs }

let prefix exp = exp.prefix
let base exp = exp.base
let base_files exp ~recurse =
  Os.Dir.fold_files ~recurse Os.Dir.path_list exp.base []

let dur exp = Os.Mtime.count exp.time

(* Outcomes *)

module Outcome = struct
  type status = [ `Corrected | `Expected | `New | `Unexpected | `Unknown ]
  let merge_statuses sts =
    let merge acc st = match acc, st with
    | `Unknown, _ | _, `Unknown -> `Unknown
    | `Unexpected, _ | _, `Unexpected -> `Unexpected
    | `New, _ | _, `New -> `New
    | `Corrected, _ | _, `Corrected -> `Corrected
    | `Expected, `Expected -> `Expected
    in
    List.fold_left merge `Expected sts

  type t = status (* In the future hold here all the observations. *)
  let status = Fun.id
  let merge = merge_statuses
end

let file_outcome r file = match B00_vcs.kind r.vcs with
| B00_vcs.Git ->
    let git = B00_vcs.repo_cmd r.vcs in
    let cmd = Cmd.(git % "status" % "--porcelain" %% path file) in
    let* st = Os.Cmd.run_out ~trim:false cmd in
    begin match String.take_left 2 st with
    | "" -> Ok `Expected
    | "M " | "A " -> Ok `Corrected
    | "??" -> Ok `New
    | s when s.[1] = 'M' -> Ok `Unexpected
    | _ -> Ok `Unknown
    end
| B00_vcs.Hg ->
    failwith "TODO"

(* Logging stuff *)


let pp_label ppf o =
  let label ppf st l = Fmt.tty_string st ppf (String.concat " " [""; l; ""]) in
  match o with
  | `Unexpected -> label ppf [`Bg `Red; `Fg `White] "M"
  | `New -> label ppf [`Bg `Yellow; `Fg `Black] "?"

let log_outcome r file = function
| `Unexpected | `New as o ->
    Log.app (fun m -> m "%a %a" pp_label o (fpath_pp_high_suffix r.prefix) file)
| _ -> ()

(* Results log *)

let pp_vcs_cmd vcs ?(file = false) ppf cmd =
  let pp_file_arg ppf () = Fmt.(pp_cli_arg string) ppf "file" in
  let file = if file then pp_file_arg else Fmt.nop in
  Fmt.pf ppf "%a %a" Fmt.(code string) (String.concat " " [vcs; cmd]) file ()

let pp_git = pp_vcs_cmd "git"
let pp_hg = pp_vcs_cmd "hg"

let pp_new_cmd ppf vcs = match B00_vcs.kind vcs with
| B00_vcs.Git -> pp_git ~file:true ppf "add"
| B00_vcs.Hg -> pp_hg ppf ~file:true "TODO"

let pp_correct_cmd ppf vcs = match B00_vcs.kind vcs with
| B00_vcs.Git -> pp_git ~file:true ppf "add -p"
| B00_vcs.Hg -> pp_hg ~file:true ppf "TODO"

let pp_unexpected_cmd ppf vcs = match B00_vcs.kind vcs with
| B00_vcs.Git -> pp_git ~file:true ppf "diff"
| B00_vcs.Hg -> pp_hg ppf ~file:true "TODO"

let pp_status_cmd ppf (vcs, dir) = match B00_vcs.kind vcs with
| B00_vcs.Git -> pp_git ppf ("status -s " ^ Fpath.to_string dir)
| B00_vcs.Hg -> pp_hg ppf "TODO"

let pp_diff_cmd ppf (vcs, dir) = match B00_vcs.kind vcs with
| B00_vcs.Git -> pp_git ppf ("diff " ^ Fpath.to_string dir)
| B00_vcs.Hg -> pp_hg ppf "TODO"

let pp_status st status =
  Fmt.tty st (fun ppf c -> Fmt.pf ppf "%d %s" c status)

let pp_corrected ppf n = if n = 0 then () else Fmt.pf ppf " (%d corrected)" n
let pp_expected = pp_status [`Fg `Green] "expected"
let pp_unexpected = pp_status [`Fg `Red] "unexpected"
let pp_new = pp_status [`Fg `Yellow] "new"
let pp_unknown = pp_status [`Fg `Red] "unknown"

let pp_expected ppf = function
| (0, _) -> () | (n, c) -> Fmt.pf ppf "@,%a%a" pp_expected n pp_corrected c

let pp_unexpected ppf = function
| (0, _) -> () | (n, vcs) ->
    Fmt.pf ppf "@,%a  (check with %a, correct with %a)"
      pp_unexpected n pp_unexpected_cmd vcs pp_correct_cmd vcs

let pp_new ppf = function
| (0, _) -> () | (n, vcs) ->
    Fmt.pf ppf "@,%a  (integrate with %a)" pp_new n pp_new_cmd vcs

let pp_unknown ppf = function 0 -> () | n -> Fmt.pf ppf "@,%a" pp_unknown n

let pp_all_pass ppf (count, corr, dur) =
  let test = if count > 1 then "tests expected" else "test expected" in
  let green = [`Fg `Green] in
  Fmt.pf ppf "%a %a%a in %a"
    (Fmt.tty_string green) "All" (pp_status green test) count
    pp_corrected corr Mtime.Span.pp dur

let pp_total ppf (count, dur) =
  let test = if count > 1 then "tests" else "test" in
  Fmt.pf ppf "@,%a in %a" (pp_status [`Bold] test) count Mtime.Span.pp dur

let log_results exp os =
  let expected = ref 0 and unexpected = ref 0 and new' = ref 0
  and corrected = ref 0 and unknown = ref 0 in
  let incr o = match Outcome.status o with
  | `Expected -> incr expected | `Unexpected -> incr unexpected
  | `New -> incr new' | `Corrected -> incr expected; incr corrected
  | `Unknown -> incr unknown
  in
  let count = List.length os in
  let () = List.iter incr os in
  match !expected = count with
  | true ->
      Log.app (fun m -> m "%a" pp_all_pass (count, !corrected, dur exp));
      B00_cli.Exit.ok
  | false ->
      Log.app (fun m ->
          m "@[<v> @[<v>%a%a%a%a%a@]@,@,\
             Summary with %a@,Details with %a@]"
            pp_expected (!expected, !corrected) pp_new (!new', exp.vcs)
            pp_unknown !unknown pp_unexpected (!unexpected, exp.vcs)
            pp_total (count, dur exp)
            pp_status_cmd (exp.vcs, exp.base)
            pp_diff_cmd (exp.vcs, exp.base));
      Os.Exit.code 1

let stdout exp ?env ?cwd ~stdout:out cmd =
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true out in
  let* () = Os.Cmd.run ?env ?cwd ~stdout cmd in
  let* o = file_outcome exp out in
  log_outcome exp out o;
  Ok o

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The b0 programmers

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
