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

(* Aborting *)

exception Abort of string
let abort msg = raise (Abort msg)
let abortf fmt = Format.kasprintf abort fmt
let result_to_abort = function Ok v -> v | Error msg -> abort msg
let abort_to_result f = try Ok (f ()) with Abort e -> Error e

(* Outcomes *)

module Outcome = struct
  type status = [ `Corrected | `Expected | `New | `Unexpected | `Unknown ]
  type test = File of { file : Fpath.t; diff : bool }

  type t = { status : status; test : test }
  let v status test = { status; test }
  let status o = o.status
  let test o = o.test
end

(* Contexts *)

type t =
  { base : Fpath.t;
    env : B0_cmdlet.Env.t;
    log_absolute : bool;
    log_diffs : bool;
    mutable outcomes : Outcome.t list;
    mutable seen : Fpath.Set.t;
    time : Os.Mtime.counter;
    vcs : B00_vcs.t; }

let make ?vcs ?(log_absolute = false) ?(log_diffs = true) env ~base =
  let scope_dir = B0_cmdlet.Env.scope_dir env in
  let base = Fpath.(scope_dir // base) in
  let outcomes = [] and seen = Fpath.Set.empty in
  let time = Os.Mtime.counter () in
  let vcs = match vcs with
  | None -> B00_vcs.get ~dir:scope_dir () |> result_to_abort
  | Some vcs -> vcs
  in
  { base; env; log_absolute; log_diffs; outcomes; seen; time; vcs }

let base ctx = ctx.base
let base_files ?rel ctx ~recurse =
  result_to_abort @@
  Os.Dir.fold_files ?rel ~recurse Os.Dir.path_list ctx.base []

let dur ctx = Os.Mtime.count ctx.time
let env ctx = ctx.env
let log_absolute ctx = ctx.log_absolute
let log_diffs ctx = ctx.log_diffs
let vcs ctx = ctx.vcs
let outcomes ctx = ctx.outcomes

let cwd_rel_path ctx p = Fpath.relative ~to_dir:(B0_cmdlet.Env.cwd ctx.env) p
let path_for_user ctx p = if ctx.log_absolute then p else cwd_rel_path ctx p
let pp_path_for_user ctx ppf p = match ctx.log_absolute with
| true -> fpath_pp_high_suffix (B0_cmdlet.Env.scope_dir ctx.env) ppf p
| false -> (Fmt.code Fpath.pp) ppf (cwd_rel_path ctx p)

(* Showing results *)

let log_diff ctx file =  match B00_vcs.kind ctx.vcs with
| Git ->
    let git = B00_vcs.repo_cmd ctx.vcs in
    let cmd = Cmd.(git % "diff" %% path file) in
    Log.if_error ~use:() (Os.Cmd.run cmd)
| Hg ->
    failwith "Hg support is TODO"

let log_outcome ctx o =
  let label ppf st l = Fmt.tty_string st ppf (String.concat " " ["";l;""]) in
  let pp_label ppf = function
  | `Unexpected -> label ppf [`Bg `Red; `Fg `White] "M"
  | `New -> label ppf [`Bg `Yellow; `Fg `Black] "?"
  in
  match Outcome.test o with
  | File { file; diff } ->
      match Outcome.status o with
      | `Unexpected | `New as st ->
          Log.app (fun m -> m "%a %a" pp_label st (pp_path_for_user ctx) file);
          if ctx.log_diffs && diff then log_diff ctx file
      | _ -> ()

(* Log summary *)

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

let log_summary ctx =
  let expected = ref 0 and unexpected = ref 0 and new' = ref 0
  and corrected = ref 0 and unknown = ref 0 in
  let incr o = match Outcome.status o with
  | `Expected -> incr expected | `Unexpected -> incr unexpected
  | `New -> incr new' | `Corrected -> incr expected; incr corrected
  | `Unknown -> incr unknown
  in
  let count = List.length ctx.outcomes in
  let () = List.iter incr ctx.outcomes in
  match !expected = count with
  | true ->
      Log.app (fun m -> m "%a" pp_all_pass (count, !corrected, dur ctx));
      B00_cli.Exit.ok
  | false ->
      Log.app (fun m ->
          m "@[<v> @[<v>%a%a%a%a%a@]@,@,\
             Summary with %a@,Details with %a@]"
            pp_expected (!expected, !corrected) pp_new (!new', ctx.vcs)
            pp_unknown !unknown pp_unexpected (!unexpected, ctx.vcs)
            pp_total (count, dur ctx)
            pp_status_cmd (ctx.vcs, path_for_user ctx ctx.base)
            pp_diff_cmd (ctx.vcs, path_for_user ctx ctx.base));
      Os.Exit.code 1

(* Primitives *)

let outcome_of_file ?(diff = true) ctx file =
  let file = Fpath.(ctx.base // file) in
  match B00_vcs.kind ctx.vcs with
  | Git ->
      let git = B00_vcs.repo_cmd ctx.vcs in
      let cmd = Cmd.(git % "status" % "--porcelain" %% path file) in
      let status = Os.Cmd.run_out ~trim:false cmd |> result_to_abort in
      let status = match String.take_left 2 status with
      | "" -> `Expected
      | "M " | "A " -> `Corrected
      | "??" -> `New
      | s when s.[1] = 'M' -> `Unexpected
      | _ -> `Unknown
      in
      Outcome.v status (File { file; diff })
  | Hg ->
    failwith "Hg support is TODO"

let check_add ctx o = match Outcome.test o with
| File { file; _ } ->
    if not (Fpath.Set.mem file ctx.seen)
    then ctx.seen <- Fpath.Set.add file ctx.seen
    else Fmt.invalid_arg "%a: file already checked. \
                          Are you trying to write it multiple times ?"
        Fpath.pp_unquoted file

let add_outcome ctx o =
  check_add ctx o; ctx.outcomes <- o :: ctx.outcomes; log_outcome ctx o

let finish ctx = log_summary ctx

(* Expectations *)

let file ?diff ctx file = add_outcome ctx (outcome_of_file ctx file)
let stdout ?diff ctx ?env ?cwd ?stdout cmd =
  let out = match stdout with
  | None -> Fpath.v (Fpath.basename (Cmd.get_tool cmd) ^ ".stdout")
  | Some stdout -> stdout
  in
  let out = Fpath.(ctx.base // out) in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true out in
  let () = Os.Cmd.run ?env ?cwd ~stdout cmd |> result_to_abort in
  file ?diff ctx out

(* Cmdlet *)

let short =
  let doc = "Short output, do not output diffs on unexpected or new tests." in
  Cmdliner.Arg.(value & flag & info ["s"; "short"] ~doc)

let log_absolute_arg =
  let doc =
    "Output absolute paths instead of having them relative to the \
     current working directory."
  in
  Cmdliner.Arg.(value & flag & info ["a"; "absolute-paths"] ~doc)

let exits =
  Cmdliner.Cmd.Exit.info 1 ~doc:"on unexpected expectations." ::
  Cmdliner.Cmd.Exit.defaults

let run f env base short log_absolute =
  B0_cmdlet.exit_of_result' @@
  abort_to_result @@ fun () ->
  let ctx = make env ~log_absolute ~log_diffs:(not short) ~base in
  f ctx; finish ctx

let cmdlet env cmd ~base f =
  let run = run f env base in
  let run = Cmdliner.Term.(const run $ short $ log_absolute_arg) in
  B0_cmdlet.eval ~exits env cmd run

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
