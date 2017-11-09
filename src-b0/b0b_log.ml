(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let find_units o names =
  let log = Log.Error in
  let kind = Unit.value_kind in
  let list = fun () -> Outcome.unit_names o in
  let get_or_suggest n =
    let list = list () in
    match List.mem n list with
    | true -> Ok n
    | false -> Error (String.suggest list n)
  in
  B0b_cli.find_named_values ~log ~kind ~list ~get_or_suggest names

let select_ops o names uncached op_ids =
  let idset = Outcome.unit_id_set names o in
  let cachep = match uncached with
  | None -> fun o -> true
  | Some cached -> fun o -> Outcome.Op.cached o = cached
  in
  let keep o =
    Unit.Idset.mem (Outcome.Op.unit_id o) idset && cachep o &&
    (op_ids = [] || List.mem (Outcome.Op.id o) op_ids)
  in
  let ops = List.filter keep (Outcome.ops o) in
  List.sort Outcome.Op.compare_exec_start_time ops

let pp_op = function
| `Normal -> Outcome.Op.pp_log_line
| `Short -> Outcome.Op.pp_log_line
| `Long -> Outcome.Op.pp_long

let pp_ops fmt o ppf ops =
  Fmt.pf ppf "@[<v>%a@]" Fmt.(list (pp_op fmt)) ops

let dump_log fmt o ops colorize pager =
  let tmp = Fpath.(OS.Dir.default_tmp () / "log") in
  let write_log log_file oc o =
    let cap = Fmt.tty_styling_cap () in
    begin match colorize with (* FIXME add with_styling_cap *)
    | true -> Fmt.set_tty_styling_cap Tty.Ansi
    | false -> Fmt.set_tty_styling_cap Tty.None
    end;
    let ppf = Format.formatter_of_out_channel oc in
    Fmt.pf ppf "%a@?" (pp_ops fmt o) ops;
    Fmt.set_tty_styling_cap cap;
    OS.Cmd.run Cmd.(pager % p log_file)
  in
  OS.File.with_tmp_oc tmp write_log o

let log_outcome fmt o ops =
  Log.app (fun m -> m "@[<v>%a@]" (pp_ops fmt o) ops); Ok `Ok

let out_log fmt units cached ids json no_pager color o =
  match find_units o units with
  | [], ret -> Ok ret
  | names, ret ->
      let ops = select_ops o names cached ids in
      match json with
      | true ->
          let json = B0_extra.ops_to_trace_event_format o ops in
          let json = B0_json.G.to_string json in
          Log.app (fun m -> m "%s" json); Ok `Ok
      | false ->
          match no_pager with
          | true -> log_outcome fmt o ops
          | false ->
              B0b_cli.find_pager ~don't:no_pager >>= function
              | None -> log_outcome fmt o ops
              | Some pager ->
                  let try_colorize = color <> Some Tty.None in
                  let cmd = Cmd.(v @@ p pager) in
                  let pager, colorize = match Fpath.filename pager with
                  | "more" | "less" when try_colorize -> Cmd.(cmd % "-R"), true
                  | _ -> cmd, false
                  in
                  match R.join @@ dump_log fmt o ops colorize pager with
                  | Ok () -> Ok `Ok
                  | Error _ as e ->
                      Log.on_error_msg e ~use:(fun _ -> ()); Ok `Some_error

let log variant units cached ids fmt json no_pager setup =
  let b0_dir = Driver.b0_dir setup in
  let log = Log.Error in
  begin match B0b_cli.get_variant ~log ~cli:variant ~b0_dir with
  | Error err -> Ok err
  | Ok load ->
      let variant = Variant.of_load load in
      match B0b_cli.variant_get_outcome ~log variant with
      | Error exit -> Ok exit
      | Ok o ->
          let color = Driver.color setup in
          out_log fmt units cached ids json no_pager color o
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let units = B0b_cli.units ~doc:"Show log of unit $(docv)."
let pkgs = B0b_cli.pkgs ~doc:"Show log of units of package $(docv)."

let cached =
  let cached =
    Some true, Arg.info ["cached"] ~doc:"Show only cached operations."
  in
  let uncached =
    Some false, Arg.info ["uncached"] ~doc:"Show only uncached operations."
  in
  Arg.(value & vflag None [cached; uncached])

let ids =
  let doc = "Show operation with identifier $(docv)." in
  Arg.(value & opt_all int [] & info ["id"] ~doc ~docv:"ID")

let json =
  let doc = "Output JSON text in Trace Event Format." in
  Arg.(value & flag & info ["json"] ~doc)

let doc = "Inspect the last build log"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "outcome" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command inspects current variant's last log of build
        operations.";
    `S Manpage.s_common_options;
    `S B0_driver.Cli.s_driver_opts; ]

let cmd =
  Term.(pure log $ Cli.variant $ units $ cached $ ids $ B0_driver.Cli.out_fmt $
        json $ B0b_cli.no_pager),
  Term.info "log" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
  `Driver

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
