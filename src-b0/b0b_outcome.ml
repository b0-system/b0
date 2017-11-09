(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

(* FIXME review internals *)
let kind roots builts p = match Fpath.Set.mem p roots with
| true -> `Root
| false ->
    match Fpath.Set.mem p builts with
    | true -> `Built
    | false -> `Unknown

module Int = struct type t = int let compare : t -> t -> t = compare end
module Iset = Set.Make (Int)

let index_ops o =
  let nmap = Outcome.unit_id_name_map o in
  let add_op op p m =
    let unit = match Unit.Idmap.find (Outcome.Op.unit_id op) nmap with
    | exception Not_found -> assert false | unit -> unit
    in
    match Fpath.Map.find p m with
    | exception Not_found -> Fpath.Map.add p [unit, Outcome.Op.id op] m
    | ops -> Fpath.Map.add p ((unit, Outcome.Op.id op) :: ops) m
  in
  let rec loop read built = function
  | [] -> read, built
  | op :: ops ->
      let read = Fpath.Set.fold (add_op op) (Outcome.Op.reads op) read in
      let built = Fpath.Set.fold (add_op op) (Outcome.Op.writes op) built in
      loop read built ops
  in
  let read, built = loop Fpath.Map.empty Fpath.Map.empty (Outcome.ops o) in
  let unit_map op_index =
    let rec loop acc = function
    | [] -> acc
    | (unit, id) :: els ->
        match String.Map.find unit acc with
        | exception Not_found -> loop (String.Map.add unit [id] acc) els
        | ids -> loop (String.Map.add unit (id :: ids) acc) els
    in
    loop String.Map.empty op_index
  in
  Fpath.Map.map unit_map read, Fpath.Map.map unit_map built

let pp_index =
  let pp_binding ppf (unit, ids) =
    Fmt.pf ppf"%a: @[%a@]" Unit.pp_name_str unit Fmt.(list ~sep:sp int) ids
  in
  Fmt.vbox @@ Fmt.(iter_bindings String.Map.iter pp_binding)

let out_short = Fpath.pp
let out_normal roots builts ppf p =
  let kind ppf () = match kind roots builts p with
  | `Root -> Tty.pp_str [`Fg `Green] ppf "R"
  | `Built -> Tty.pp_str [`Faint; `Fg `Green] ppf "B"
  | `Unknown -> Tty.pp_str [`Fg `Red] ppf "?"
  in
  Fmt.pf ppf "%a %a" kind () Fpath.pp p

let out_long ireads iwrites roots builts meta ppf p =
  let kind ppf () = Fmt.string ppf @@ match kind roots builts p with
  | `Root -> "root"
  | `Built -> "built"
  | `Unknown -> "unknown"
  in
  Fmt.pf ppf "@[<v1>%a@," Fpath.pp p;
  Fmt.field "kind" kind ppf ();
  begin match Fpath.Map.find p ireads with
  | exception Not_found -> ()
  | index ->
      Fmt.cut ppf ();
      Fmt.(field "read-by" pp_index) ppf index
  end;
  begin match Fpath.Map.find p iwrites with
  | exception Not_found -> ()
  | index ->
      Fmt.cut ppf ();
      Fmt.(field "built-by" pp_index) ppf index
  end;
  begin match Fpath.Map.find p meta with
  | exception Not_found -> ()
  | m ->
      Fmt.cut ppf ();
      Fmt.(field "meta" Fmt.(vbox @@ Fpath.Meta.pp)) ppf m;
  end;
  Fmt.pf ppf "@]"

let select_paths o = function
| `All -> Fpath.Set.union (Outcome.root_files o) (Outcome.built_files o)
| `Roots -> Outcome.root_files o
| `Built -> Outcome.built_files o

let outcome_paths o out_fmt path_kind =
  let ps = select_paths o path_kind in
  let pp = match out_fmt with
  | `Short -> out_short
  | `Normal -> out_normal (Outcome.root_files o) (Outcome.built_files o)
  | `Long ->
      let ireads, iwrites = index_ops o in
      out_long ireads iwrites (Outcome.root_files o) (Outcome.built_files o)
        (Outcome.fpath_meta o)
  in
  match Fpath.Set.is_empty ps with
  | true -> ()
  | false -> Log.app (fun m -> m "@[<v>%a@]" (Fpath.Set.pp pp) ps)

let outcome_stats o = Log.app (fun m -> m "@[%a@]" Outcome.pp_stats o)

let outcome action variant out_fmt path_kind setup =
  let b0_dir = Driver.b0_dir setup in
  let log = Some Log.Error in
  begin
    match B0b_cli.get_variant ~log ~cli:variant ~b0_dir with
    | Error err -> Ok err
    | Ok load ->
        let variant = Variant.of_load load in
        match B0b_cli.variant_get_outcome ~log variant with
        | Error exit -> Ok exit
        | Ok o ->
            match action with
            | `Paths -> outcome_paths o out_fmt path_kind; Ok `Ok
            | `Stats -> outcome_stats o; Ok `Ok
  end
  |> B0b_cli.join_exit |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  B0b_cli.action_arg [ ("paths", `Paths); ("stats", `Stats); ]

let doc = "Inspect the last build outcome"
let sdocs = Manpage.s_common_options
let man_xrefs = [ `Main; `Cmd "cache" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command inspects the last build outcome.";
    `S "ACTIONS";
    `I ("$(b,paths)", "show read and written paths");
    `I ("$(b,stats)", "show numbers about the build");
    `S Manpage.s_common_options;
    `S B0_driver.Cli.s_driver_opts; ]

let cmd =
  Term.(pure outcome $ action $ Cli.variant $ B0_driver.Cli.out_fmt $
        B0_driver.Cli.file_kind),
  Term.info "outcome" ~doc ~sdocs ~exits:B0b_cli.exits ~man ~man_xrefs,
  `Driver

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
