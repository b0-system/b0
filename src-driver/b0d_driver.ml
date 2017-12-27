(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open Cmdliner

(* Driver setup *)

type exec = [ `Driver | `Instance ]
type setup =
  { cli : B0d_driver_cli.t;
    b0_dir : B0d_dir.t;
    exec : exec }

let setup cli b0_dir exec = { cli; b0_dir = B0d_dir.v b0_dir; exec }
let b0_dir s = s.b0_dir
let exec s = s.exec
let color s = B0d_driver_cli.color s.cli
let verbosity s = B0d_driver_cli.verbosity s.cli

(* Drivers *)

type cmd = (setup -> int) Term.t * Term.info * exec

type t =
  { name : string;
    version : string;
    libs : string list;
    cmds : cmd list; }

let create ~name ~version ~libs cmds = { name; version; libs; cmds; }

let err_no_driver_cmd = "No driver command is specified."
let err_no_driver = "No driver is defined."
let err_dup_driver n old =
  strf "driver %s: driver %s is already set" n.name old.name

let driver = ref None
let set d = match !driver with
| None -> driver := Some d
| Some old -> invalid_arg (err_dup_driver d old)

(* B0 and driver directory lookup *)

let find_b0_dir cli descr = match B0d_driver_cli.b0_dir cli with
| Some d -> Fpath.(B0d_driver_cli.cwd cli // d)
| None ->
    match descr with
    | None ->
        (* FIXME that will fail on some commands that need
           b0 but declare not be instanced. Since we need to
           the description to find the `_b0` dir maybe these
           commands are wrong. *)
        Fpath.(B0d_driver_cli.cwd cli // B0d_cli.default_b0_dir)
    | Some descr ->
        match B0d_root_descr.b0_dir descr with
        | Some d -> d (* d is absolute *)
        | None -> Fpath.(B0d_root_descr.dir descr // B0d_cli.default_b0_dir)

let find_driver_dir cli descr = match B0d_driver_cli.driver_dir cli with
| Some d -> Fpath.(B0d_driver_cli.cwd cli // d)
| None ->
    let descr_driver_dir = match descr with
    | None -> None
    | Some descr -> B0d_root_descr.driver_dir descr
    in
    match descr_driver_dir with
    | Some d -> d (* d is absolute *)
    | None -> Fpath.(find_b0_dir cli descr // B0d_cli.default_driver_dir)

(* Driver instance stamps *)

let stamp_file bin = Fpath.(bin + ".stamp")

let read_stamp stamp_file =
  begin
    OS.File.exists stamp_file >>= function
    | false -> Ok None
    | true -> OS.File.read stamp_file >>| fun stamp -> Some stamp
  end
  |> Log.on_error_msg ~use:(fun () -> None)

let write_stamp stamp_file = function
| None -> Ok ()
| Some stamp -> OS.File.write stamp_file stamp

(* Driver instance setup *)

let find_descr d cli =
  let dir, force = match B0d_driver_cli.root cli with
  | None -> B0d_driver_cli.cwd cli, false
  | Some dir -> Fpath.(B0d_driver_cli.cwd cli // dir), true
  in
  Log.time (fun _ m -> m "Root description lookup.")
  (fun () -> B0d_root_descr.find ~force ~dir) ()

let compile d action bin stamp_file stamp =
  Log.info (fun m -> m "%s %s: Compiling instance." d.name d.version);
  B0d_driver_compile.compile action
  >>= fun () -> write_stamp stamp_file stamp
  >>= fun () -> Ok bin

let instance_setup d cli descr =
  Log.time (fun _ m -> m "Instance setup.")
  (fun () ->
    begin
      let driver_libs = d.libs in
      let driver_name = d.name in
      let driver_dir = find_driver_dir cli (Some descr) in
      let bin_name = driver_name ^ if Sys.win32 then ".exe" else "" in
      let bin = Fpath.(driver_dir / bin_name) in
      match B0d_driver_cli.trust cli with
      | true -> Ok bin
      | false ->
          B0d_driver_compile.action cli descr
            ~driver_dir ~driver_name ~driver_libs ~bin >>= fun action ->
          let stamp_file = stamp_file bin in
          let stamp = match B0d_driver_compile.action_stamp action with
          | Ok s -> Some s
          | Error _ as e -> Log.on_error_msg ~use:(fun () -> None) e
          in
          OS.File.exists bin >>= function
          | false -> compile d action bin stamp_file stamp
          | true ->
            let bin_stamp = read_stamp stamp_file in
            match stamp, bin_stamp with
            | Some stamp, Some bin_stamp
              when stamp = bin_stamp && not (B0d_driver_cli.force cli) ->
                Log.info (fun m -> m "Instance stamp is valid.");
                Ok bin
            | _ -> compile d action bin stamp_file stamp
    end
    |> R.reword_error_msg ~replace:true
      (fun s -> R.msgf "instance setup: %s" s))
  ()

let instance_setup_only d cli =
  find_descr d cli >>= function
  | Some descr -> instance_setup d cli descr >>= fun _ -> Ok 0
  | None -> Ok (B0d_cli.no_description_found ())

(* Executing drivers and instances. *)

let exec_driver ?(msg = "") d cmd cli =
  Log.info (fun m -> m "%s %s %s: driver run%s" d.name Hash.name d.version msg);
  let setup = setup cli (find_b0_dir cli None) `Driver in
  Ok (cmd setup)

let exec_instance d cli descr =
  (* N.B. we reset the cwd since the instance will re-set it via
     B0d_driver_cli.setup. *)
  instance_setup d cli descr
  >>= fun bin ->
  (* FIXME this is wrong basically we should remove any --root from
     Sys.argv and replace it by this one. The problem with this
     is the intepretation of relative files paths andinteraction
     with -C. *)
  let argv = Cmd.of_list @@ Array.to_list @@ Sys.argv in
  OS.Dir.set_current (B0d_root_descr.dir descr)
  >>= fun () -> OS.Cmd.execv bin argv
  >>| fun () -> (* should not happen *) B0d_cli.exit_driver_setup_err

(* Wrapping driver commands *)

let wrap_driver_run d cmd exec_need =
  let run cli cmd =
    begin
      cli >>= fun cli -> match B0d_driver_cli.only cli with
      | true -> instance_setup_only d cli
      | false ->
          match exec_need with
          | `Driver -> exec_driver d cmd cli
          | `Instance ->
              find_descr d cli >>= function
              | None -> exec_driver d cmd cli ~msg:" (no description)"
              | Some descr -> exec_instance d cli descr
    end
    |> Log.on_error_msg ~use:(fun () -> B0d_cli.exit_driver_setup_err)
  in
  Term.(pure run $ B0d_driver_cli.setup $ cmd)

let wrap_instance_run d cmd =
  let run cli cmd =
    begin cli >>= fun cli ->
      Log.info (fun m -> m "%s %s %s: instance run" d.name Hash.name d.version);
      let setup = setup cli (find_b0_dir cli None) `Instance in
      Ok (cmd setup)
    end
    |> Log.on_error_msg ~use:(fun () -> B0d_cli.exit_driver_setup_err)
  in
  Term.(pure run $ B0d_driver_cli.setup $ cmd)

let wrap_cmd exec d (cmd, info, exec_need) = match exec with
| `Driver -> wrap_driver_run d cmd exec_need, info
| `Instance -> wrap_instance_run d cmd, info

(* Driver and instance main functions *)

let main exec d = match List.map (wrap_cmd exec d) d.cmds with
| main :: [] -> Term.(exit_status @@ eval main)
| main :: cmds -> Term.(exit_status @@ eval_choice main cmds)
| [] -> invalid_arg err_no_driver_cmd

let driver_main () = match !driver with
| None -> invalid_arg err_no_driver
| Some d -> main `Driver d

let instance_main () = match !driver with
| None -> invalid_arg err_no_driver
| Some d -> main `Instance d

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
