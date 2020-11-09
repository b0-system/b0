(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

module Exit = struct
  open Cmdliner

  let deploy_error = Os.Exit.Code 118
  let build_error = Os.Exit.Code 119
  let b0_file_error = Os.Exit.Code 120
  let no_b0_file = Os.Exit.Code 121

  let e c doc = Term.exit_info (Os.Exit.get_code c) ~doc
  let info_deploy_error = e deploy_error "on deploy error."
  let info_build_error = e build_error "on build error."
  let info_b0_file_error = e b0_file_error "on B0 file error."
  let info_no_b0_file = e no_b0_file "no B0 file found."

  let infos =
    info_deploy_error :: info_build_error ::
    info_b0_file_error :: info_no_b0_file ::
    B00_cli.Exit.infos
end

module Env = struct
  let b0_dir = B00_cli.Memo.b0_dir_env
  let b0_file = "B0_FILE"
  let cache_dir = B00_cli.Memo.cache_dir_env
  let color = "B0_COLOR"
  let code = "B0_DRIVER_CODE"
  let hash_fun = B00_cli.Memo.hash_fun_env
  let jobs = B00_cli.Memo.jobs_env
  let verbosity = "B0_VERBOSITY"
end

module Conf = struct
  let b0_file_name = "B0.ml"
  let drivers_dir_name = ".drivers"
  type t =
    { b0_dir : Fpath.t;
      b0_file : Fpath.t option;
      cache_dir : Fpath.t;
      cwd : Fpath.t;
      code : B00_ocaml.Conf.code option;
      hash_fun : (module Hash.T);
      jobs : int;
      log_level : Log.level;
      no_pager : bool;
      memo : (Memo.t, string) result Lazy.t;
      tty_cap : Tty.cap; }

  let memo ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs =
    let feedback =
      let op_howto ppf o = Fmt.pf ppf "b0 file log --id %d" (B000.Op.id o) in
      let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
      B00_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
        Fmt.stderr
    in
    Memo.memo ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()

  let v
      ~b0_dir ~b0_file ~cache_dir ~cwd ~code ~hash_fun ~jobs
      ~log_level ~no_pager ~tty_cap ()
    =
    let trash_dir = Fpath.(b0_dir / B00_cli.Memo.trash_dir_name) in
    let memo = lazy (memo ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs) in
    { b0_dir; b0_file; cache_dir; cwd; code; hash_fun; jobs;
      memo; log_level; no_pager; tty_cap }

  let b0_dir c = c.b0_dir
  let b0_file c = c.b0_file
  let cache_dir c = c.cache_dir
  let cwd c = c.cwd
  let code c = c.code
  let hash_fun c = c.hash_fun
  let jobs c = c.jobs
  let log_level c = c.log_level
  let memo c = Lazy.force c.memo
  let no_pager c = c.no_pager
  let tty_cap c = c.tty_cap

  let get_b0_file c = match c.b0_file with
  | Some file -> Ok file
  | None ->
      let path = Fmt.(code Fpath.pp_unquoted) in
      let code = Fmt.(code string) in
      Fmt.error
        "@[<v>No %a file found in %a@,\
                 or upwards. Use option %a to specify one or %a for help.@]"
        code "B0.ml" path c.cwd Fmt.(code string) "--b0-file"
        Fmt.(code string) "--help"

  (* Setup *)

  let find_b0_file ~cwd ~b0_file = match b0_file with
  | Some b0_file -> Some (Fpath.(cwd // b0_file))
  | None ->
      let rec loop dir =
        let file = Fpath.(dir / b0_file_name) in
        match Os.File.exists file with
        | Ok true -> Some file
        | _ ->
            if not (Fpath.is_root dir) then loop (Fpath.parent dir) else None
      in
      loop cwd

  let setup_with_cli
      ~b0_dir ~b0_file ~cache_dir ~code ~hash_fun ~jobs
      ~log_level ~no_pager ~tty_cap ()
    =
    let tty_cap = B00_cli.B00_std.get_tty_cap tty_cap in
    let log_level = B00_cli.B00_std.get_log_level log_level in
    B00_cli.B00_std.setup tty_cap log_level ~log_spawns:Log.Debug;
    Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
    let b0_file = find_b0_file ~cwd ~b0_file in
    let root = match b0_file with Some f -> Fpath.parent f | None -> cwd  in
    let b0_dir = B00_cli.Memo.get_b0_dir ~cwd ~root ~b0_dir in
    let cache_dir = B00_cli.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
    let hash_fun = B00_cli.Memo.get_hash_fun ~hash_fun in
    let jobs = B00_cli.Memo.get_jobs ~jobs in
    Ok (v ~b0_dir ~b0_file ~cache_dir ~cwd ~code ~hash_fun
          ~jobs ~log_level ~no_pager ~tty_cap ())
end

module Cli = struct
  open Cmdliner

  let docs = Manpage.s_common_options
  let b0_dir = B00_cli.Memo.b0_dir ~docs ()
  let b0_file =
    let env = Arg.env_var Env.b0_file in
    let doc = "Use $(docv) as the B0 file." and docv = "PATH" in
    let none = "B0.ml file in cwd or first upwards" in
    Arg.(value & opt (Arg.some ~none B00_cli.fpath) None &
         info ["b0-file"] ~doc ~docv ~docs ~env)

  let cache_dir = B00_cli.Memo.cache_dir ~docs ()
  let code =
    let env = Arg.env_var Env.code in
    let code_enum = ["byte", Some `Byte; "native", Some `Native; "auto", None]in
    let code = Arg.enum code_enum in
    let docv = "CODE" in
    let doc =
      "Compile driver to $(docv). $(docv) must be $(b,byte), $(b,native) or \
       $(b,auto). If $(b,auto) favors native code if available."
    in
    Arg.(value & opt code None & info ["driver-code"] ~doc ~docv ~docs ~env)

  let hash_fun = B00_cli.Memo.hash_fun ~docs ()
  let jobs = B00_cli.Memo.jobs ~docs ()
  let log_level =
    B00_cli.B00_std.log_level ~docs ~env:(Arg.env_var Env.verbosity) ()

  let tty_cap = B00_cli.B00_std.tty_cap ~docs ~env:(Arg.env_var Env.color) ()
  let no_pager = B00_pager.don't ~docs ()
  let conf =
    let conf
        b0_dir b0_file cache_dir code hash_fun jobs log_level no_pager tty_cap
      =
      Result.map_error (fun s -> `Msg s) @@
      Conf.setup_with_cli
        ~b0_dir ~b0_file ~cache_dir ~code ~hash_fun ~jobs ~log_level
        ~no_pager ~tty_cap ()
    in
    Term.term_result @@
    Term.(const conf $ b0_dir $ b0_file $ cache_dir $ code $
          hash_fun $ jobs $ log_level $ no_pager $ tty_cap)
end

(* Drivers *)

type main = unit -> Os.Exit.t Cmdliner.Term.result
type t =
  { name : string;
    version : string;
    libs : B00_ocaml.Lib.Name.t list }

let create ~name ~version ~libs = { name; version; libs }
let name d = d.name
let version d = d.version
let libs d = d.libs

let has_b0_file = ref false (* set by run *)
let driver = ref None
let set ~driver:d ~main = driver := Some (d, main)

let timing b0_file d _ m =
  let b0_file = if b0_file then "with B0.ml" else "no B0.ml" in
  m "total time %s %s %s" d.name d.version b0_file

let run ~has_b0_file:b0_file =
  B00_cli.Exit.exit ~exec_error:B00_cli.Exit.some_error @@
  match !driver with
  | None -> invalid_arg "No driver set via B0_driver.set"
  | Some (d, main) ->
      has_b0_file := b0_file;
      Log.time begin fun _ m ->
        let b0_file = if b0_file then "with B0.ml" else "no B0.ml" in
        m "total time %s %s %s" d.name d.version b0_file
      end @@ fun () ->
      B00_cli.Exit.of_eval_result @@
      try main ()
      with B0_def.Scope.After_seal e ->
        (* FIXME I suspect we may never see this it will be catched
           by memo protection. At least make a good error msg. *)
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun m -> m ~header:"" "@[<v>%a@,@[%s@]@]" Fmt.backtrace bt e);
        `Ok Exit.b0_file_error

let has_b0_file () = !has_b0_file

module Compile = struct
  open B00_std.Fut.Syntax

  let build_dir c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver)

  let build_log c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver / "log")

  let exe c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver / "exe")

  let write_src m c src ~src_file  =
    let esrc = Memo.fail_if_error m (B0_file.expand src) in
    let reads = B0_file.expanded_file_manifest esrc in
    List.iter (Memo.file_ready m) reads;
    Memo.write m ~reads src_file @@ fun () ->
    Ok (B0_file.expanded_src esrc)

  let base_ext_libs =
    [ B00_ocaml.Lib.Name.v "cmdliner";
      B00_ocaml.Lib.Name.v "unix"; ]

  let base_libs =
    [ B00_ocaml.Lib.Name.v "b0.b00.std";
      B00_ocaml.Lib.Name.v "b0.b00";
      B00_ocaml.Lib.Name.v "b0.b00.kit";
      B00_ocaml.Lib.Name.v "b0";
      B00_ocaml.Lib.Name.v "b0.kit"; ]

  let find_libs m r libs =
    Fut.of_list @@ List.map (B00_ocaml.Lib.Resolver.get r) libs

  let find_boot_libs m ~clib_ext ~env libs r =
    match Os.Env.find ~empty_is_none:true "B0_BOOTSTRAP" with
    | None -> find_libs m r libs
    | Some bdir ->
        let bdir = Fpath.v bdir in
        let boot_lib name =
          let dir = Fpath.(bdir / B00_ocaml.Lib.Name.undot ~rep:'-' name) in
          let archive = Some (B00_ocaml.Lib.Name.to_archive_name name) in
          let* lib =
            B00_ocaml.Lib.of_dir m ~clib_ext ~name ~requires:[] ~archive ~dir
          in
          match lib with
          | Error _ as e -> Memo.fail_if_error m e
          | Ok lib -> Fut.return lib
        in
        Fut.of_list (List.map boot_lib libs)

  let find_libs m ocaml_conf ~build_dir ~driver ~requires =
    let* ocamlpath = B00_ocaml.Ocamlpath.get m None in
    let cache_dir = Fpath.(build_dir / "ocamlib") in
    let ocamlpath = B00_ocaml.Lib.Resolver.ocamlpath ~cache_dir ~ocamlpath in
    let ocamlfind = B00_ocaml.Lib.Resolver.ocamlfind ~cache_dir in
    let r = B00_ocaml.Lib.Resolver.create m ocaml_conf [ocamlpath; ocamlfind] in
    let requires = List.map fst requires in
    let* requires = find_libs m r requires in
    (* FIXME we are loosing locations here would be nice to
       have them to report errors. *)
    (* FIXME we likely also want a notion of ext lib for drivers *)
    let clib_ext = B00_ocaml.Conf.lib_ext ocaml_conf in
    let* driver_libs =
      find_boot_libs m ~clib_ext ~env:"B0_DRIVER_BOOTSTRAP" (libs driver) r
    in
    let* base_ext_libs = find_libs m r base_ext_libs in
    let* base_libs =
      find_boot_libs m ~clib_ext ~env:"B0_BOOTSTRAP" base_libs r
    in
    let all_libs = base_ext_libs @ base_libs @ driver_libs @ requires in
    let seen_libs = base_ext_libs @ base_libs @ requires in
    Fut.return (all_libs, seen_libs)

  let find_compiler c m = match Conf.code c with
  | Some (`Byte as c) -> Fut.return (B00_ocaml.Tool.ocamlc, c)
  | Some (`Native as c) -> Fut.return (B00_ocaml.Tool.ocamlopt, c)
  | None ->
      let* ocamlopt = B00.Memo.tool_opt m B00_ocaml.Tool.ocamlopt in
      match ocamlopt with
      | None -> Fut.return (B00_ocaml.Tool.ocamlc, `Byte)
      | Some comp -> Fut.return (B00_ocaml.Tool.ocamlopt, `Native)

  let compile_src m c ~driver ~build_dir src ~exe =
    let ocaml_conf = Fpath.(build_dir / "ocaml.conf") in
    let* comp, code = find_compiler c m in
    B00_ocaml.Conf.write m ~comp ~o:ocaml_conf;
    let* ocaml_conf = B00_ocaml.Conf.read m ocaml_conf in
    let obj_ext = B00_ocaml.Conf.obj_ext ocaml_conf in
    let comp = B00.Memo.tool m comp in
    let requires = B0_file.requires src in
    let* all_libs, seen_libs =
      find_libs m ocaml_conf ~build_dir ~driver ~requires
    in
    let src_file = Fpath.(build_dir / "src.ml") in
    write_src m c src ~src_file;
    let writes =
      let base = Fpath.rem_ext src_file in
      let base ext = Fpath.(base + ext) in
      match code with
      | `Byte -> [base ".cmo"; exe ]
      | `Native -> [base ".cmx"; base obj_ext; exe ]
    in
    let dirs = List.map B00_ocaml.Lib.dir seen_libs in
    let incs = Cmd.unstamp @@ Cmd.paths ~slip:"-I" dirs in
    let archives =
      let ar = match code with
      | `Native -> B00_ocaml.Lib.cmxa
      | `Byte -> B00_ocaml.Lib.cma
      in
      List.filter_map ar all_libs
    in
    let c_archives = List.filter_map B00_ocaml.Lib.c_archive all_libs in
    let ars = List.rev_append archives c_archives in
    (* FIXME this should be done b the resolver *)
    List.iter (B00.Memo.file_ready m) ars;
    let reads = src_file :: ars in
    B00.Memo.spawn m ~reads ~writes @@
    comp Cmd.(atom "-linkall" % "-g" % "-o" %% unstamp (path exe) % "-opaque" %%
              incs %% (unstamp @@ (paths archives %% path src_file)));
    Fut.return ()

  let write_log_file ~log_file m =
    Log.if_error ~use:() @@ B00_cli.Memo.Log.(write log_file (of_memo m))

  let compile c ~driver src =
    Result.bind (Conf.memo c) @@ fun m ->
    let build_dir = build_dir c ~driver in
    let log_file = build_log c ~driver in
    let exe = exe c ~driver in
    (* That shit should be streamlined: brzo, odig, b0caml all
       have similar setup/log/reporting bits. *)
    Os.Exit.on_sigint
      ~hook:(fun () -> write_log_file ~log_file m) @@ fun () ->
    B00.Memo.run_proc m begin fun () ->
      let* () = B00.Memo.delete m build_dir in
      let* () = B00.Memo.mkdir m build_dir in
      compile_src m c ~driver ~build_dir src ~exe
    end;
    B00.Memo.stir ~block:true m;
    write_log_file ~log_file m;
    match B00.Memo.status m with
    | Ok () -> Ok exe
    | Error e ->
        let name = name driver in
        let dopt = if name = "b0" then "" else Fmt.str " --driver %s" name in
        let read_howto ppf _ = Fmt.pf ppf "b0 file log%s -r " dopt in
        let write_howto ppf _ = Fmt.pf ppf "b0 file log%s -w " dopt in
        B000_conv.Op.pp_aggregate_error
          ~read_howto ~write_howto () Fmt.stderr e;
        Fmt.error "Could not compile B0 file %a"
          Fmt.(code Fpath.pp_unquoted) (B0_file.file src)
end

let with_b0_file ~driver cmd =
 let open B00_std.Result.Syntax in
  let run conf cmd = match has_b0_file () with
  | true -> cmd conf
  | false ->
      Log.if_error ~use:Exit.no_b0_file @@
      let* b0_file = Conf.get_b0_file conf in
      Log.if_error' ~use:Exit.b0_file_error @@
      let* s = Os.File.read b0_file in
      let* src = B0_file.of_string ~file:b0_file s in
      let* exe = Compile.compile conf ~driver src in
      let argv = Cmd.of_list (fun x -> x) (Array.to_list Sys.argv) in
      Ok (Os.Exit.exec exe argv)
  in
  Cmdliner.Term.(pure run $ Cli.conf $ cmd)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
