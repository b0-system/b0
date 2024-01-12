(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

module Exit = struct
  open Cmdliner

  let deploy_error = Os.Exit.Code 118
  let build_error = Os.Exit.Code 119
  let b0_file_error = Os.Exit.Code 120
  let no_b0_file = Os.Exit.Code 121

  let e c doc = Cmd.Exit.info (Os.Exit.get_code c) ~doc
  let info_deploy_error = e deploy_error "on deploy error."
  let info_build_error = e build_error "on build error."
  let info_b0_file_error = e b0_file_error "on B0 file error."
  let info_no_b0_file = e no_b0_file "no B0 file found."

  let infos =
    info_deploy_error :: info_build_error ::
    info_b0_file_error :: info_no_b0_file ::
    B0_cli.Exit.infos
end

module Env = struct
  let b0_dir = B0_cli.Memo.b0_dir_env
  let b0_file = "B0_FILE"
  let cache_dir = B0_cli.Memo.cache_dir_env
  let color = "B0_COLOR"
  let code = "B0_DRIVER_CODE"
  let hash_fun = B0_cli.Memo.hash_fun_env
  let jobs = B0_cli.Memo.jobs_env
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
      env : Os.Env.t;
      code : B0_ocaml.Code.t option;
      hash_fun : (module Hash.T);
      jobs : int;
      log_level : Log.level;
      no_pager : bool;
      memo : (B0_memo.t, string) result Lazy.t;
      tty_cap : Tty.cap; }

  let memo ~hash_fun ~cwd ~env ~cache_dir ~trash_dir ~jobs =
    let feedback =
      let op_howto ppf o = Fmt.pf ppf "b0 file log --id %d" (B0_zero.Op.id o) in
      let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
      B0_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
        Fmt.stderr
    in
    B0_memo.make ~hash_fun ~cwd ~env ~cache_dir ~trash_dir ~jobs ~feedback ()

  let make
      ~b0_dir ~b0_file ~cache_dir ~cwd ~code ~env ~hash_fun ~jobs
      ~log_level ~no_pager ~tty_cap ()
    =
    let trash_dir = Fpath.(b0_dir / B0_cli.Memo.trash_dir_name) in
    let memo = lazy (memo ~hash_fun ~cwd ~env ~cache_dir ~trash_dir ~jobs) in
    { b0_dir; b0_file; cache_dir; cwd; code; env; hash_fun; jobs;
      memo; log_level; no_pager; tty_cap }

  let b0_dir c = c.b0_dir
  let b0_file c = c.b0_file
  let cache_dir c = c.cache_dir
  let cwd c = c.cwd
  let code c = c.code
  let env c = c.env
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
    let tty_cap = B0_cli.B0_std.get_tty_cap tty_cap in
    let log_level = B0_cli.B0_std.get_log_level log_level in
    B0_cli.B0_std.setup tty_cap log_level ~log_spawns:Log.Debug;
    let* cwd = Os.Dir.cwd () in
    let* env = Os.Env.current () in
    let b0_file = find_b0_file ~cwd ~b0_file in
    let root = match b0_file with Some f -> Fpath.parent f | None -> cwd  in
    let b0_dir = B0_cli.Memo.get_b0_dir ~cwd ~root ~b0_dir in
    let cache_dir = B0_cli.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
    let hash_fun = B0_cli.Memo.get_hash_fun ~hash_fun in
    let jobs = B0_cli.Memo.get_jobs ~jobs in
    Ok (make ~b0_dir ~b0_file ~cache_dir ~cwd ~code ~env ~hash_fun
          ~jobs ~log_level ~no_pager ~tty_cap ())
end

module Cli = struct
  open Cmdliner

  let docs = Manpage.s_common_options
  let b0_dir = B0_cli.Memo.b0_dir ~docs ()
  let b0_file =
    let env = Cmd.Env.info Env.b0_file in
    let doc = "Use $(docv) as the B0 file." and docv = "PATH" in
    let absent = "$(b,B0.ml) file in cwd or first upwards" in
    Arg.(value & opt (Arg.some B0_cli.fpath) None &
         info ["b0-file"] ~absent ~doc ~docv ~docs ~env)

  let cache_dir = B0_cli.Memo.cache_dir ~docs ()
  let code =
    let env = Cmd.Env.info Env.code in
    let code_enum = ["byte", Some `Byte; "native", Some `Native; "auto", None]in
    let code = Arg.enum code_enum in
    let docv = "CODE" in
    let doc =
      "Compile driver to $(docv). $(docv) must be $(b,byte), $(b,native) or \
       $(b,auto). If $(b,auto) favors native code if available."
    in
    Arg.(value & opt code None & info ["driver-code"] ~doc ~docv ~docs ~env)

  let hash_fun = B0_cli.Memo.hash_fun ~docs ()
  let jobs = B0_cli.Memo.jobs ~docs ()
  let log_level =
    B0_cli.B0_std.log_level ~docs ~env:(Cmd.Env.info Env.verbosity) ()

  let tty_cap = B0_cli.B0_std.tty_cap ~docs ~env:(Cmd.Env.info Env.color) ()
  let no_pager = B0_pager.don't ~docs ()
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

type main =
  unit -> (Os.Exit.t Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result

type t =
  { name : string;
    version : string;
    libs : B0_ocaml.Libname.t list }

let make ~name ~version ~libs = { name; version; libs }
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
  B0_cli.Exit.exit ~exec_error:B0_cli.Exit.some_error @@
  match !driver with
  | None -> invalid_arg "No driver set via B0_driver.set"
  | Some (d, main) ->
      has_b0_file := b0_file;
      Log.time begin fun _ m ->
        let b0_file = if b0_file then "with B0.ml" else "no B0.ml" in
        m "total time %s %s %s" d.name d.version b0_file
      end @@ fun () ->
      B0_cli.Exit.of_eval_result @@
      try main ()
      with B0_scope.After_seal e ->
        (* FIXME I suspect we may never see this it will be catched
           by memo protection. At least make a good error msg. *)
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun m -> m ~header:"" "@[<v>%a@,@[%s@]@]" Fmt.backtrace bt e);
        Ok (`Ok Exit.b0_file_error)

let has_b0_file () = !has_b0_file

module Compile = struct
  open B0_std.Fut.Syntax

  let build_dir c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver)

  let build_log c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver / "log")

  let exe c ~driver =
    Fpath.(Conf.b0_dir c / Conf.drivers_dir_name / name driver / "exe")

  let write_src m c expanded_src ~file_api_stamp ~src_file =
    let src_reads = B0_file.expanded_file_manifest expanded_src in
    let reads = List.rev_append file_api_stamp src_reads in
    List.iter (B0_memo.file_ready m) src_reads;
    B0_memo.write m ~reads src_file @@ fun () ->
    Ok (B0_file.expanded_src expanded_src)

  let base_ext_libs =
    [ B0_ocaml.Libname.v "cmdliner";
      B0_ocaml.Libname.v "unix"; ]

  let b0_file_lib = B0_ocaml.Libname.v "b0.file"
  let base_libs =
    [ B0_ocaml.Libname.v "b0.std";
      B0_ocaml.Libname.v "b0.memo";
      b0_file_lib;
      B0_ocaml.Libname.v "b0.kit"; ]

  let find_libs m r libs =
    Fut.of_list @@ List.map (B0_ocaml.Libresolver.get r) libs

  let find_boot_libs m ~clib_ext ~env libs r =
    match Os.Env.find ~empty_is_none:true "B0_BOOTSTRAP" with
    | None -> find_libs m r libs
    | Some bdir ->
        let bdir = Fpath.v bdir in
        let boot_lib libname =
          let dir = Fpath.(bdir / B0_ocaml.Libname.undot ~rep:'-' libname) in
          let archive = Some (B0_ocaml.Libname.to_archive_name libname) in
          let* lib =
            B0_ocaml.Lib.of_dir m ~clib_ext ~libname ~requires:[] ~archive ~dir
              ~js_stubs:[]
          in
          match lib with
          | Error _ as e -> B0_memo.fail_if_error m e
          | Ok lib -> Fut.return lib
        in
        Fut.of_list (List.map boot_lib libs)

  let find_libs m ocaml_conf ~build_dir ~driver ~requires =
    let cache_dir =
      Fpath.(build_dir / B0_ocaml.Libresolver.Scope.cache_dir_name)
    in
    (* let ocamlpath = B0_ocaml.Libresolver.Scope.ocamlpath ~cache_dir in *)
    let ocamlfind = B0_ocaml.Libresolver.Scope.ocamlfind ~cache_dir in
    let scopes = [(*ocamlpath;*) ocamlfind] in
    let r = B0_ocaml.Libresolver.make m ocaml_conf scopes in
    let requires = List.map fst requires in
    let* requires = find_libs m r requires in
    (* FIXME we are loosing locations here would be nice to
       have them to report errors. *)
    (* FIXME we likely also want a notion of ext lib for drivers *)
    let clib_ext = B0_ocaml.Conf.lib_ext ocaml_conf in
    let* driver_libs =
      find_boot_libs m ~clib_ext ~env:"B0_DRIVER_BOOTSTRAP" (libs driver) r
    in
    let* base_ext_libs = find_libs m r base_ext_libs in
    let* base_libs =
      find_boot_libs m ~clib_ext ~env:"B0_BOOTSTRAP" base_libs r
    in
    let b0_file_lib =
      let is_b0_file_lib l =
        B0_ocaml.Libname.equal (B0_ocaml.Lib.libname l) b0_file_lib
      in
      List.find_opt is_b0_file_lib base_libs |> Option.get
    in
    let all_libs = base_ext_libs @ base_libs @ driver_libs @ requires in
    let seen_libs = base_ext_libs @ base_libs @ requires in
    Fut.return (b0_file_lib, all_libs, seen_libs)

  let find_compiler c m = match Conf.code c with
  | Some (`Byte as c) -> Fut.return (B0_ocaml.Tool.ocamlc, c)
  | Some (`Native as c) -> Fut.return (B0_ocaml.Tool.ocamlopt, c)
  | None ->
      let* ocamlopt = B0_memo.tool_opt m B0_ocaml.Tool.ocamlopt in
      match ocamlopt with
      | None -> Fut.return (B0_ocaml.Tool.ocamlc, `Byte)
      | Some comp -> Fut.return (B0_ocaml.Tool.ocamlopt, `Native)

  let compile_src m c ~driver ~build_dir src ~exe =
    let ocaml_conf = Fpath.(build_dir / "ocaml.conf") in
    let* comp, code = find_compiler c m in
    B0_ocaml.Conf.write m ~comp ~o:ocaml_conf;
    let* ocaml_conf = B0_ocaml.Conf.read m ocaml_conf in
    let obj_ext = B0_ocaml.Conf.obj_ext ocaml_conf in
    let comp = B0_memo.tool m comp in
    let expanded_src = B0_memo.fail_if_error m (B0_file.expand src) in
    let requires = B0_file.expanded_requires expanded_src in
    let* b0_file_lib, all_libs, seen_libs =
      find_libs m ocaml_conf ~build_dir ~driver ~requires
    in
    let src_file = Fpath.(build_dir / "src.ml") in
    let file_api_stamp = match code with (* archive changes when API does *)
    | `Byte -> Option.to_list (B0_ocaml.Lib.cma b0_file_lib)
    | `Native -> Option.to_list (B0_ocaml.Lib.cmxa b0_file_lib)
    in
    write_src m c expanded_src ~file_api_stamp ~src_file;
    let writes =
      let base = Fpath.strip_ext src_file in
      let base ext = Fpath.(base + ext) in
      match code with
      | `Byte -> [base ".cmo"; exe ]
      | `Native -> [base ".cmx"; base obj_ext; exe ]
    in
    let dirs = List.map B0_ocaml.Lib.dir seen_libs in
    let incs = Cmd.unstamp @@ Cmd.paths ~slip:"-I" dirs in
    let archives =
      let ar = match code with
      | `Native -> B0_ocaml.Lib.cmxa
      | `Byte -> B0_ocaml.Lib.cma
      in
      List.filter_map ar all_libs
    in
    let c_archives = List.filter_map B0_ocaml.Lib.c_archive all_libs in
    let ars = List.rev_append archives c_archives in
    (* FIXME this should be done b the resolver *)
    List.iter (B0_memo.file_ready m) ars;
    let reads = src_file :: ars in
    B0_memo.spawn m ~reads ~writes @@
    comp Cmd.(arg "-linkall" % "-g" % "-o" %% unstamp (path exe) % "-opaque" %%
              incs %% (unstamp @@ (paths archives %% path src_file)));
    Fut.return ()

  let write_log_file ~log_file m =
    Log.if_error ~use:() @@ B0_cli.Memo.Log.(write log_file (of_memo m))

  let compile c ~driver ~feedback src =
    Result.bind (Conf.memo c) @@ fun m ->
    let m = if feedback then m else B0_memo.with_feedback m ignore in
    let build_dir = build_dir c ~driver in
    let log_file = build_log c ~driver in
    let exe = exe c ~driver in
    (* That shit should be streamlined: brzo, odig, b0caml all
       have similar setup/log/reporting bits. *)
    Os.Exit.on_sigint
      ~hook:(fun () -> write_log_file ~log_file m) @@ fun () ->
    B0_memo.run_proc m begin fun () ->
      let* () = B0_memo.delete m build_dir in
      let* () = B0_memo.mkdir m build_dir in
      compile_src m c ~driver ~build_dir src ~exe
    end;
    B0_memo.stir ~block:true m;
    write_log_file ~log_file m;
    match B0_memo.status m with
    | Ok () -> Ok exe
    | Error e ->
        let name = name driver in
        let dopt = if name = "b0" then "" else Fmt.str " --driver %s" name in
        let read_howto ppf _ = Fmt.pf ppf "b0 file log%s -r " dopt in
        let write_howto ppf _ = Fmt.pf ppf "b0 file log%s -w " dopt in
        if feedback then
          B0_zero_conv.Op.pp_aggregate_error
            ~read_howto ~write_howto () Fmt.stderr e;
        Fmt.error "Could not compile B0 file %a"
          Fmt.(code Fpath.pp) (B0_file.file src)
end

let compile_b0_file conf ~driver ~feedback b0_file =
  let* src = Os.File.read b0_file in
  let* src = B0_file.of_string ~file:b0_file src in
  Compile.compile conf ~driver ~feedback src

let with_b0_file ~driver cmd =
  let run conf cmd =
    if has_b0_file () then cmd conf else
    Log.if_error ~use:Exit.no_b0_file @@
    let* b0_file = Conf.get_b0_file conf in
    Log.if_error' ~use:Exit.b0_file_error @@
    let* exe = compile_b0_file conf ~driver ~feedback:true b0_file in
    let argv = Cmd.list (Array.to_list Sys.argv) in
    Ok (Os.Exit.exec exe argv)
  in
  Cmdliner.Term.(const run $ Cli.conf $ cmd)

let has_failed_b0_file = ref false

let with_b0_file_if_any ~driver cmd =
  let run conf cmd =
    if has_b0_file () then cmd conf else
    match Conf.b0_file conf with
    | None -> cmd conf
    | Some b0_file ->
        match compile_b0_file conf ~driver ~feedback:false b0_file with
        | Error e ->
            (Log.warn @@ fun m -> m "%s. See %a." e Fmt.code' "b0 file log -e");
            has_failed_b0_file := true;
            cmd conf
        | Ok exe ->
            let argv = Cmd.list (Array.to_list Sys.argv) in
            Os.Exit.exec exe argv
  in
  Cmdliner.Term.(const run $ Cli.conf $ cmd)

let has_failed_b0_file () = !has_failed_b0_file
