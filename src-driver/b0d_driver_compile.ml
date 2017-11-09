(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let failwith_error = function Error (`Msg m) -> failwith m | Ok v -> v

(* Compiler lookup *)

let find_tool descr_field arg ~absent cli descr =
  let tool = match arg cli with
  | Some tool -> tool
  | None ->
      match descr_field descr with
      | Some tool -> tool
      | None -> absent
  in
  match OS.Cmd.which_raw tool with
  | None -> R.error_msgf "%s: Command not found." tool
  | Some tool -> Ok tool

let find_ocamlc cli descr =
  find_tool
    B0d_root_descr.ocamlc B0d_driver_cli.ocamlc ~absent:"ocamlc" cli descr

let find_ocamlopt cli descr =
  find_tool
    B0d_root_descr.ocamlopt B0d_driver_cli.ocamlopt ~absent:"ocamlopt.opt"
    cli descr

let compile_kind cli descr = match B0d_driver_cli.compile_kind cli with
| Some kind -> Ok kind
| None ->
    match B0d_root_descr.compile_kind descr with
    | `Native _ -> Ok `Native
    | `Byte _ -> Ok `Byte
    | `Auto -> Ok `Auto
    | `Conflict (n, b) ->
        R.error_msgf "@[<v>description conflict, byte code needed for:@,\
                      %a@,\
                      native code deeded for:@,%a@,@]"
          (Format.pp_print_list Fpath.pp) b
          (Format.pp_print_list Fpath.pp) n

let find_compiler cli descr = compile_kind cli descr >>= function
| `Native -> find_ocamlopt cli descr >>| fun c -> `Native, c
| `Byte -> find_ocamlc cli descr >>| fun c -> `Byte, c
| `Auto ->
    match find_ocamlopt cli descr with
    | Ok c -> Ok (`Native, c)
    | Error (`Msg mn) ->
        match find_ocamlc cli descr with
        | Ok c -> Ok (`Byte, c)
        | Error (`Msg mb) -> R.error_msgf "No compiler found:\n%s\n%s" mn mb

(* Compiler flags *)

let cflags cli descr compile =
  let compile_args = Cmd.of_list @@ B0d_driver_cli.compile cli in
  let compile_descr = Cmd.of_list @@ B0d_root_descr.compile descr in
  let compile_kind = Cmd.of_list @@ match compile with
  | `Native, _ -> B0d_root_descr.compile_native descr
  | `Byte, _ -> B0d_root_descr.compile_byte descr
  in
  Cmd.(compile_args %% compile_descr %% compile_kind)

let lflags cli descr compile =
  let link_args = Cmd.of_list @@ B0d_driver_cli.link cli in
  let link_descr = Cmd.of_list @@ B0d_root_descr.link descr in
  let link_kind = Cmd.of_list @@ match compile with
  | `Native, _ -> B0d_root_descr.link_native descr
  | `Byte, _ -> B0d_root_descr.link_byte descr
  in
  Cmd.(link_args %% link_descr %% link_kind)

let lib_flags libs =
  let rec loop acc = function
  | [] -> Cmd.of_rev_list acc
  | lib :: libs ->
      let inc = Fpath.(to_string (parent lib)) in
      loop (Fpath.to_string lib :: inc :: "-I" :: acc) libs
  in
  loop [] libs

(* Library lookup

   The __B0_DEV_DIR env variable is used during the development of
   b0 to lookup the build directories rather than the install base.
   FIXME Generalize this to allow general driver dev.
   FIXME Try to reuse with what will be done for B0_ocaml *)

let b0_dev_dir = match OS.Env.var "__B0_DEV_DIR" with
| None -> None
| Some dir -> Some (Fpath.v dir)

let compiler_conf compiler =
  let compiler, lib_ext = match compiler with
  | `Native, compiler -> compiler, ".cmxa"
  | `Byte, compiler -> compiler, ".cma"
  in
  OS.Cmd.(run_out Cmd.(v compiler % "-where") |> to_string)
  >>= fun olib -> Fpath.of_string olib
  >>| fun olib ->
  (* FIXME Windows. Rather use -config and parse standard_library and ext_lib *)
  Fpath.parent olib, lib_ext, ".a"

let lookup_b0_dev libdir lib_ext lib = match b0_dev_dir with
| None -> None
| Some dir ->
    (* FIXME can we do better than this *)
    match lib with
    | "b0.std" ->
        Some (Fpath.(dir // v "_build/src-std/"), strf "b0_std%s" lib_ext)
    | "b0" ->
        Some (Fpath.(dir // v "_build/src-lib/"), strf "b0%s" lib_ext)
    | "b0.driver" ->
        Some (Fpath.(dir // v "_build/src-driver/"), strf "b0_driver%s" lib_ext)
    | "b0.care" ->
        Some (Fpath.(dir // v "_build/src-care/"), strf "b0_care%s" lib_ext)
    | "b0.b0" ->
        Some (Fpath.(dir // v "_build/src-b0/"), strf "b0_b0%s" lib_ext)
    | "d0" ->
        Some (Fpath.(dir // v "_build/src-d0/"), strf "d0%s" lib_ext)
    | _ -> None

let file_exists dir file =
  let path = Fpath.(dir / file) in
  let exists = OS.File.exists path |> failwith_error in
  if exists then Some path else None

let lookup_lib libdir lib_ext lib =
  let dir, archive = match lookup_b0_dev libdir lib_ext lib with
  | Some (dir, archive) -> dir, archive
  | None ->
      match lib with
      | "unix" -> Fpath.(libdir / "ocaml"), (strf "unix%s" lib_ext)
      | lib ->
          match String.cut ~sep:"." lib with
          | None -> Fpath.(libdir / lib), (strf "%s%s" lib lib_ext)
          | Some (l, sub) -> Fpath.(libdir / l), (strf "%s_%s%s" l sub lib_ext)
  in
  match file_exists dir archive with
  | Some lib -> `Resolved lib
  | None -> `Unresolved Fpath.(dir / archive)

let lookup_libs lookup_lib ~omit libs =
  let rec loop r u = function
  | [] -> if u = [] then `Resolved (List.rev r) else `Unresolved (List.rev u)
  | l :: ls ->
      match lookup_lib l with
      | `Unresolved lib -> loop r (lib :: u) ls
      | `Resolved lib when String.Set.mem l omit -> loop r u ls
      | `Resolved lib -> loop (lib :: r) u ls
  in
  loop [] [] libs

(* Description source *)

let src_of_file_spec file =
  let modname_of_file file =
    let fname = Fpath.filename file in
    let name = match String.rindex fname '.' with
    | exception Not_found -> fname
    | i -> String.with_index_range fname ~last:(i - 1)
    in
    String.capitalize_ascii name
  in
  let read_src file = OS.File.read file |> failwith_error in
  match Fpath.has_ext ".ml" file with
  | true ->
      let modname = modname_of_file file in
      strf "module %s = struct\n\
            let () = B0.Def.Loc.(set_current (file (B0.Fpath.v %S)))\n\
            #1 \"%s\"\n\
            %s\n\
            end"
        modname
        (Fpath.to_string file) (Fpath.to_string file) (read_src file)
  | false ->
      let modname = String.capitalize_ascii (Fpath.filename file) in
      let mli_file = Fpath.(file + ".mli") in
      let ml_file = Fpath.(file + ".ml") in
      strf "module %s : sig\n\
            #1 \"%s\"\n\
            %s\n\
            end = struct\n\
            let () = B0.Def.Loc.(set_current (file (B0.Fpath.v %S)))\n\
            #1 \"%s\"\n\
            %s\n\
            end"
        modname
        (Fpath.to_string mli_file) (read_src mli_file)
        (Fpath.to_string ml_file)
        (Fpath.to_string ml_file) (read_src ml_file)

let descr_src descr lib_lookup driver_libs =
  let omit = B0d_root_descr.drop_libs descr in
  let rec loop i root rlibs = function
  | [] ->
      let root =
        "let () = B0_driver.Driver.instance_main ()" ::
        (* FIXME pass the root to instances
           "let () = B0.Def.Loc.set_root None" :: *) root
      in
      (String.concat "\n" (List.rev root), List.rev rlibs)
  | (dfile, srcs) :: subs ->
      let rec add_srcs root rlibs = function
      | [] -> root, rlibs
      | (file, libs, _) :: srcs ->
          match lookup_libs lib_lookup ~omit libs with
          | `Unresolved _ -> add_srcs root rlibs srcs
          | `Resolved libs ->
              let root = src_of_file_spec file :: root in
              let rlibs = List.rev_append libs rlibs in
              add_srcs root rlibs srcs
      in
      let start =
        strf "module Sub_descr_%d : sig end = struct\n\
              let () = B0.Def.Loc.set_sub_root (Some (B0.Fpath.v %S))"
          i (Fpath.to_string @@ Fpath.parent @@ dfile)
      in
      let root = start :: root in
      let root, rlibs = add_srcs root rlibs srcs in
      let root = "" :: "" :: strf "end (* of Sub_descr_%d *)" i :: root in
      loop (i + 1) root rlibs subs
  in
  try
    let start =
      [ ""; strf "let () = B0.Def.Loc.set_root (Some (B0.Fpath.v %S))"
          (Fpath.to_string (B0d_root_descr.dir descr)) ]
    in
    let root_src, libs = loop 0 start [] (B0d_root_descr.srcs descr) in
    match lookup_libs lib_lookup ~omit driver_libs with
    | `Resolved dlibs -> Ok (root_src, Fpath.uniquify @@ List.append dlibs libs)
    | `Unresolved core ->
        R.error_msgf "missing libraries: %s"
          (String.concat ", " @@ List.map Fpath.to_string core)
  with
  | Failure m -> R.error_msg m

(* Instance compilation action *)

let core_libs_rev =
  [ "b0.driver"; "cmdliner"; "b0.care"; "b0"; "b0.std"; "unix" ]

type action =
  { cli : B0d_driver_cli.t;
    descr : B0d_root_descr.t;
    driver_libs : string list;
    driver_dir : Fpath.t;
    driver_name : string;
    bin : Fpath.t;
    compiler : [`Byte | `Native ] * string;
    clib_ext : string;
    cflags : Cmd.t;
    lflags : Cmd.t;
    src : string;
    libs : Fpath.t list; }

let action cli descr ~driver_dir ~driver_name ~driver_libs ~bin =
  find_compiler cli descr
  >>= fun compiler -> compiler_conf compiler
  >>= fun (libdir, lib_ext, clib_ext) ->
  let lookup_lib = lookup_lib libdir lib_ext in
  let libs = List.rev_append core_libs_rev driver_libs in
  descr_src descr lookup_lib libs
  >>= fun (src, libs) ->
  let cflags = cflags cli descr compiler in
  let lflags = lflags cli descr compiler in
  Ok { cli; descr; driver_libs; driver_dir; driver_name; bin;
       compiler; clib_ext; cflags; lflags; src; libs }

(* FIXME reuse B0 funs, also hash OCAMLLIB OCAMLPARAM etc. *)

let action_stamp a =
  let hash_file_to_bytes f = Hash.(to_byte_string @@ file f) in
  let byte_lib_stamp acc l = hash_file_to_bytes l :: acc in
  let native_lib_stamp acc l =
    hash_file_to_bytes l :: hash_file_to_bytes Fpath.(l -+ a.clib_ext) :: acc
  in
  let hash_lib = match fst a.compiler with
  | `Native -> native_lib_stamp
  | `Byte -> byte_lib_stamp
  in
  let cstamp = Hash.(to_byte_string @@ raw_file (snd a.compiler)) in
  let lib_stamps = List.fold_left hash_lib [] a.libs in
  Ok (Hash.to_byte_string @@ Hash.string @@
      String.concat "" @@
      (a.src :: cstamp ::
       (List.rev_append lib_stamps @@
        List.rev_append (Cmd.to_rev_list a.cflags) @@
        Cmd.to_rev_list a.lflags)))

let compile a =
  let driver_dir = a.driver_dir in
  let driver_name = a.driver_name in
  let src_file = Fpath.(driver_dir / strf "%si.ml" driver_name) in
  let lib_flags = lib_flags a.libs in
  let cmd =
    Cmd.(v (snd a.compiler) % "-o" % p a.bin % "-linkall" %%
         a.cflags %% a.lflags %% lib_flags % p src_file)
  in
  OS.Dir.create a.driver_dir
  >>= fun _ -> OS.File.write src_file a.src
  >>= fun () -> OS.Cmd.run cmd

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
