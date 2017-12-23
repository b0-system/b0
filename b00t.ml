#!/usr/bin/env ocaml

(* Usage: ocaml b00t.ml [reset|cold|build|full]
   See DEVEL.md *)

let root_dir = Sys.getcwd ()
let absolute p = String.concat "" [root_dir; "/"; p]
let fpath ?(ext = "") ~dir f = String.concat "" [dir; "/"; f; ext]

let boot_dir = absolute "_boot"
let libs = ["result"; "cmdliner"]
let b0_ml_src = fpath ~dir:(absolute "B0.d") "B0.ml"
let b0_lib_dirs = ["src-std"; "src-lib"; "src-care"; "src-driver"; "src-b0"]
let b0_cstubs_dirs = ["src-std"]
let exe = (fpath ~dir:boot_dir "b0") ^ (if Sys.win32 then ".exe" else "")

(* Logging *)

let strf = Printf.sprintf
let err fmt = Printf.kfprintf (fun oc -> flush oc; exit 1) stderr fmt
let log fmt = Printf.kfprintf (fun oc -> flush oc) stdout fmt

(* The running joke *)

let rev_cut ~sep s = match String.rindex s sep with
| exception Not_found -> None
| i -> String.(Some (sub s 0 i, sub s (i + 1) (length s - (i + 1))))

let cuts ~sep s =
  let rec loop acc = function
  | "" -> acc
  | s ->
      match rev_cut ~sep s with
      | None -> s :: acc
      | Some (l, r) -> loop (r :: acc) l
  in
  loop [] s

(* Read, write and collect files *)

let string_of_file f =
  let ic = open_in_bin f in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let string_to_file f s =
  let oc = open_out_bin f in
  output_string oc s;
  close_out oc

let files_with_ext ~ext dirs =
  let add_file dir acc f = match rev_cut ~sep:'.' f with
  | Some (m, e) when e = ext -> (fpath ~dir f) :: acc
  | Some _ | None -> acc
  in
  let add_dir acc dir = Array.fold_left (add_file dir) acc (Sys.readdir dir) in
  List.fold_left add_dir [] dirs

(* Finding and running commands *)

let find_cmd cmds =
  let test, null = match Sys.win32 with
  | true -> "where", " NUL"
  | false -> "type", "/dev/null"
  in
  let cmd c = Sys.command (strf "%s %s 1>%s 2>%s" test c null null) = 0 in
  try Some (List.find cmd cmds) with Not_found -> None

let err_cmd exit cmd = err "exited with %d: %s\n" exit cmd
let quote_cmd = match Sys.win32 with
| false -> fun cmd -> cmd
| true -> fun cmd -> strf "\"%s\"" cmd

let run_cmd args =
  let cmd = String.concat " " (List.map Filename.quote args) in
(*  log "[EXEC] %s\n" cmd; *)
  let exit = Sys.command (quote_cmd cmd) in
  if exit = 0 then () else err_cmd exit cmd

let read_cmd args =
  let stdout = Filename.temp_file (Filename.basename Sys.argv.(0)) "b00t" in
  at_exit (fun () -> try ignore (Sys.remove stdout) with _ -> ());
  let cmd = String.concat " " (List.map Filename.quote args) in
  let cmd = quote_cmd @@ strf "%s 1>%s" cmd (Filename.quote stdout) in
  let exit = Sys.command cmd in
  if exit = 0 then string_of_file stdout else err_cmd exit cmd

(* Looking up OCaml libraries *)

let libdir = try Sys.getenv "B0_DRIVER_LIBDIR" with
| Not_found ->
    try fpath ~dir:(Sys.getenv "OPAM_SWITCH_PREFIX") "lib" with
    | Not_found ->
        err "No OCaml library installation prefix found.\n\
             Set the B0_DRIVER_LIBDIR environment variable to a library\n\
             directory or invoke 'eval $(opam env)'."

let lib_dir lib = fpath ~dir:libdir lib
let lib_ar ext lib = fpath ~dir:libdir (fpath ~dir:lib lib ~ext)

(* Lookup an OCaml compiler and ocamldep *)

let byt_comp = find_cmd ["ocamlc.opt"; "ocamlc"]
let nat_comp = find_cmd ["ocamlopt.opt"; "ocamlopt"]
let comp, comp_kind, ar_ext = match nat_comp with
| Some comp -> comp, `Native, ".cmxa"
| None ->
    match byt_comp with
    | Some comp -> comp, `Byte, ".cma"
    | None -> err "No OCaml (byte or native) compiler found in PATH\n"

let ocamldep = match find_cmd ["ocamldep.opt"; "ocamldep"] with
| None -> err "No ocamldep found in PATH\n"
| Some dep -> dep

(* Source of b0 driver instance of b0

   We concatenate the sorted source files of each B0 library along
   with the B0.ml description of b0 into a single driver instance
   source which is compiled and linked along with the C stub sources
   in a single step in the _boot directory. The latter exists in the
   repo to avoid having to mkdir which Sys doesn't provide. *)

let sorted_lib_srcs dirs =
  (* We don't use absolute paths yet here because ocamldep -sort doesn't
     support paths with spaces, see MPR6968. *)
  let srcs = files_with_ext ~ext:"ml" dirs in
  read_cmd (ocamldep :: "-slash" :: "-sort" :: srcs)
  |> String.trim |> cuts ~sep:' '

let add_ml_module buf ml_file =
  let get_rev_cut ~sep s = match rev_cut ~sep s with
  | None -> assert false | Some (l, r) -> (l, r)
  in
  let ml = absolute ml_file in
  let mli = ml ^ "i" in
  let mod_name =
    let f, _ext = get_rev_cut ~sep:'.' ml_file in
    let _, base = get_rev_cut ~sep:'/' f in
    String.capitalize_ascii base
  in
  Buffer.add_string buf @@ strf
    "module %s : sig\n#1 %S\n%s\nend = struct\n#1 %S\n%s\nend\n"
    mod_name mli (string_of_file mli) ml (string_of_file ml)

let add_b0_ml_src buf src =
  let instance_src =
    strf "let () = B0.Def.Loc.set_root (Some (B0.Fpath.v %S))\n\
          let () = B0.Def.Loc.set_sub_root (Some (B0.Fpath.v %S))\n\
          let () = B0.Def.Loc.(set_current (file (B0.Fpath.v %S)))\n\
          #1 %S\n\
          %s\n\
          let () = B0_driver.Driver.instance_main ()\n"
      root_dir root_dir src src (string_of_file src)
  in
  Buffer.add_string buf instance_src

let instance_src () =
  let buf = Buffer.create (2 * 1024 * 1024) in
  List.iter (add_ml_module buf) (sorted_lib_srcs b0_lib_dirs);
  add_b0_ml_src buf b0_ml_src;
  Buffer.contents buf

(* Compile instance in _boot *)

let add_inc i acc = "-I" :: i :: acc

let cstubs_args cstubs_dirs =
  let cstubs_dirs = List.map absolute cstubs_dirs in
  let srcs = files_with_ext ~ext:"c" cstubs_dirs in
  let args = List.fold_right add_inc cstubs_dirs srcs in
  match comp_kind with `Byte -> "-custom" :: args | `Native -> args

let compile_instance () =
  log "Compiling b0 driver instance for b0 with %s\n" comp;
  let incs = List.fold_right add_inc (boot_dir :: List.map lib_dir libs) [] in
  let lib_ars = ("unix" ^ ar_ext) :: List.map (lib_ar ar_ext) libs in
  let cstubs_args = cstubs_args b0_cstubs_dirs in
  let src_file = fpath ~dir:boot_dir "b0i.ml" in
  string_to_file src_file (instance_src ());
  Sys.chdir boot_dir; (* .o files of cstubs are generated in the cwd *)
  run_cmd (comp :: "-o" :: exe :: (incs @ lib_ars @ cstubs_args @ [src_file]));
  Sys.chdir root_dir

let run_instance args =
  log "Running instance %s\n" exe;
  run_cmd (exe :: args @ ["--b0-dir"; "_boot_b0"]) (* bof *)

(* Reset _boot *)

let reset () =
  log "Clearing contents of %s\n" boot_dir;
  let rm = function ".keep" -> () | f -> Sys.remove (fpath ~dir:boot_dir f) in
  Array.iter rm (Sys.readdir boot_dir)

(* Main *)

let b00t = function
| `Reset -> reset ()
| `Cold -> compile_instance (); log "Done! Instance: %s\n" exe
| `Build args -> run_instance args
| `Full -> compile_instance (); run_instance []

let parse_cli () = match Array.to_list Sys.argv with
| _ :: [ "reset" ] -> `Reset
| _ :: [ "cold" ] -> `Cold
| [] | [ _ ] | _ :: [ "full" ] -> `Full
| _ :: "build" :: args  -> `Build args
| cmd :: args ->
    err "%s: Unknown argument(s): %s\n" cmd @@ String.concat " " args

let () = b00t (parse_cli ())
