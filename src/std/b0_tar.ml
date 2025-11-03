(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

type ptime = int
type member = [ `Dir | `File of string ]
type t = string list

let empty = []

(* Header. http://pubs.opengroup.org/onlinepubs/9699919799/utilities/\
           pax.html#tag_20_92_13_06  *)

let to_unix_path_string =
  if Fpath.natural_dir_sep = "/" then Fpath.to_string else
  fun path -> String.concat "/" (Fpath.to_segments path)

let set_filename h path =
  let s = to_unix_path_string path in
  match String.length s with
  | n when n <= 100 -> Bytes.blit_string s 0 h 0 n
  | n ->
      try match String.split_last ~sep:"/" s with
      | None -> raise Exit
      | Some (prefix, name) ->
          (* This could be made more clever by trying to find
             the slash nearest to the half string position. *)
          if String.length prefix > 155 || String.length name > 100
          then raise Exit;
          Bytes.blit_string name 0 h 0 (String.length name);
          Bytes.blit_string prefix 0 h 345 (String.length prefix);
      with
      | Exit -> Fmt.failwith "%a: file name too long" Fpath.pp path

let set_string off h s = Bytes.blit_string s 0 h off (String.length s)
let set_octal field off len (* terminating NULL included *) h n =
  let octal = Printf.sprintf "%0*o" (len - 1) n in
  if String.length octal < len
  then Bytes.blit_string octal 0 h off (String.length octal) else
  Fmt.failwith "field %s: can't encode %d in %d-digit octal number"
    field (len - 1) n

let header_checksum h =
  let len = Bytes.length h in
  let rec loop acc i =
    if i > len then acc else
    let acc = acc + (Char.code (Bytes.unsafe_get h i)) in
    loop acc (i + 1)
  in
  loop 0 0

let header path mode mtime size typeflag =
  try
    let header = Bytes.make 512 '\x00' in
    set_filename header path;
    set_octal "mode"  100 8 header mode;
    set_octal "owner" 108 8 header 0;
    set_octal "group" 116 8 header 0;
    set_octal "size"  124 12 header size;
    set_octal "mtime" 136 12 header mtime;
    set_string        148 header "        "; (* Checksum *)
    set_string        156 header typeflag;
    set_string        257 header "ustar";
    set_string        263 header "00";
    set_octal "devmajor" 329 8 header 0;
    set_octal "devminor" 337 8 header 0;
    let c = header_checksum header in
    set_octal "checksum" 148 9 (* not NULL terminated *) header c;
    Ok (Bytes.unsafe_to_string header)
  with Failure e -> Error e

(* Members *)

let padding content = match String.length content mod 512  with
| 0 -> ""
| n -> Bytes.unsafe_to_string (Bytes.make (512 - n) '\x00')

let add t path ~mode ~mtime member =
  let typeflag, size, data = match member with
  | `Dir -> "5", 0, []
  | `File content -> "0", String.length content, [content; padding content]
  in
  let* header = header path mode mtime size typeflag in
  Ok (List.rev_append data (header :: t))

(* Encode *)

let to_string t =
  let end_of_file = Bytes.unsafe_to_string (Bytes.make 1024 '\x00') in
  String.concat "" (List.rev (end_of_file :: t))

(* Convenience *)

let of_dir ~dir ~exclude_paths ~root ~mtime =
  let path_set_of_dir dir ~exclude_paths =
    let excluded p = Fpath.Set.mem p exclude_paths in
    let prune_dir _ _ p _ = excluded p in
    let add _ _ p acc = if excluded p then acc else Fpath.Set.add p acc in
    let rel = true and dotfiles = true and follow_symlinks = true in
    let recurse = true and init = Fpath.Set.empty in
    Os.Dir.fold ~rel ~dotfiles ~follow_symlinks ~prune_dir ~recurse add dir init
  in
  Result.map_error (fun e -> Fmt.str "Tar archive creation failed: %s" e) @@
  let tar_add path tar =
    Result.error_to_failure @@
    let path_in_root = Fpath.(root // path) in
    let path_in_dir = Fpath.(dir // path) in
    let* stat = Os.Path.stat path_in_dir in
    match stat.Unix.st_kind with
    | S_DIR -> add tar path_in_root ~mode:0o775 ~mtime `Dir
    | S_REG ->
        let mode = stat.Unix.st_perm in
        let mode = if 0o100 land mode > 0 then 0o775 else 0o664 in
        let* content = Os.File.read path_in_dir in
        add tar path_in_root ~mode ~mtime (`File content)
    | _ -> Fmt.failwith "%a: not a file or directory" Fpath.pp path
  in
  let* paths = path_set_of_dir dir ~exclude_paths in
  try
    let tar = Fpath.Set.fold tar_add paths empty in
    Ok (to_string tar)
  with
  | Failure e -> Error e

(* Compressing and unarchiving *)

let compress_tool_for_file_ext ?(de = "") file =
  match Fpath.take_ext ~multi:false file with
  | ".tar" -> Ok None
  | ".tgz" | ".gz" -> Ok (Some (Cmd.tool "gzip"))
  | ".tbz" | ".bzip2" -> Ok (Some (Cmd.tool "bzip2"))
  | ".xz" -> Ok (Some (Cmd.tool "lzma"))
  | ".zst" -> Ok (Some (Cmd.tool "zstd"))
  | ext ->
      Fpath.error file "Unknown extension %a, cannot %scompress" Fmt.code ext de

let compress ?search ~force ~make_path file ~archive =
  let* compress = compress_tool_for_file_ext file in
  match compress with
  | None -> Os.File.write ~force ~make_path file archive
  | Some compress ->
     let* compress = Os.Cmd.get ?search compress in
     let stdin = Os.Cmd.in_string archive in
     let stdout = Os.Cmd.out_file ~force ~make_path file in
     Os.Cmd.run ~stdin ~stdout compress

let unarchive ?search ~make_path ~verbose ~src ~in_dir () =
  let* tar = Os.Cmd.get ?search (Cmd.tool "tar") in
  let untar file ~in_dir =
    let tar = Cmd.(tar %% if' verbose (arg "-v") % "-xf" %% path file) in
    let* _ = Os.Dir.create ~make_path in_dir in
    Os.Cmd.run ~cwd:in_dir tar
  in
  Result.join @@
  let* compress = compress_tool_for_file_ext ~de:"de" src in
  match compress with
  | None -> Ok (untar src ~in_dir)
  | Some compress ->
      Os.File.with_tmp_fd @@ fun tmpfile fd ->
      let stdin = Os.Cmd.in_file src in
      let stdout = Os.Cmd.out_fd ~close:false fd in
      let* () = Os.Cmd.run ~stdin ~stdout Cmd.(compress % "-d") in
      untar tmpfile ~in_dir
