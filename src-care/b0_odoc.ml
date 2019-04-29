(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B00

let parse_lines ~file data parse acc =
  let err n e = Fmt.error "%a:%d: %s" Fpath.pp_unquoted file n e in
  let rec loop acc n data = match String.cut_left ~sep:"\n" data with
  | Some (line, rest) ->
      begin match parse line acc with
      | exception Failure e -> err n e
      | acc -> loop acc (n + 1) rest
      end
  | None when data = "" -> Ok acc
  | None -> try Ok (parse data acc) with Failure e -> err n e
  in
  loop acc 1 data

let read_path_writes m file k =
  let parse_path p acc = Result.to_failure (Fpath.of_string p) :: acc in
  let parse lines = parse_lines ~file lines parse_path [] in
  Memo.read m file (fun lines -> k (Memo.fail_error (parse lines)))

let tool = Tool.by_name "odoc" (* FIXME conf ! *)

let files_to_includes fs =
  (* So sad https://github.com/ocaml/odoc/issues/81 *)
  let add_dir acc f = Fpath.Set.add (Fpath.parent f) acc in
  let dirs = List.fold_left add_dir Fpath.Set.empty fs in
  Fpath.Set.fold (fun dir acc -> Cmd.(acc % "-I" %% path dir)) dirs Cmd.empty

module Compile = struct
  module Dep = struct
    type t = string * Digest.t
    let name = fst
    let digest = snd
    let pp ppf (n, d) = Fmt.pf ppf "@[%s %s@]" n (Digest.to_hex d)
    let parse_dep line acc = match String.cut_right ~sep:" " line with
    | None -> Fmt.failwith "Could not parse line %S" line
    | Some (name, digest) ->
        let digest = try Digest.from_hex digest with
        | Invalid_argument _ (* sic *) ->
            Fmt.failwith "Could not parse digest %S" digest
        in
        (name, digest) :: acc

    let write m cobj ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:[cobj] ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "compile-deps" %% path cobj)

    let read m file k =
      let parse lines = parse_lines ~file lines parse_dep [] in
      Memo.read m file (fun lines -> k (Memo.fail_error (parse lines)))
  end

  module Writes = struct
    let write m cobj ~to_odoc ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:[cobj] ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "compile-targets" % "-o" %% path to_odoc %% path cobj)

    let read = read_path_writes
  end

  let cmd
      ?(resolve_forward_deps = false) ?(hidden = false) m ~odoc_deps ~writes
      ~pkg cobj ~o
    =
    let odoc = Memo.tool m tool in
    let incs = files_to_includes odoc_deps in
    Memo.spawn m ~reads:(cobj :: odoc_deps) ~writes @@
    odoc Cmd.(arg "compile" % "--pkg" % pkg %% if' hidden (arg "--hidden") %%
              if' resolve_forward_deps (arg "--resolve-fwd-refs") %
              "-o" %% path o %% path cobj %% incs)
end

module Html = struct
  module Dep = struct
    type t = string * string * Digest.t
    let pkg (p, _, _) = p
    let name (_, n, _) = n
    let digest (_, _, d) = d
    let to_compile_dep (_, n, d) = (n, d)
    let parse_dep line acc = match String.cut_right ~sep:" " line with
    | None -> Fmt.failwith "Could not parse line %S" line
    | Some (rest, digest) ->
        let digest = try Digest.from_hex digest with
        | Invalid_argument _ (* sic *) ->
            Fmt.failwith "Could not parse digest %S" digest
        in
        match String.cut_right ~sep:" " rest with
        | Some (pkg, name) -> (pkg, name, digest) :: acc
        | None -> Fmt.failwith "Could not parse pkg and mod names %S" rest

    let write m ~odoc_files pkg_odoc_dir ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:odoc_files ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "html-deps" %% path pkg_odoc_dir)

    let read m file k =
      let parse lines = parse_lines ~file lines parse_dep [] in
      Memo.read m file (fun lines -> k (Memo.fail_error (parse lines)))
  end

  module Writes = struct
    let write m ~odoc_deps odoc_file ~to_dir ~o =
      let odoc = Memo.tool m tool in
      let incs = files_to_includes odoc_deps in
      let reads = odoc_file :: odoc_deps in
      Memo.spawn m ~reads ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "html-targets" %% incs % "-o" %% path to_dir %%
                path odoc_file)

    let read = read_path_writes
  end

  let cmd ?(hidden = false) ?theme_uri m ~odoc_deps ~writes odoc_file ~to_dir =
    let odoc = Memo.tool m tool in
    let incs = files_to_includes odoc_deps in
    let theme_uri = match theme_uri with
    | None -> Cmd.empty
    | Some u -> Cmd.(arg "--theme-uri" % u)
    in
    Memo.spawn m ~reads:(odoc_file :: odoc_deps) ~writes @@
    odoc Cmd.(arg "html" %% if' hidden (arg "--hidden") %% theme_uri % "-o" %%
              path to_dir %% path odoc_file %% incs)
end

module Html_fragment = struct
  let cmd m ~odoc_deps mld_file ~o =
    let odoc = Memo.tool m tool in
    let incs = files_to_includes odoc_deps in
    Memo.spawn m ~reads:(mld_file :: odoc_deps) ~writes:[o] @@
    odoc Cmd.(arg "html-fragment" % "-o" %% path o %% path mld_file %% incs)
end

module Support_files = struct
  module Writes = struct
    let write ?(without_theme = false) m ~to_dir ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:[] ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "support-files-targets" %%
                if' without_theme (arg "--without-theme") % "-o" %% path to_dir)

    let read = read_path_writes
  end

  let cmd ?(without_theme = false) m ~writes ~to_dir =
    let odoc = Memo.tool m tool in
    Memo.spawn m ~reads:[] ~writes @@
    odoc Cmd.(arg "support-files" %%
              if' without_theme (arg "--without-theme") % "-o" %% path to_dir)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
