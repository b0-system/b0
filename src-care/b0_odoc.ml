(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00

let read_path_writes m file k =
  let parse_path n p acc = match Fpath.of_string p with
  | Error e -> B0_lines.err n "%s" e
  | Ok p -> p :: acc
  in
  let parse lines = B0_lines.fold ~file (String.trim lines) parse_path [] in
  Memo.read m file (fun lines -> k (Memo.fail_if_error m (parse lines)))

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
    let parse_dep n line acc = match String.cut_right ~sep:" " line with
    | None -> B0_lines.err n "Could not parse line %S" line
    | Some (name, digest) ->
        let digest = try Digest.from_hex digest with
        | Invalid_argument _ (* sic *) ->
            B0_lines.err n "Could not parse digest %S" digest
        in
        (name, digest) :: acc

    let write m cobj ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:[cobj] ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "compile-deps" %% path cobj)

    let read m file k =
      let parse lines = B0_lines.fold ~file (String.trim lines) parse_dep [] in
      Memo.read m file (fun lines -> k (Memo.fail_if_error m (parse lines)))
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

  let to_odoc m ?hidden ~pkg ~odoc_deps obj ~o:odoc =
    let writes = Fpath.(odoc + ".writes") in
    Writes.write m obj ~to_odoc:odoc ~o:writes;
    Writes.read m writes @@ fun writes ->
    cmd m ?hidden ~odoc_deps ~writes ~pkg obj ~o:odoc
end

module Html = struct
  module Dep = struct
    type t = string * string * Digest.t
    let pkg (p, _, _) = p
    let name (_, n, _) = n
    let digest (_, _, d) = d
    let to_compile_dep (_, n, d) = (n, d)
    let parse_dep n line acc = match String.cut_right ~sep:" " line with
    | None -> B0_lines.err n "Could not parse line %S" line
    | Some (rest, digest) ->
        let digest = try Digest.from_hex digest with
        | Invalid_argument _ (* sic *) ->
            B0_lines.err n "Could not parse digest %S" digest
        in
        match String.cut_right ~sep:" " rest with
        | Some (pkg, name) -> (pkg, name, digest) :: acc
        | None -> B0_lines.err n "Could not parse pkg and mod names %S" rest

    let write m ~odoc_files pkg_odoc_dir ~o =
      let odoc = Memo.tool m tool in
      Memo.spawn m ~reads:odoc_files ~writes:[o] ~stdout:(`File o) @@
      odoc Cmd.(arg "html-deps" %% path pkg_odoc_dir)

    let read m file k =
      let parse lines = B0_lines.fold ~file (String.trim lines) parse_dep [] in
      Memo.read m file (fun lines -> k (Memo.fail_if_error m (parse lines)))
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

  let write m ?theme_uri ~html_dir ~odoc_deps odoc =
    let writes = Fpath.(odoc -+ ".html.writes") in
    let odoc_deps = match odoc_deps with
    | [] ->
        (* Hack to work around https://github.com/ocaml/odoc/issues/290.
           If we have only mld's html-deps returns nothing. This
           will at least include the package directory. *)
        [odoc]
    | deps ->
        (* This will also be wrong in general for mld pages. So we add
           again the odoc's file directory by default. *)
        (odoc :: deps)
    in
    Writes.write m ~odoc_deps odoc ~to_dir:html_dir ~o:writes;
    Writes.read m writes @@ fun writes ->
    cmd m ?theme_uri ~odoc_deps ~writes odoc ~to_dir:html_dir
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
    let theme = Cmd.(if' without_theme (arg "--without-theme")) in
    Memo.spawn m ~reads:[] ~writes @@
    odoc Cmd.(arg "support-files" %% theme % "-o" %% path to_dir)

  let write m ~without_theme ~html_dir ~build_dir =
    let o = Fpath.(build_dir / "odoc-support-files.writes") in
    let to_dir = html_dir in
    Writes.write m ~without_theme ~to_dir ~o;
    Writes.read m o @@ fun writes ->
    cmd m ~writes ~without_theme ~to_dir
end

module Theme = struct

  (* Theme names *)

  type name = string
  let default = "odoc.default"

  (* User preference *)

  let config_file = Fpath.v "odig/odoc-theme"
  let set_user_preference name =
    try
      let config = Os.Dir.config () |> Result.to_failure in
      let config_file = Fpath.(config // config_file) in
      Os.File.write ~force:true ~make_path:true config_file name
    with Failure e -> Error e

  let get_user_preference () =
    try
      let config = Os.Dir.config () |> Result.to_failure in
      let file = Fpath.(config // config_file) in
      match Os.File.exists file |> Result.to_failure with
      | false -> Ok default
      | true -> Ok (String.trim (Os.File.read file |> Result.to_failure))
    with Failure e -> Error e

  (* Theme *)

  type t = name * Fpath.t
  let name (n, _) = n
  let path (_, p) = p
  let pp ppf (n, p) =
    Fmt.pf ppf "@[<h>%s %a@]" n (Fmt.tty [`Faint] Fpath.pp_quoted) p

  let pp_name ppf (n, _) = Fmt.string ppf n
  let of_dir dir =
    Log.time (fun _ m -> m "theme list of %a" Fpath.pp_quoted dir) @@ fun () ->
    try
      let add_themes _ pkg dir acc =
        let tdir = Fpath.(dir / "odoc-theme") in
        match Os.Dir.exists tdir |> Result.to_failure with
        | false -> acc
        | true ->
            let name pkg name = Fmt.str "%s.%s" pkg name in
            let add_theme _ sub dir acc = (name pkg sub, dir) :: acc in
            Result.to_failure @@
            Os.Dir.fold_dirs ~recurse:false add_theme tdir acc
      in
      let ts = Os.Dir.fold_dirs ~recurse:false add_themes dir [] in
      let compare (n0, _) (n1, _) =
        compare (String.Ascii.lowercase n0) (String.Ascii.lowercase n1)
      in
      List.sort compare (Result.to_failure ts)
    with Failure e -> Log.err (fun m -> m "theme list: %s" e); []

  let find n ts = match List.find (fun t -> name t = n) ts with
  | t -> Ok t
  | exception Not_found ->
      let ss = String.suggest (List.rev_map name ts) n in
      let post ppf () = Fmt.pf ppf " using %s instead" default in
      Fmt.error "%a" (Fmt.did_you_mean ~kind:"theme" ~post Fmt.string) (n, ss)
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
