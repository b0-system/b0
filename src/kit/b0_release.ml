(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

module Meta = struct
  let src_archive_name =
    let doc = "Source release archive (base) name" in
    B0_meta.Key.v "release-archive-name" ~doc ~pp_value:Fmt.string

  let src_archive_ext =
    let doc = "Source release archive file extension" in
    B0_meta.Key.v "release-archive-name" ~doc ~pp_value:Fmt.string

  let src_archive_url =
    let doc = "Source release archive URL pattern" in
    B0_meta.Key.v "release-archive-name" ~doc ~pp_value:Fmt.string
end

let version_of_pack ?(commit_ish = "HEAD") p =
  Result.map_error (Fmt.str "@[<v>Cannot determine release version:@,%s@]") @@
  match B0_def.scope_dir (B0_pack.def p) with
  | None -> Error "No scope directory for pack"
  | Some dir ->
      let* vcs = B00_vcs.find ~dir () in
      match vcs with
      | None -> Fmt.error "No VCS found in %a" Fpath.pp_unquoted dir
      | Some vcs ->
          let* tag = B00_vcs.latest_tag vcs commit_ish in
          match tag with
          | Some t -> Ok (String.drop_initial_v t)
          | None ->
              Fmt.error "No annotated tag for %s in %a"
                commit_ish B00_vcs.pp vcs

(* Source archives. *)

let src_archive_name_of_pack p =
  match B0_pack.find_meta Meta.src_archive_name p with
  | Some n -> n
  | None ->
      let n = B0_pack.basename p in
      if not (String.equal n "default") then n else
      match B0_def.scope_dir (B0_pack.def p) with
      | None -> "unknown" (* unlikely, libraries should not do this. *)
      | Some d -> Fpath.basename d

let src_archive_ext_of_pack p =
  match B0_pack.find_meta Meta.src_archive_ext p with
  | None -> ".tbz" | Some ext -> ext

let default_archive =
  "\x25%ARCHIVE_NAME%\x25-\x25%VERSION_NUM%\x25\x25%ARCHIVE_EXT%\x25"

let default_src_archive_url homepage =
  let drop_final_slash s =
    let max = String.length s - 1 in
    if max < 0 then s else
    if s.[max] = '/' then String.subrange ~last:(max - 1) s else s
  in
  Fmt.str "%s/releases/%s" (drop_final_slash homepage) default_archive

let default_github_src_archive_url repo =
  let repo = match String.cut_left ~sep:"git+" repo with
  | Some (_, repo) -> repo | _ -> repo
  in
  let repo = match String.cut_right ~sep:"." repo with
  | Some (repo, _) -> repo | _ -> repo
  in
  Fmt.str "%s/releases/download/\x25\x25VERSION\x25\x25/%s" repo default_archive

let src_archive_url_of_pack ~version p =
  let archive_name = src_archive_name_of_pack p in
  let archive_ext = src_archive_ext_of_pack p in
  let substs =
    String.Map.empty
    |> String.Map.add "ARCHIVE_NAME" archive_name
    |> String.Map.add "ARCHIVE_EXT" archive_ext
    |> String.Map.add "VERSION" version
    |> String.Map.add "VERSION_NUM" (String.drop_initial_v version)
  in
  Result.map_error (Fmt.str "@[<v>Cannot determine release URL:@,%s@]") @@
  let err () = Error "See docs of B0_release.src_archive_url_of_pack." in
  let* archive_url = match B0_pack.find_meta Meta.src_archive_url p with
  | Some url -> Ok url
  | None ->
      match B0_pack.find_meta B0_meta.homepage p with
      | None -> err ()
      | Some h ->
          let is_github = match B00_http.Uri.parse_authority h with
          | None -> false
          | Some auth ->
              match String.split_on_char '.' auth with
              | ("github" :: _ ) | ( _ :: "github" :: _) -> true | _ -> false
          in
          match is_github with
          | false -> Ok (default_src_archive_url h)
          | true ->
              match B0_pack.find_meta B0_meta.repo p with
              | None -> err ()
              | Some repo -> Ok (default_github_src_archive_url repo)
  in
  match String.subst_pct_vars substs archive_url with
  | None -> Ok archive_url | Some url -> Ok url

(* Change logs *)

let changes_file_of_pack p = match B0_def.scope_dir (B0_pack.def p) with
| None -> Ok None
| Some dir ->
    let changes = Fpath.(dir / "CHANGES.md") in
    let* exists = Os.File.exists changes in
    if not exists then Ok None else Ok (Some changes)

let changes_latest_of_file f =
  let* contents = Os.File.read f in
  Ok (B00_cmark.first_section ~preamble:false contents)

module Cmdlet = struct
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers

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
