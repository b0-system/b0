(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

type host = string

let get_ssh () =
  (* TODO abstract this pattern, see `B0_opam` *)
  let ssh = OS.Env.opt_var "B0_SSH" ~absent:"ssh" in
  match OS.Cmd.which_raw ssh with
  | None -> R.error_msgf "ssh: not found in PATH (as %s)" ssh
  | Some ssh -> Ok (Cmd.v ssh)

let get_rsync () =
  let ssh = OS.Env.opt_var "B0_RSYNC" ~absent:"rsync" in
  match OS.Cmd.which_raw ssh with
  | None -> R.error_msgf "rsync: not found in PATH (as %s)" ssh
  | Some rsync -> Ok (Cmd.v rsync)

let run excludes remote_root host scheme conf cmd =
  let root =
    let root = Variant.Scheme.proxy_conf_root_dir conf in
    Fpath.to_string @@ Fpath.to_dir_path root
  in
  let remote_root =
    (* FIXME need rem_empty_seg *)
    let remote_root = Fpath.to_string @@ remote_root in
    (* FIXME need Fpath.segs on root *)
    remote_root ^ root
  in
  let remote_root_addr = strf "%s:%s" host remote_root in
  let remote_variant =
    let proxied_scheme = Variant.Scheme.name scheme in
    (* FIXME don't construct things like this *)
    strf "%s:%s_b0/v/%s" host remote_root proxied_scheme
  in
  let variant_proxy =
    let variant_dir = Variant.Scheme.proxy_conf_variant_dir conf in
    (* FIXME proxy dir should not be hardcoded. *)
    Fpath.(variant_dir / "proxy")
  in
  let b0_dir =
    let b0_dir = Variant.Scheme.proxy_conf_b0_dir conf in
    Fpath.to_string @@ Fpath.to_dir_path b0_dir
  in
  let excludes =
    let excludes = Cmd.of_values ~slip:"--exclude" Fpath.to_string excludes in
      (* Exclusion paths in rsync are relative, exclude b0 if its in root dir *)
    match String.is_prefix ~affix:root b0_dir with
    | false -> excludes
    | true ->
        let first = String.length root in
        let last = String.length b0_dir - 2 (* rem slash *) in
        let dir = String.with_index_range ~first ~last b0_dir in
        Cmd.(v "--exclude" % dir %% excludes)
  in
  OS.Dir.create variant_proxy >>= fun _ ->
  get_ssh () >>= fun ssh ->
  let ssh = Cmd.(ssh % host) in
  get_rsync () >>= fun rsync ->
  let rsync = Cmd.(rsync % "-haz" % "--partial" % "--delete" % "-e" % "ssh") in
  Log.app (fun m -> m "[RSYNC] -> source tree to %s" remote_root_addr);
  OS.Cmd.run Cmd.(ssh % "mkdir" % "-p" % remote_root)
  >>= fun () ->
  OS.Cmd.run Cmd.(rsync %% excludes % root % remote_root_addr)
  >>= fun () ->
  OS.Cmd.run_status Cmd.(ssh % "-tq" % "--" % "cd" % remote_root % "&&" %% cmd)
  >>= fun st ->
  Log.app (fun m -> m "[RSYNC] <- build outcome");
  OS.Cmd.run Cmd.(rsync % remote_variant % p variant_proxy)
  >>= fun () ->
  Ok st

let variant_scheme
    ?loc ?doc ?name ?(excludes = []) ?(root = Fpath.v ".b0_builds") host scheme
  =
  let sname = Variant.Scheme.name scheme in
  let name = match name with None -> strf "ssh-%s" sname | Some n -> n in
  let doc = match doc with
  | Some d -> d
  | None -> strf "Build on SSH host '%s' with scheme '%s'" host sname
  in
  let create _ = Ok () in
  let setup _ = Ok () in
  let delete _ = Log.app (fun m -> m "FIXME delete build on host"); Ok () in
  let run = run excludes root host scheme in
  let p = Variant.Scheme.proxy ~create ~setup ~delete ~run scheme in
  Variant.Scheme.v ?loc ~doc name (`Proxy p)

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
