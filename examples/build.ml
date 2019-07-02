(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B00

let ocamlc = Tool.by_name "ocamlc"
let echo = Tool.by_name "echo"
let cat = Tool.by_name "cat"
let hello m =
  let ocamlc = Memo.tool m ocamlc in
  let bdir = Fpath.(v "examples") in
  let exe = Fpath.(bdir / "hello") in
  let src = Fpath.(exe + ".ml") in
  let cmi = Fpath.(exe + ".cmi") in
  let cmo = Fpath.(exe + ".cmo") in
  let did_mkdir, set_did_mkdir = Memo.Fut.create m in
  Memo.Fut.wait did_mkdir
    (fun d -> Log.app (fun m -> m "did mkdir %a!" Fpath.pp_quoted d));
  Memo.file_ready m src;
  Memo.spawn m ~reads:[cmo] ~writes:[exe] @@
  ocamlc Cmd.(arg "-verbose" %% arg "-o" %% path exe %% path cmo);
  Memo.spawn m ~reads:[src] ~writes:[cmi; cmo] @@
  ocamlc Cmd.(arg "-c" % "-o" %% path exe %% path src);
  let dir = Fpath.(bdir / "bla") in
  Memo.mkdir m dir begin fun _ ->
    Memo.Fut.set set_did_mkdir dir;
    Memo.read m src begin fun src ->
      let gen = Fpath.(exe + ".gen") in
      let echo = Memo.tool m echo in
      Memo.spawn m ~writes:[gen] ~stdout:(`File gen) @@
      echo Cmd.(arg src)
    end;
  end;
  Memo.read m exe begin fun exe_data ->
    let mode = 0o755 in
    Memo.write m ~reads:[exe] ~mode Fpath.(exe + ".bis") begin fun () ->
      Ok exe_data
    end
  end;
  let cat = Memo.tool m cat in
  Memo.spawn m ~stdin:(Os.File.null) ~stdout:(`File Fpath.(bdir / "ha")) @@
  cat Cmd.empty;
  ()

let () =
  Result.to_failure @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let cache_dir = Fpath.(cwd / "examples" / "_cache") in
  let trash_dir = Fpath.(cwd / "examples" / "_trash") in
  Result.bind (Memo.memo ~cwd ~cache_dir ~trash_dir ()) @@ fun m ->
  hello m;
  Log.if_error_pp
    (fun ppf fs -> Fmt.pf ppf "[<v>Never became ready:@,%a@]" Fpath.Set.pp_dump fs)
    ~use:()
    (Memo.finish m);
  Ok ()

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
