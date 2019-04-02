(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B00
open B0_web

module Trace_event = struct
  let str pp get o = Jsong.strf "%a" pp (get o)
  let span_us s =
    let span_us = Int64.(to_int @@ div (Time.Span.to_uint64_ns s) 1000L) in
    Jsong.int span_us

  let args o =
    let kind_mems obj = match Op.kind o with
    | Op.Spawn s ->
        let cmd = Cmd.(path (Op.Spawn.tool s) %% (Op.Spawn.args s)) in
        obj
        |> Jsong.mem "cmd" (Jsong.cmd cmd)
        |> Jsong.mem "result" (str Op.Spawn.pp_result Op.Spawn.result s)
        |> Jsong.mem "cwd" (Jsong.fpath (Op.Spawn.cwd s))
        |> Jsong.mem "env" (Jsong.(list string) (Op.Spawn.env s))
        |> Jsong.mem "success-exits"
          (str Op.Spawn.pp_success_exits Op.Spawn.success_exits s)
        |> Jsong.mem "stdo-ui"
          (Jsong.strf "%a" (Op.Spawn.pp_stdo_ui ~elide:false) s)
    | Op.Read r ->
        obj
        |> Jsong.mem "file" (Jsong.fpath (Op.Read.file r))
        |> Jsong.mem "result" (str Op.Read.pp_result Op.Read.result r)
    | Op.Write w ->
        obj
        |> Jsong.mem "file" (Jsong.fpath (Op.Write.file w))
        |> Jsong.mem "result" (str Op.Write.pp_result Op.Write.result w)
    | Op.Mkdir m ->
        obj
        |> Jsong.mem "dir" (Jsong.fpath (Op.Mkdir.dir m))
        |> Jsong.mem "result" (str Op.Mkdir.pp_result Op.Mkdir.result m)
    | Op.Wait_files -> obj
    in
    (* The order here is for the viewer. *)
    Jsong.obj
    |> Jsong.mem "kind" (Jsong.string (Op.kind_name (Op.kind o)))
    |> Jsong.mem "group" (Jsong.string (Op.group o))
    |> Jsong.mem "status" (str Op.pp_status Op.status o)
    |> Jsong.mem "revived" (Jsong.bool (Op.exec_revived o))
    |> Jsong.mem "writes" (Jsong.(list fpath) (Op.writes o))
    |> Jsong.mem "created" (span_us (Op.creation_time o))
    |> kind_mems
    |> Jsong.mem "reads" (Jsong.(list fpath) (Op.reads o))
    |> Jsong.mem "hash" (Jsong.string (Hash.to_hex (Op.hash o)))
    |> Jsong.obj_end

  let op o =
    let id o = Jsong.string (string_of_int @@ Op.id o) in
    let cat o = Jsong.string @@ Op.kind_name (Op.kind o) in
    Jsong.obj
    |> Jsong.mem "name" (id o)
    |> Jsong.mem "cat" (cat o)
    |> Jsong.mem "ph" (Jsong.string "X")
    |> Jsong.mem "ts" (span_us (Op.exec_start_time o))
    |> Jsong.mem "dur" (span_us (Op.exec_duration o))
    |> Jsong.mem "pid" (Jsong.int 1)
    |> Jsong.mem "tid" (Jsong.int 1)
    |> Jsong.mem "args" (args o)
    |> Jsong.obj_end

  let of_ops os = Jsong.list op os
end

module Compilation_database = struct
  let spawn_out o spawn src arr out_file =
    let cmd = Cmd.(path (Op.Spawn.tool spawn) %% (Op.Spawn.args spawn)) in
    arr |> Jsong.el begin
      Jsong.obj
      |> Jsong.mem "directory" (Jsong.fpath (Op.Spawn.cwd spawn))
      |> Jsong.mem "file" (Jsong.fpath src)
      |> Jsong.mem "arguments" (Jsong.cmd cmd)
      |> Jsong.mem "output" (Jsong.fpath out_file)
      |> Jsong.mem "id" (Jsong.int (Op.id o))
      |> Jsong.obj_end
    end

  let add_op arr o = match Op.kind o with
  | Op.Spawn s ->
      let src = match Op.reads o with [] -> Os.File.null | fs -> List.hd fs in
      List.fold_left (spawn_out o s src) arr (Op.writes o)
  | _ -> arr

  let of_ops os = Jsong.arr_end (List.fold_left add_op Jsong.arr os)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
