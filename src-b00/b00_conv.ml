(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B00

(* Binary encoding *)

module Bin = struct
  let magic = "b\x00\x00\x00"
  let err i fmt = Fmt.failwith ("%d: " ^^ fmt) i
  let err_trunc i n = err i "truncated input expected %d more bytes" n
  let write_magic b = Buffer.add_string b magic
  let read_magic s i =
    let last = i + (String.length magic) - 1 in
    let magic' = String.with_index_range ~first:i ~last s in
    if String.equal magic magic' then i + (String.length magic) else
    err i "magic mismatch: %S but expected %S" magic' magic

  let write_byte b n =
    Buffer.add_char b (Char.chr (n land 0xFF)) [@@ocaml.inline]

  let read_byte s i = Char.code (String.get s i) [@@ocaml.inline]

  let write_int b n =
    let w = write_byte in
    w b n; w b (n lsr 8); w b (n lsr 16); w b (n lsr 24);
    if Sys.word_size = 32 then (w b 0x00; w b 0x00; w b 0x00; w b 0x00) else
    (w b (n lsr 32); w b (n lsr 40); w b (n lsr 48); w b (n lsr 56))

  let read_int s max i =
    let r = read_byte in
    let next = i + 8 in
    if next > max + 1 then err_trunc i 8 else
    let b0 = r s (i    ) and b1 = r s (i + 1)
    and b2 = r s (i + 2) and b3 = r s (i + 3) in
    let n = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
    if Sys.word_size = 32 then next, n else
    let b4 = r s (i + 4) and b5 = r s (i + 5)
    and b6 = r s (i + 6) and b7 = r s (i + 7) in
    next, (b7 lsl 56) lor (b6 lsl 48) lor (b5 lsl 40) lor (b4 lsl 32) lor n

  let write_string b s =
    write_int b (String.length s);
    Buffer.add_string b s
end

module File_cache = struct
  let pp_feedback ppf = function
  | `File_cache_need_copy p ->
      Fmt.pf ppf "@[Warning: need copy: %a@]" Fpath.pp p
end

module Guard = struct
  let pp_feedback ppf = function
  | `File_status_repeat f ->
      Fmt.pf ppf "%a: file status repeated" Fpath.pp_unquoted f
  | `File_status_unstable f ->
      Fmt.pf ppf "%a: file status unstable" Fpath.pp_unquoted f
end

module Op = struct
  let file_write_color = [`Faint; `Fg `Green]
  let pp_file_contents = Fmt.truncated ~max:150
  let pp_error_msg ppf e = Fmt.pf ppf "@[error: %s@]" e

  let kind_name_padded = function
  | B00.Op.Spawn _ -> "spawn" | Read _ -> "read " | Write _ -> "write"
  | Copy _ -> "copy " | Mkdir _ -> "mkdir" | Wait_files -> "wait "

  let pp_status ppf v = Fmt.string ppf @@ match v with
  | B00.Op.Waiting -> "waiting" | Executed -> "executed"
  | Failed -> "failed" | Aborted -> "aborted"

  module Spawn = struct

    (* Formatting *)

    let pp_success_exits ppf = function
    | [] -> Fmt.string ppf "any"
    | cs -> Fmt.(list ~sep:comma int) ppf cs

    let pp_cmd ppf s =
      (* XXX part of this could maybe moved to B0_std.Cmd. *)
      let tool = Fpath.to_string (Op.Spawn.tool s) in
      let args = Cmd.to_list (Op.Spawn.args s) in
      let quote = Filename.quote in
      let pp_brack = Fmt.tty_string [`Fg `Yellow] in
      let pp_tool ppf e = Fmt.tty_string [`Fg `Blue; `Bold] ppf (quote e) in
      let pp_arg ppf a = Fmt.pf ppf "%s" (quote a) in
      let pp_o_arg ppf a = Fmt.tty_string file_write_color ppf (quote a) in
      let rec pp_args last_was_o ppf = function
      | [] -> ()
      | a :: args ->
          Fmt.char ppf ' ';
          if last_was_o then pp_o_arg ppf a else pp_arg ppf a;
          pp_args (String.equal a "-o") ppf args
      in
      let pp_stdin ppf = function
      | None -> ()
      | Some file -> Fmt.pf ppf "< %s" (quote (Fpath.to_string file))
      in
      let pp_stdo redir ppf = function
      | `Ui -> ()
      | `Tee f | `File f ->
          Fmt.pf ppf " %s %a" redir pp_o_arg (Fpath.to_string f)
      in
      Fmt.pf ppf "@[<h>"; pp_brack ppf "[";
      pp_tool ppf tool; pp_args false ppf args;
      pp_stdin ppf (Op.Spawn.stdin s);
      pp_stdo ">" ppf (Op.Spawn.stdout s);
      pp_stdo "2>" ppf (Op.Spawn.stderr s);
      pp_brack ppf "]"; Fmt.pf ppf "@]"

    let pp_stdo ppf = function
    | `Ui -> Fmt.pf ppf "<ui>"
    | `File f -> Fpath.pp ppf f
    | `Tee f -> Fmt.pf ppf "@[<hov><ui> and@ %a@]" Fpath.pp f

    let pp_stdo_ui ~truncate ppf s = match (Op.Spawn.stdo_ui s) with
    | None -> Fmt.none ppf ()
    | Some (Ok d) ->
        if truncate then pp_file_contents ppf d else String.pp ppf d
    | Some (Error e) -> pp_error_msg ppf e

    let pp_result ppf = function
    | Ok st -> Os.Cmd.pp_status ppf st
    | Error e -> pp_error_msg ppf e

    let pp =
      let pp_env = Fmt.(vbox @@ list string) in
      let pp_opt_path = Fmt.(option ~none:none) Fpath.pp in
      Fmt.record @@
      [ Fmt.field "cmd" Fmt.id pp_cmd;
        Fmt.field "env" Op.Spawn.env pp_env;
        Fmt.field "relevant_env" Op.Spawn.relevant_env pp_env;
        Fmt.field "cwd" Op.Spawn.cwd Fpath.pp;
        Fmt.field "success-exits" Op.Spawn.success_exits pp_success_exits;
        Fmt.field "stdin" Op.Spawn.stdin pp_opt_path;
        Fmt.field "stdout" Op.Spawn.stdout pp_stdo;
        Fmt.field "stderr" Op.Spawn.stderr pp_stdo;
        Fmt.field "stdo-ui" Fmt.id (pp_stdo_ui ~truncate:false);
        Fmt.field "result" Op.Spawn.result pp_result ]
  end

  module Read = struct
    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok d -> pp_file_contents ppf d

    let pp =
      Fmt.concat @@
      [ Fmt.field "file" Op.Read.file Fpath.pp;
        Fmt.field "result" Op.Read.result pp_result ]
  end

  module Write = struct
    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok () -> Fmt.string ppf "written"

    let pp =
      Fmt.concat @@
      [ Fmt.field "file" Op.Write.file Fpath.pp;
        Fmt.field "mode" Op.Write.mode Fmt.int;
        Fmt.field "result" Op.Write.result pp_result; ]
  end

  module Copy = struct
    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok () -> Fmt.string ppf "copied"

    let pp = Fmt.field "result" Op.Copy.result pp_result
  end

  module Mkdir = struct
    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok created -> Fmt.string ppf (if created then "created" else "existed")

    let pp =
      Fmt.concat @@
      [ Fmt.field "dir" Op.Mkdir.dir Fpath.pp;
        Fmt.field "result" Op.Mkdir.result pp_result ]
  end

  module Wait_files = struct
  end

  (* Formatting *)

  let pp_kind ppf = function
  | Op.Spawn s -> Spawn.pp ppf s
  | Op.Read r -> Read.pp ppf r
  | Op.Write w -> Write.pp ppf w
  | Op.Copy c -> Copy.pp ppf c
  | Op.Mkdir m -> Mkdir.pp ppf m
  | Op.Wait_files -> ()

  let pp_kind_short o ppf = function
  | Op.Spawn s -> Spawn.pp_cmd ppf s
  | Op.Read r -> Fpath.pp ppf (Op.Read.file r)
  | Op.Write w -> (Fmt.tty file_write_color Fpath.pp) ppf (Op.Write.file w)
  | Op.Mkdir m -> Fpath.pp ppf (Op.Mkdir.dir m)
  | Op.Copy c ->
      Fmt.pf ppf "%a to %a"
        Fpath.pp (Op.Copy.src c)
        (Fmt.tty file_write_color Fpath.pp) (Op.Copy.dst c)
  | Op.Wait_files ->
      Fmt.pf ppf "@[<v>%a@]" (Fmt.list Fpath.pp) (Op.reads o)

  let pp_synopsis ppf o =
    let pp_kind ppf k = Fmt.tty_string [`Fg `Green] ppf k in
    let pp_status ppf = function
    | Op.Executed -> ()
    | Op.Failed -> Fmt.pf ppf "[%a]" (Fmt.tty_string [`Fg `Red]) "FAILED"
    | Op.Aborted -> Fmt.pf ppf "[%a]" (Fmt.tty_string [`Fg `Red]) "ABORTED"
    | Op.Waiting -> Fmt.pf ppf "[waiting]"
    in
    let pp_revived ppf = function
    | true -> Fmt.tty_string [`Fg `Magenta; `Faint] ppf "R"
    | false -> Fmt.tty_string [`Fg `Magenta] ppf "E"
    in
    Fmt.pf ppf "@[%a[%a %a:%03d]@]"
      pp_status (Op.status o) pp_kind (kind_name_padded (Op.kind o))
      pp_revived (Op.exec_revived o) (Op.id o)

  let pp_short ppf o =
    Fmt.pf ppf "@[<h>%a %a@]" pp_synopsis o (pp_kind_short o) (Op.kind o)

  let pp_writes =
    let pp_file_write = Fmt.tty file_write_color Fpath.pp in
    Fmt.braces @@ Fmt.list pp_file_write

  let pp_op =
    let pp_span ppf s =
      Fmt.pf ppf "%a (%ans)" Time.Span.pp s Time.Span.pp_ns s
    in
    let pp_reads = Fmt.braces @@ Fmt.list Fpath.pp in
    let dur o = Time.Span.abs_diff (Op.exec_end_time o) (Op.exec_start_time o)in
    Fmt.concat @@
    [ Fmt.field "group" Op.group Fmt.string;
      Fmt.field "writes" Op.writes pp_writes;
      Fmt.field "reads" Op.reads pp_reads;
      Fmt.field "created" Op.creation_time pp_span;
      Fmt.field "start" Op.exec_start_time pp_span;
      Fmt.field "duration" dur pp_span;
      Fmt.field "hash" Op.hash Hash.pp;
      Fmt.field "kind" (fun o -> Op.kind_name (Op.kind o)) Fmt.string; ]

  let pp ppf o =
    Fmt.pf ppf "@[<v>%a@, @[<v>%a@]@, @[<v>%a@]@]"
      pp_synopsis o pp_kind (Op.kind o) pp_op o

  let err_style = [`Fg `Red]
  let pp_did_not_write ppf (o, fs) =
    let label = Fmt.tty_string err_style in
    Fmt.pf ppf "@[<v>%a@, %a@, @[<v>%a@]@, @[<v>%a@]@]"
      pp_synopsis o (Fmt.field ~label "Did not write" Fmt.id pp_writes) fs
      pp_kind (Op.kind o) pp_op o

  let pp_spawn_status_fail ppf o =
    let s = Op.Spawn.get o in
    Fmt.pf ppf "@[<v>%a@, Illegal exit status: %a expected: %a@, @[<v>%a@]\
                @, @[<v>%a@]@]"
      pp_synopsis o
      (Fmt.tty err_style Spawn.pp_result)
      (Op.Spawn.result s)
      Spawn.pp_success_exits
      (Op.Spawn.success_exits s)
      pp_kind (Op.kind o) pp_op o

  (* Binary serialization *)


  let write_op b o =
    Bin.write_int b (Op.id o);
    ()

  let read_op s max i =
    let _i, _o = Bin.read_int s max i in
    failwith "TODO"

  let to_string ops =
    let b = Buffer.create (1024 * 1024) in
    Bin.write_magic b; List.iter (write_op b) ops;
    Buffer.contents b

  let of_string ?(file = Os.File.dash) s =
    try
      let max = String.length s in
      let i = Bin.read_magic s 0 in
      let rec loop s max i acc = match i > max with
      | true -> Ok (List.rev acc)
      | false ->
          let i, o = read_op s max i in
          loop s max i (o :: acc)
      in
      loop s max i []
    with
    | Failure e -> Fmt.error "%a: %s" Fpath.pp_unquoted file e
end

module Exec = struct
  let pp_feedback ppf = function
  | `Exec_submit (pid, op) ->
      let pp_pid ppf = function
      | None -> () | Some pid -> Fmt.pf ppf "[pid:%d]" (Os.Cmd.pid_to_int pid)
      in
      Fmt.pf ppf "@[[SUBMIT][%s:%d]%a %a@]"
        (B00.Op.kind_name (B00.Op.kind op)) (B00.Op.id op) pp_pid pid
        (Op.pp_kind_short op) (B00.Op.kind op)
end

module Memo = struct
  let pp_feedback ppf = function
  | `Fiber_exn (exn, bt) ->
      Fmt.pf ppf "@[<v>fiber exception:@,%a@]" Fmt.exn_backtrace (exn, bt)
  | `Fiber_fail e ->
      Fmt.pf ppf "@[<v>fiber failed:@,%s@]" e
  | `Miss_tool (t, e) ->
      Fmt.pf ppf "@[<v>missing tool:@,%s@]" e
  | `Op_cache_error (op, e) ->
      Fmt.pf ppf "@[op %d: cache error: %s@]" (B00.Op.id op) e
  | `Op_complete (op, (`Did_not_write fs)) ->
      match (B00.Op.status op) with
      | B00.Op.Failed ->
          begin match fs with
          | [] ->
              begin match B00.Op.kind op with
              | B00.Op.Spawn _ ->
                  Fmt.pf ppf "@[%a@]" Op.pp_spawn_status_fail op;
              | _ -> Fmt.pf ppf "@[%a@]" Op.pp op;
              end
          | fs -> Fmt.pf ppf "@[%a@]" Op.pp_did_not_write (op, fs);
          end
      | _ ->
          match B00.Op.kind op with
          | B00.Op.Spawn s when B00.Op.Spawn.stdo_ui s <> None ->
              Fmt.pf ppf "@[<v>@[<h>[DONE]%a:@]@, %a@]"
                Op.pp_short op (Op.Spawn.pp_stdo_ui ~truncate:false) s
          | _ -> Fmt.pf ppf "@[<h>[DONE]%a@]" Op.pp_short op


  let stdo_feedback ppf =
    let pp_feedback ppf = function
    | #B00.Memo.feedback as f -> pp_feedback ppf f
    | #B00.File_cache.feedback as f -> File_cache.pp_feedback ppf f
    | #B00.Exec.feedback as f -> Exec.pp_feedback ppf f
    in
    Fmt.pr "@[%a@]@." pp_feedback
end



(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
