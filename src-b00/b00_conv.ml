(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B00

(* Binary encoding *)

module Bin = struct
  let err i fmt = Fmt.failwith_notrace ("%d: " ^^ fmt) i
  let err_byte ~kind i b =
    err i "corrupted input, unexpected byte 0x%x for %s" b kind

  let check_next ~kind s i next =
   if next <= String.length s then () else
   err i  "unexpected end of input, expected %d bytes for %s" (next - i) kind

  let get_byte s i = Char.code (String.get s i) [@@ocaml.inline]

  let dec_eoi s i =
    if i = String.length s then () else
    err i "expected end of input (len: %d)" (String.length s)

  let enc_magic b magic = Buffer.add_string b magic
  let dec_magic s i magic =
    let next = i + String.length magic in
    check_next ~kind:magic s i next;
    let magic' = String.with_index_range ~first:i ~last:(next - 1) s in
    if String.equal magic magic' then next else
    err i "magic mismatch: %S but expected %S" magic' magic

  let enc_byte b n =
    Buffer.add_char b (Char.chr (n land 0xFF)) [@@ocaml.inline]

  let dec_byte ~kind s i =
    let next = i + 1 in
    check_next ~kind s i next;
    let b = get_byte s i in
    next, b
  [@@ocaml.inline]

  let enc_unit b () = ()
  let dec_unit s i = i, ()

  let enc_bool b bool = enc_byte b (if bool then 1 else 0)
  let dec_bool s i =
    let kind = "bool" in
    let next, b = dec_byte ~kind s i in
    match b  with
    | 0 -> next, false
    | 1 -> next, true
    | b -> err_byte ~kind i b

  let enc_int b n =
    let w = enc_byte in
    w b n; w b (n lsr 8); w b (n lsr 16); w b (n lsr 24);
    if Sys.word_size = 32 then (w b 0x00; w b 0x00; w b 0x00; w b 0x00) else
    (w b (n lsr 32); w b (n lsr 40); w b (n lsr 48); w b (n lsr 56))

  let dec_int s i =
    let r = get_byte in
    let next = i + 8 in
    check_next ~kind:"int" s i next;
    let b0 = r s (i    ) and b1 = r s (i + 1)
    and b2 = r s (i + 2) and b3 = r s (i + 3) in
    let n = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
    if Sys.word_size = 32 then next, n else
    let b4 = r s (i + 4) and b5 = r s (i + 5)
    and b6 = r s (i + 6) and b7 = r s (i + 7) in
    next, (b7 lsl 56) lor (b6 lsl 48) lor (b5 lsl 40) lor (b4 lsl 32) lor n

  let enc_int64 b i =
    (* XXX From 4.08 on use Buffer.add_int64_le *)
    let w = enc_byte in
    let i0 = Int64.to_int i in
    let i1 = Int64.to_int (Int64.shift_right_logical i 16) in
    let i2 = Int64.to_int (Int64.shift_right_logical i 32) in
    let i3 = Int64.to_int (Int64.shift_right_logical i 48) in
    let b0 = i0 and b1 = i0 lsr 8 and b2 = i1 and b3 = i1 lsr 8
    and b4 = i2 and b5 = i2 lsr 8 and b6 = i3 and b7 = i3 lsr 8 in
    w b b0; w b b1; w b b2; w b b3; w b b4; w b b5; w b b6; w b b7

  external swap64 : int64 -> int64 = "%bswap_int64"
  external unsafe_get_int64_ne : string -> int -> int64 = "%caml_string_get64u"

  let unsafe_get_int64_le b i = match Sys.big_endian with
  | true -> swap64 (unsafe_get_int64_ne b i)
  | false -> unsafe_get_int64_ne b i

  let dec_int64 s i =
    let next = i + 8 in
    check_next ~kind:"int64" s i next;
    next, unsafe_get_int64_le s i

  let enc_string b s =
    enc_int b (String.length s);
    Buffer.add_string b s

  let dec_string s i =
    let i, len = dec_int s i in
    let next = i + len in
    check_next ~kind:"string" s i next;
    next, String.sub s i len

  let enc_fpath b p = enc_string b (Fpath.to_string p)
  let dec_fpath s i =
    let next, s = dec_string s i in
    match Fpath.of_string s with
    | Error e -> err i "corrupted file path value: %s" e
    | Ok p -> next, p

  let enc_list el b l =
    let rec loop len acc = function
    | [] -> len, acc | v :: vs -> loop (len + 1) (v :: acc) vs
    in
    let len, rl = loop 0 [] l in
    enc_int b len;
    let rec loop el b = function [] -> () | v :: vs -> el b v; loop el b vs in
    loop el b rl

  let dec_list el s i  =
    let i, count = dec_int s i in
    let rec loop el s i count acc = match count = 0 with
    | true -> i, acc (* enc_list writes the reverse list. *)
    | false ->
        let i, v = el s i in
        loop el s i (count - 1) (v :: acc)
    in
    loop el s i count []

  let enc_option w b = function
  | None -> enc_byte b 0
  | Some v -> enc_byte b 1; w b v

  let dec_option some s i =
    let kind = "option" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> next, None
    | 1 -> let i, v = some s next in i, Some v
    | b -> err_byte ~kind i b

  let enc_result ~ok ~error b = function
  | Ok v -> enc_byte b 0; ok b v
  | Error e -> enc_byte b 1; error b e

  let dec_result ~ok ~error s i =
    let kind = "result" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> let i, v = ok s next in i, Ok v
    | 1 -> let i, e = error s next in i, Error e
    | b -> err_byte ~kind i b
end

module File_cache = struct
  let pp_feedback ppf = function
  | `File_cache_need_copy p ->
      Fmt.pf ppf "@[Warning: need copy: %a@]" Fpath.pp_quoted p
end

module Guard = struct
  let pp_feedback ppf = function
  | `File_status_repeat f ->
      Fmt.pf ppf "%a: file status repeated" Fpath.pp_quoted f
  | `File_status_unstable f ->
      Fmt.pf ppf "%a: file status unstable" Fpath.pp_quoted f
end

module Op = struct
  let file_write_color = [`Faint; `Fg `Green]
  let pp_file_contents = Fmt.truncated ~max:150
  let pp_error_msg ppf e = Fmt.pf ppf "@[error: %s@]" e

  let kind_name_padded = function
  | B00.Op.Spawn _ -> "spawn" | Read _ -> "read " | Write _ -> "write"
  | Copy _ -> "copy " | Mkdir _ -> "mkdir" | Wait_files _ -> "wait "

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
    | `File f -> Fpath.pp_quoted ppf f
    | `Tee f -> Fmt.pf ppf "@[<hov><ui> and@ %a@]" Fpath.pp_quoted f

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
      let pp_opt_path = Fmt.(option ~none:none) Fpath.pp_quoted in
      Fmt.record @@
      [ Fmt.field "cmd" Fmt.id pp_cmd;
        Fmt.field "env" Op.Spawn.env pp_env;
        Fmt.field "relevant_env" Op.Spawn.relevant_env pp_env;
        Fmt.field "cwd" Op.Spawn.cwd Fpath.pp_quoted;
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
      [ Fmt.field "file" Op.Read.file Fpath.pp_quoted;
        Fmt.field "result" Op.Read.result pp_result ]
  end

  module Write = struct
    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok () -> Fmt.string ppf "written"

    let pp =
      Fmt.concat @@
      [ Fmt.field "file" Op.Write.file Fpath.pp_quoted;
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
      [ Fmt.field "dir" Op.Mkdir.dir Fpath.pp_quoted;
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
  | Op.Wait_files _ -> ()

  let pp_kind_short o ppf = function
  | Op.Spawn s -> Spawn.pp_cmd ppf s
  | Op.Read r -> Fpath.pp_quoted ppf (Op.Read.file r)
  | Op.Write w ->
      (Fmt.tty file_write_color Fpath.pp_quoted) ppf (Op.Write.file w)
  | Op.Mkdir m -> Fpath.pp_quoted ppf (Op.Mkdir.dir m)
  | Op.Copy c ->
      Fmt.pf ppf "%a to %a"
        Fpath.pp_quoted (Op.Copy.src c)
        (Fmt.tty file_write_color Fpath.pp_quoted) (Op.Copy.dst c)
  | Op.Wait_files _ ->
      Fmt.pf ppf "@[<v>%a@]" (Fmt.list Fpath.pp_quoted) (Op.reads o)

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
    let pp_file_write = Fmt.tty file_write_color Fpath.pp_quoted in
    Fmt.braces @@ Fmt.list pp_file_write

  let pp_op =
    let pp_span ppf s =
      Fmt.pf ppf "%a (%ans)" Time.Span.pp s Time.Span.pp_ns s
    in
    let pp_reads = Fmt.braces @@ Fmt.list Fpath.pp_quoted in
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

  let enc_time_span b s = Bin.enc_int64 b (Time.Span.to_uint64_ns s)
  let dec_time_span s i =
    let i, u = Bin.dec_int64 s i in
    i, Time.Span.of_uint64_ns u

  let enc_hash b h = Bin.enc_string b (Hash.to_bytes h)
  let dec_hash s i =
    let i, s = Bin.dec_string s i in
    i, Hash.of_bytes s

  let enc_status b = function
  | Op.Aborted -> Bin.enc_byte b 0
  | Op.Executed -> Bin.enc_byte b 1
  | Op.Failed -> Bin.enc_byte b 2
  | Op.Waiting -> Bin.enc_byte b 3

  let dec_status s i =
    let kind = "Op.status" in
    let next, b = Bin.dec_byte ~kind s i in
    match b with
    | 0 -> next, Op.Aborted
    | 1 -> next, Op.Executed
    | 2 -> next, Op.Failed
    | 3 -> next, Op.Waiting
    | b -> Bin.err_byte ~kind i b

  let enc_copy b c =
    Bin.enc_fpath b (Op.Copy.src c);
    Bin.enc_fpath b (Op.Copy.dst c);
    Bin.enc_int b (Op.Copy.mode c);
    Bin.enc_option Bin.enc_int b (Op.Copy.linenum c);
    Bin.enc_result ~ok:Bin.enc_unit ~error:Bin.enc_string b (Op.Copy.result c)

  let dec_copy s i =
    let i, src = Bin.dec_fpath s i in
    let i, dst = Bin.dec_fpath s i in
    let i, mode = Bin.dec_int s i in
    let i, linenum = Bin.dec_option Bin.dec_int s i in
    let i, result = Bin.dec_result ~ok:Bin.dec_unit ~error:Bin.dec_string s i in
    i, Op.Copy.v ~src ~dst ~mode ~linenum ~result

  let enc_mkdir b m =
    Bin.enc_fpath b (Op.Mkdir.dir m);
    Bin.enc_result ~ok:Bin.enc_bool ~error:Bin.enc_string b (Op.Mkdir.result m)

  let dec_mkdir s i =
    let i, dir = Bin.dec_fpath s i in
    let i, result = Bin.dec_result ~ok:Bin.dec_bool ~error:Bin.dec_string s i in
    i, Op.Mkdir.v ~dir ~result

  let enc_read b r = (* we don't save the data it's already on the FS *)
    Bin.enc_fpath b (Op.Read.file r);
    let r = Result.bind (Op.Read.result r) @@ fun _ -> Ok () in
    Bin.enc_result ~ok:Bin.enc_unit ~error:Bin.enc_string b r

  let dec_read s i =
    let i, file = Bin.dec_fpath s i in
    let i, result = Bin.dec_result ~ok:Bin.dec_unit ~error:Bin.dec_string s i in
    let result = Result.bind result @@ fun _ -> Ok "<see read file>" in
    i, Op.Read.v ~file ~result

  let enc_spawn_stdo b = function
  | `Ui -> Bin.enc_byte b 0
  | `File p -> Bin.enc_byte b 1; Bin.enc_fpath b p
  | `Tee p -> Bin.enc_byte b 1; Bin.enc_fpath b p

  let dec_spawn_stdo s i =
    let kind = "Op.spawn_stdo" in
    let next, b = Bin.dec_byte ~kind s i in
    match b with
    | 0 -> next, `Ui
    | 1 -> let i, p = Bin.dec_fpath s next in i, `File p
    | 2 -> let i, p = Bin.dec_fpath s next in i, `Tee p
    | b -> Bin.err_byte ~kind i b

  let enc_cmd b cmd =
    let arg b a = Bin.enc_byte b 0; Bin.enc_string b a in
    let shield b = Bin.enc_byte b 1 in
    let append b = Bin.enc_byte b 2 in
    let empty b = Bin.enc_byte b 3 in
    Cmd.iter_enc ~arg ~shield ~append ~empty b cmd

  let rec dec_cmd s i =
    let kind = "Cmd.t" in
    let next, b = Bin.dec_byte ~kind s i in
    match b with
    | 0 -> let i, s = Bin.dec_string s next in i, Cmd.arg s
    | 1 -> let i, cmd = dec_cmd s next in i, Cmd.shield cmd
    | 2 ->
        let i, cmd0 = dec_cmd s next in
        let i, cmd1 = dec_cmd s i in
        i, Cmd.append cmd1 cmd0
    | 3 -> next, Cmd.empty
    | b -> Bin.err_byte ~kind i b

  let enc_os_cmd_status b = function
  | `Exited c -> Bin.enc_byte b 0; Bin.enc_int b c
  | `Signaled c -> Bin.enc_byte b 1; Bin.enc_int b c

  let dec_os_cmd_status s i =
    let kind = "Os.Cmd.status" in
    let next, b = Bin.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = Bin.dec_int s next in i, `Exited c
    | 1 -> let i, c = Bin.dec_int s next in i, `Signaled c
    | b -> Bin.err_byte ~kind i b

  let enc_spawn b s =
    Bin.enc_list Bin.enc_string b (Op.Spawn.env s);
    Bin.enc_list Bin.enc_string b (Op.Spawn.relevant_env s);
    Bin.enc_fpath b (Op.Spawn.cwd s);
    Bin.enc_option Bin.enc_fpath b (Op.Spawn.stdin s);
    enc_spawn_stdo b (Op.Spawn.stdout s);
    enc_spawn_stdo b (Op.Spawn.stderr s);
    Bin.enc_list Bin.enc_int b (Op.Spawn.success_exits s);
    Bin.enc_fpath b (Op.Spawn.tool s);
    enc_cmd b (Op.Spawn.args s);
    Bin.enc_string b (Op.Spawn.stamp s);
    Bin.enc_option (Bin.enc_result ~ok:Bin.enc_string ~error:Bin.enc_string)
      b (Op.Spawn.stdo_ui s);
    Bin.enc_result ~ok:enc_os_cmd_status ~error:Bin.enc_string
      b (Op.Spawn.result s)

  let dec_spawn s i =
    let i, env = Bin.dec_list Bin.dec_string s i in
    let i, relevant_env = Bin.dec_list Bin.dec_string s i in
    let i, cwd = Bin.dec_fpath s i in
    let i, stdin = Bin.dec_option Bin.dec_fpath s i in
    let i, stdout = dec_spawn_stdo s i in
    let i, stderr = dec_spawn_stdo s i in
    let i, success_exits = Bin.dec_list Bin.dec_int s i in
    let i, tool = Bin.dec_fpath s i in
    let i, args = dec_cmd s i in
    let i, stamp = Bin.dec_string s i in
    let i, stdo_ui =
      let ok = Bin.dec_string and error = Bin.dec_string in
      Bin.dec_option (Bin.dec_result ~ok ~error) s i
    in
    let i, result =
      let ok = dec_os_cmd_status and error = Bin.dec_string in
      Bin.dec_result ~ok ~error s i
    in
    i, Op.Spawn.v ~env ~relevant_env ~cwd ~stdin ~stdout ~stderr ~success_exits
      tool args ~stamp ~stdo_ui ~result

  let enc_wait_files b wait = Bin.enc_unit b ()
  let dec_wait_files s i =
    let i, () = Bin.dec_unit s i in
    i, Op.Wait_files.v ()

  let enc_write b w =
    Bin.enc_string b (Op.Write.stamp w);
    Bin.enc_int b (Op.Write.mode w);
    Bin.enc_fpath b (Op.Write.file w);
    Bin.enc_result
      ~ok:Bin.enc_unit ~error:Bin.enc_string b (Op.Write.result w)

  let dec_write s i =
    let i, stamp = Bin.dec_string s i in
    let i, mode = Bin.dec_int s i in
    let i, file = Bin.dec_fpath s i in
    let data () = Error "Serialized op, data fun not available" in
    let i, result = Bin.dec_result ~ok:Bin.dec_unit ~error:Bin.dec_string s i in
    i, Op.Write.v ~stamp ~mode ~file ~data ~result

  let enc_kind b = function
  | Op.Copy copy -> Bin.enc_byte b 0; enc_copy b copy
  | Op.Mkdir mkdir -> Bin.enc_byte b 1; enc_mkdir b mkdir
  | Op.Read read -> Bin.enc_byte b 2; enc_read b read
  | Op.Spawn spawn -> Bin.enc_byte b 3; enc_spawn b spawn
  | Op.Wait_files wait -> Bin.enc_byte b 4; enc_wait_files b wait
  | Op.Write write -> Bin.enc_byte b 5; enc_write b write

  let dec_kind s i =
    let kind = "Op.kind" in
    let next, b = Bin.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = dec_copy s next in i, Op.Copy c
    | 1 -> let i, m = dec_mkdir s next in i, Op.Mkdir m
    | 2 -> let i, r = dec_read s next in i, Op.Read r
    | 3 -> let i, s = dec_spawn s next in i, Op.Spawn s
    | 4 -> let i, w = dec_wait_files s next in i, Op.Wait_files w
    | 5 -> let i, w = dec_write s next in i, Op.Write w
    | b -> Bin.err_byte ~kind i b

  let enc_op b o =
    Bin.enc_int b (Op.id o);
    Bin.enc_string b (Op.group o);
    enc_time_span b (Op.creation_time o);
    enc_time_span b (Op.exec_start_time o);
    enc_time_span b (Op.exec_end_time o);
    Bin.enc_bool b (Op.exec_revived o);
    enc_status b (Op.status o);
    Bin.enc_list Bin.enc_fpath b (Op.reads o);
    Bin.enc_list Bin.enc_fpath b (Op.writes o);
    enc_hash b (Op.hash o);
    enc_kind b (Op.kind o);
    ()

  let dec_op s i =
    let i, id = Bin.dec_int s i in
    let i, group = Bin.dec_string s i in
    let i, creation_time = dec_time_span s i in
    let i, exec_start_time = dec_time_span s i in
    let i, exec_end_time = dec_time_span s i in
    let i, exec_revived = Bin.dec_bool s i in
    let i, status = dec_status s i in
    let i, reads = Bin.dec_list Bin.dec_fpath s i in
    let i, writes = Bin.dec_list Bin.dec_fpath s i in
    let i, hash = dec_hash s i in
    let i, kind = dec_kind s i in
    i, Op.v id ~group ~creation_time ~exec_start_time ~exec_end_time
      ~exec_revived ~status ~reads ~writes ~hash kind


  let magic = "b\x00\x00\x00"
  let to_string ops =
    let b = Buffer.create (1024 * 1024) in
    Bin.enc_magic b magic;
    Bin.enc_list enc_op b ops;
    Buffer.contents b

  let of_string ?(file = Os.File.dash) s =
    try
      let i = Bin.dec_magic s 0 magic in
      let i, ops = Bin.dec_list dec_op s i in
      Bin.dec_eoi s i;
      Ok ops
    with
    | Failure e -> Fmt.error "%a:%s" Fpath.pp_unquoted file e
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
