(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B000

module Op = struct

  (* Formatting *)

  let style_op_id = [ `Bold ]
  let style_file_write = [`Fg `Green]
  let style_file_delete = [`Fg `Red]
  let style_hash = [`Italic]
  let style_subfield = [`Italic]
  let style_cmd_brackets = [`Bold]
  let style_cmd_tool = [`Fg `Blue]
  let style_notify_warn = [`Fg `Yellow]
  let style_notify_info = [`Fg `Blue]
  let style_status_exec_revived = []
  let style_status_exec = [`Fg (`Hi `Green)]
  let style_status_failed = [`Fg `Red]
  let style_status_aborted = [`Faint; `Fg `Red]
  let style_status_waiting = [`Fg `Cyan]
  let style_kind_name = [`Bold]
  let style_err = [`Fg `Red]
  let style_op_howto = [`Faint]

  let op_field = Fmt.field
  let pp_path_option = Fmt.(option ~none:none) Fpath.pp_quoted
  let pp_file_read = Fpath.pp_quoted
  let pp_file_write = Fmt.tty style_file_write Fpath.pp_quoted
  let pp_file_wait = Fpath.pp_quoted
  let pp_file_delete = Fmt.tty style_file_delete Fpath.pp_quoted
  let pp_file_mode ppf m = Fmt.pf ppf "%o" m
  let pp_hash = Fmt.tty style_hash Hash.pp
  let pp_file_contents = Fmt.truncated ~max:150
  let pp_error_msg ppf e = Fmt.pf ppf "@[error: %s@]" e
  let pp_subfield_label = Fmt.tty_string style_subfield
  let pp_subfield s f pp = Fmt.field ~label:pp_subfield_label ~sep:Fmt.sp s f pp
  let pp_reads = Fmt.list pp_file_read
  let pp_writes = Fmt.list pp_file_write
  let pp_timings =
    let pp_span = Time.Span.pp in
    let wait o = Time.Span.abs_diff (Op.time_created o) (Op.time_started o) in
    op_field "timings" Fmt.id @@
    Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
    [ pp_subfield "duration" Op.duration pp_span;
      pp_subfield "created" Op.time_created pp_span;
      pp_subfield "started" Op.time_started pp_span;
      pp_subfield "waited" wait pp_span; ]

  let pp_spawn_cmd ppf s =
    let args = Cmd.to_list (Op.Spawn.args s) in
    let quote = Filename.quote in
    let pquote p = Filename.quote (Fpath.to_string p) in
    let pp_brack = Fmt.tty_string style_cmd_brackets in
    let pp_tool ppf t = Fmt.tty_string style_cmd_tool ppf (pquote t) in
    let pp_arg ppf a = Fmt.pf ppf "%s" (quote a) in
    let pp_o_arg ppf a = Fmt.tty_string style_file_write ppf (quote a) in
    let rec pp_args last_was_o ppf = function
    | [] -> ()
    | a :: args ->
        Fmt.char ppf ' ';
        if last_was_o then pp_o_arg ppf a else pp_arg ppf a;
        pp_args (String.equal a "-o") ppf args
    in
    let pp_stdin ppf = function
    | None -> () | Some file -> Fmt.pf ppf "< %s" (pquote file)
    in
    let pp_stdo redir ppf = function
    | `Ui -> ()
    | `Tee f | `File f ->
        Fmt.pf ppf " %s %a" redir pp_o_arg (Fpath.to_string f)
    in
    Fmt.pf ppf "@[<h>"; pp_brack ppf "[";
    pp_tool ppf (Op.Spawn.tool s); pp_args false ppf args;
    pp_stdin ppf (Op.Spawn.stdin s);
    pp_stdo ">" ppf (Op.Spawn.stdout s);
    pp_stdo "2>" ppf (Op.Spawn.stderr s);
    pp_brack ppf "]"; Fmt.pf ppf "@]"

  let pp_spawn_success_exits ppf = function
  | [] -> Fmt.string ppf "any"
  | cs -> Fmt.(list ~sep:comma int) ppf cs

  let pp_spawn_stdo ppf = function
  | `Ui -> Fmt.pf ppf "<ui>"
  | `File f -> Fpath.pp_quoted ppf f
  | `Tee f -> Fmt.pf ppf "@[<hov><ui> and@ %a@]" Fpath.pp_quoted f

  let pp_spawn_stdo_ui ppf s = match (Op.Spawn.stdo_ui s) with
  | None -> Fmt.none ppf ()
  | Some (Ok d) -> String.pp ppf d
  | Some (Error e) -> pp_error_msg ppf e

  let pp_spawn_status ppf = function
  | `Exited c -> (Fmt.brackets Fmt.int) ppf c
  | `Signaled c -> (Fmt.brackets Fmt.(any "signaled:" ++ Fmt.int)) ppf c

  let pp_spawn_stdio =
    Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
    [ pp_subfield "stdin" Op.Spawn.stdin pp_path_option;
      pp_subfield "stderr" Op.Spawn.stderr pp_spawn_stdo;
      pp_subfield "stdout" Op.Spawn.stdout pp_spawn_stdo; ]

  let pp_spawn_result ppf = function
  | Ok st -> pp_spawn_status ppf st | Error e -> Fmt.pf ppf " %a" pp_error_msg e

  let pp_notify ppf n =
    let label, s = match Op.Notify.kind n with
    | `Warn -> "WARNING", style_notify_warn
    | `Start -> "START", style_notify_info
    | `End -> "END", style_notify_info
    | `Info -> "NOTE", style_notify_info
    in
    Fmt.pf ppf "[%a] @[%a@]"
      (Fmt.tty_string s) label Fmt.lines (Op.Notify.msg n)

  (* Operation header formatting *)

  let pp_status ppf o = match Op.status o with
  | Op.Executed -> ()
  | Op.Failed _ -> Fmt.tty_string style_status_failed ppf "FAILED "
  | Op.Aborted -> Fmt.tty_string style_status_aborted ppf "ABORTED "
  | Op.Waiting -> Fmt.tty_string style_status_waiting ppf "WAITING "

  let pp_revived ppf o = match Op.revived o with
  | true -> Fmt.tty_string style_status_exec_revived ppf "R"
  | false -> Fmt.tty_string style_status_exec ppf "E"

  let pp_op_hash ppf o =
    let h = Op.hash o in if Hash.is_nil h then () else
    (Fmt.sp ppf (); pp_revived ppf o; Fmt.char ppf ':'; pp_hash ppf h)

  let pp_kind_name ppf o =
    (Fmt.tty_string style_kind_name) ppf (Op.kind_name (Op.kind o))

  let pp_id ppf o =
    let pp_id ppf id = Fmt.pf ppf "%03d" id in
    Fmt.tty style_op_id pp_id ppf (Op.id o)

  let pp_failure ppf = function
  | Op.Exec m -> Fmt.lines ppf m
  | Op.Missing_writes fs ->
      Fmt.pf ppf "@[<v>Did not write:@,%a@]" (Fmt.list pp_file_write) fs

  let pp_op_failure ppf o = match Op.status o with
  | Failed f ->
      Fmt.pf ppf "@[%a: @[%a@]@]@,"
        (Fmt.tty_string style_err) "error" pp_failure f
  | _ -> ()

  let pp_op_failure_and_timings ppf o = pp_op_failure ppf o; pp_timings ppf o

  let pp_header_long ppf o = (* adds the group *)
    Fmt.pf ppf "[%a%a:%a %a %s%a]"
      pp_status o pp_id o pp_kind_name o
      Time.Span.pp (Op.duration o) (Op.group o)
      pp_op_hash o

  (* Short formatting *)

  let pp_copy_short ppf c =
    Fmt.pf ppf "%a to %a"
      pp_file_read (Op.Copy.src c) pp_file_write (Op.Copy.dst c)

  let pp_spawn_short =
    Fmt.(pp_spawn_cmd ++ using Op.Spawn.result pp_spawn_result)

  let pp_kind_short ppf o = match Op.kind o with
  | Op.Copy c -> pp_copy_short ppf c
  | Op.Delete d -> pp_file_delete ppf (Op.Delete.path d)
  | Op.Mkdir m -> pp_file_write ppf (Op.Mkdir.dir m)
  | Op.Notify n -> pp_notify ppf n
  | Op.Read r -> pp_file_read ppf (Op.Read.file r)
  | Op.Spawn s -> pp_spawn_short ppf s
  | Op.Wait_files _ -> (Fmt.vbox @@ Fmt.list pp_file_wait) ppf (Op.reads o)
  | Op.Write w -> pp_file_write ppf (Op.Write.file w)

  let pp_header_short ppf o =
    Fmt.pf ppf "[%a%a:%a %a%a]"
      pp_status o pp_id o pp_kind_name o Time.Span.pp (Op.duration o)
      pp_op_hash o

  let pp_short ppf o = match Op.kind o with
  | Op.Notify n -> (Fmt.box @@ pp_notify) ppf n
  | _ -> Fmt.pf ppf "@[<h>%a %a@]" pp_header_short o pp_kind_short o

  let pp_short_ui ppf o = match Op.status o with
  | Op.Failed fail ->
      begin match Op.kind o with
      | Op.Spawn s when Op.Spawn.stdo_ui s <> None ->
          Fmt.pf ppf "@[<v>%a@,@,%a@,%a@]"
            pp_short o pp_spawn_stdo_ui s pp_failure fail
      |  _ ->
          Fmt.pf ppf "@[<v>%a@,%a@]" pp_short o pp_failure fail
      end
  | _ ->
      match Op.kind o with
      | Op.Notify n -> (Fmt.box @@ pp_notify) ppf n
      | Op.Spawn s when Op.Spawn.stdo_ui s <> None ->
          Fmt.pf ppf "@[<v>%a@,@,%a@]" pp_short o pp_spawn_stdo_ui s
      | _ -> pp_short ppf o

  (* Simple ui formatting *)

  let pp_ui_kind ppf o = match Op.kind o with
  | Op.Spawn s ->
      let name = Fpath.basename (Op.Spawn.tool s) in
      (Fmt.tty_string style_cmd_tool) ppf name
  | k -> pp_kind_short ppf o

  let pp_ui_header ~op_howto ppf o =
    let pp_group ppf o = match Op.group o with
    | "" -> () | g -> Fmt.string ppf g; Fmt.sp ppf ()
    in
    let pp_status ppf o = match Op.status o with
    | Op.Failed _ ->
        Fmt.pf ppf "[%a] " (Fmt.tty_string style_status_failed) "FAILED"
    | _ -> ()
    in
    let pp_exit_code ppf o = match Op.kind o with
    | Op.Spawn s ->
        begin match Op.Spawn.result s with
        | Error _ -> ()
        | Ok s -> pp_spawn_status ppf s
        end
    | _ -> ()
    in
    let op_howto =
      Fmt.(parens @@ any "see " ++ quote ~mark:"'" op_howto)
    in
    Fmt.pf ppf "@[%a[%a%a]%a: %a"
      pp_status o pp_group o pp_ui_kind o pp_exit_code o
      Fmt.(tty [`Faint] (op_howto)) o

  let pp_ui ~sep ~op_howto ppf o = match Op.status o with
  | Op.Failed fail ->
      begin match Op.kind o with
      | Op.Spawn s when Op.Spawn.stdo_ui s <> None ->
          Fmt.pf ppf "@[<v>%a@,@,%a@,%a@]%a"
            (pp_ui_header ~op_howto) o pp_spawn_stdo_ui s
            pp_failure fail sep ()
      |  _ ->
          Fmt.pf ppf "@[<v>%a@,%a@]%a"
            (pp_ui_header ~op_howto) o pp_failure fail sep ()
        end
  | _ ->
      match Op.kind o with
      | Op.Notify n -> (Fmt.box @@ pp_notify) ppf n
      | Op.Spawn s when Op.Spawn.stdo_ui s <> None ->
          Fmt.pf ppf "@[<v>%a@,@,%a@]%a"
            (pp_ui_header ~op_howto) o pp_spawn_stdo_ui s sep ()
      | _ -> ()

  (* Long formatting *)

  let pp_copy_long =
    let pp_info =
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ pp_subfield "mode" Op.Copy.mode pp_file_mode;
        pp_subfield "linenum" Op.Copy.linenum Fmt.(option ~none:none int) ]
    in
    Fmt.record
      [ op_field "src" Op.reads pp_reads;
        op_field "dst" Op.writes pp_writes;
        Fmt.using Op.Copy.get pp_info;
        pp_op_failure_and_timings; ]

  let pp_delete_long =
    let pp_path = Fmt.using Op.Delete.path pp_file_delete in
    Fmt.record
      [ Fmt.using Op.Delete.get pp_path; pp_op_failure_and_timings; ]

  let pp_mkdir_long =
    let pp_path = Fmt.using Op.Mkdir.dir pp_file_write in
    Fmt.record
      [ Fmt.using Op.Mkdir.get pp_path; pp_op_failure_and_timings; ]

  let pp_notify_long =
    Fmt.record [ Fmt.using Op.Notify.get pp_notify; pp_op_failure_and_timings; ]

  let pp_read_long =
    let pp_path = Fmt.using Op.Read.file pp_file_read in
    Fmt.record
      [ Fmt.using Op.Read.get pp_path;
        pp_op_failure_and_timings; ]

  let pp_spawn_long =
    (* FIXME add Cmd stamp env stamp *)
    let pp_exit =
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ op_field "exit" Op.Spawn.result pp_spawn_result;
        pp_subfield "success-exits" Op.Spawn.success_exits
          pp_spawn_success_exits ]
    in
    let pp_env = Fmt.(vbox @@ list string) in
    let pp_relevant_env = Fmt.using Op.Spawn.relevant_env pp_env in
    let pp_env = Fmt.using Op.Spawn.env pp_env in
    let pp_cwd = Fmt.using Op.Spawn.cwd Fpath.pp_quoted in
    let pp_stamps =
      let cmd_stamp s = Cmd.to_stamp (Op.Spawn.args s) in
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ pp_subfield "stamp" Op.Spawn.stamp Fmt.string;
        pp_subfield "args" cmd_stamp Fmt.(list ~sep:sp string) ]
    in
    Fmt.record [
      Fmt.using Op.Spawn.get pp_spawn_cmd;
      Fmt.using Op.Spawn.get pp_exit;
      op_field "writes" Op.writes pp_writes;
      op_field "reads" Op.reads pp_reads;
      op_field "stdo-ui" Op.Spawn.get pp_spawn_stdo_ui;
      op_field "stdio" Op.Spawn.get pp_spawn_stdio;
      op_field "cwd" Op.Spawn.get pp_cwd;
      op_field "env" Op.Spawn.get pp_env;
      op_field "relevant-env" Op.Spawn.get pp_relevant_env;
      op_field "stamps" Op.Spawn.get pp_stamps;
      pp_timings ]

  let pp_wait_files_long =
    Fmt.record [Fmt.using Op.reads pp_reads; pp_timings]

  let pp_write_long =
    let pp_stamp = Fmt.using Op.Write.stamp Fmt.string in
    let pp_mode = Fmt.using Op.Write.mode pp_file_mode in
    let pp_write = Fmt.using Op.Write.file pp_file_write in
    Fmt.record
      [ Fmt.using Op.Write.get pp_write;
        op_field "stamp" Op.Write.get pp_stamp;
        op_field "mode" Op.Write.get pp_mode;
        pp_op_failure_and_timings; ]

  let pp_kind_long ppf o = match Op.kind o with
  | Op.Copy _ -> pp_copy_long ppf o
  | Op.Delete _ -> pp_delete_long ppf o
  | Op.Mkdir _ -> pp_mkdir_long ppf o
  | Op.Notify _ -> pp_notify_long ppf o
  | Op.Read _ -> pp_read_long ppf o
  | Op.Spawn _ -> pp_spawn_long ppf o
  | Op.Wait_files _ -> pp_wait_files_long ppf o
  | Op.Write _ -> pp_write_long ppf o

  let pp ppf o =
    Fmt.pf ppf "@[<v>@[<h>%a@]@, @[%a@]@,@]" pp_header_long o pp_kind_long o

  (* Binary serialization *)

  let enc_time_span b s = Binc.enc_int64 b (Time.Span.to_uint64_ns s)
  let dec_time_span s i =
    let i, u = Binc.dec_int64 s i in
    i, Time.Span.of_uint64_ns u

  let enc_hash b h = Binc.enc_string b (Hash.to_bytes h)
  let dec_hash s i =
    let i, s = Binc.dec_string s i in
    i, Hash.of_bytes s

  let enc_failure b = function
  | Op.Exec e -> Binc.enc_byte b 0; Binc.enc_string b e
  | Op.Missing_writes fs -> Binc.enc_byte b 1; Binc.enc_list Binc.enc_fpath b fs

  let dec_failure s i =
    let kind = "Op.failure" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 ->
        let i, e = Binc.dec_string s next in
        i, Op.Exec e
    | 1 ->
        let i, fs = Binc.dec_list Binc.dec_fpath s next in
        i, Op.Missing_writes fs
    | b -> Binc.err_byte ~kind i b

  let enc_status b = function
  | Op.Aborted -> Binc.enc_byte b 0
  | Op.Executed -> Binc.enc_byte b 1
  | Op.Failed f -> Binc.enc_byte b 2; enc_failure b f
  | Op.Waiting -> Binc.enc_byte b 3

  let dec_status s i =
    let kind = "Op.status" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> next, Op.Aborted
    | 1 -> next, Op.Executed
    | 2 ->
        let i, f = dec_failure s next in
        i, Op.Failed f
    | 3 -> next, Op.Waiting
    | b -> Binc.err_byte ~kind i b

  let enc_copy b c =
    Binc.enc_fpath b (Op.Copy.src c);
    Binc.enc_fpath b (Op.Copy.dst c);
    Binc.enc_int b (Op.Copy.mode c);
    Binc.enc_option Binc.enc_int b (Op.Copy.linenum c)

  let dec_copy s i =
    let i, src = Binc.dec_fpath s i in
    let i, dst = Binc.dec_fpath s i in
    let i, mode = Binc.dec_int s i in
    let i, linenum = Binc.dec_option Binc.dec_int s i in
    i, Op.Copy.v ~src ~dst ~mode ~linenum

  let enc_delete b d = Binc.enc_fpath b (Op.Delete.path d)
  let dec_delete s i =
    let i, path = Binc.dec_fpath s i in
    i, Op.Delete.v ~path

  let enc_mkdir b m =
    Binc.enc_fpath b (Op.Mkdir.dir m);
    Binc.enc_int b (Op.Mkdir.mode m)

  let dec_mkdir s i =
    let i, dir = Binc.dec_fpath s i in
    let i, mode = Binc.dec_int s i in
    i, Op.Mkdir.v ~mode ~dir

  let enc_notify_kind b = function
  | `Warn -> Binc.enc_byte b 0
  | `Start -> Binc.enc_byte b 1
  | `End -> Binc.enc_byte b 2
  | `Info -> Binc.enc_byte b 3

  let dec_notify_kind s i =
    let kind = "Op.Notify.kind" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> next, `Warn
    | 1 -> next, `Start
    | 2 -> next, `End
    | 3 -> next, `Info
    | b -> Binc.err_byte ~kind i b

  let enc_notify b n =
    enc_notify_kind b (Op.Notify.kind n);
    Binc.enc_string b (Op.Notify.msg n)

  let dec_notify s i =
    let i, kind = dec_notify_kind s i in
    let i, msg = Binc.dec_string s i in
    i, Op.Notify.v ~kind ~msg

  let enc_read b r = Binc.enc_fpath b (Op.Read.file r)
  let dec_read s i =
    let i, file = Binc.dec_fpath s i in
    i, Op.Read.v ~file ~data:""

  let enc_spawn_stdo b = function
  | `Ui -> Binc.enc_byte b 0
  | `File p -> Binc.enc_byte b 1; Binc.enc_fpath b p
  | `Tee p -> Binc.enc_byte b 2; Binc.enc_fpath b p

  let dec_spawn_stdo s i =
    let kind = "Op.spawn_stdo" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> next, `Ui
    | 1 -> let i, p = Binc.dec_fpath s next in i, `File p
    | 2 -> let i, p = Binc.dec_fpath s next in i, `Tee p
    | b -> Binc.err_byte ~kind i b

  let enc_cmd b cmd =
    let arg b a = Binc.enc_byte b 0; Binc.enc_string b a in
    let shield b = Binc.enc_byte b 1 in
    let append b = Binc.enc_byte b 2 in
    let empty b = Binc.enc_byte b 3 in
    Cmd.iter_enc ~arg ~shield ~append ~empty b cmd

  let rec dec_cmd s i =
    let kind = "Cmd.t" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> let i, s = Binc.dec_string s next in i, Cmd.arg s
    | 1 -> let i, cmd = dec_cmd s next in i, Cmd.shield cmd
    | 2 ->
        let i, cmd0 = dec_cmd s next in
        let i, cmd1 = dec_cmd s i in
        i, Cmd.append cmd1 cmd0
    | 3 -> next, Cmd.empty
    | b -> Binc.err_byte ~kind i b

  let enc_os_cmd_status b = function
  | `Exited c -> Binc.enc_byte b 0; Binc.enc_int b c
  | `Signaled c -> Binc.enc_byte b 1; Binc.enc_int b c

  let dec_os_cmd_status s i =
    let kind = "Os.Cmd.status" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = Binc.dec_int s next in i, `Exited c
    | 1 -> let i, c = Binc.dec_int s next in i, `Signaled c
    | b -> Binc.err_byte ~kind i b

  let enc_spawn b s =
    Binc.enc_list Binc.enc_string b (Op.Spawn.env s);
    Binc.enc_list Binc.enc_string b (Op.Spawn.relevant_env s);
    Binc.enc_fpath b (Op.Spawn.cwd s);
    Binc.enc_option Binc.enc_fpath b (Op.Spawn.stdin s);
    enc_spawn_stdo b (Op.Spawn.stdout s);
    enc_spawn_stdo b (Op.Spawn.stderr s);
    Binc.enc_list Binc.enc_int b (Op.Spawn.success_exits s);
    Binc.enc_fpath b (Op.Spawn.tool s);
    enc_cmd b (Op.Spawn.args s);
    Binc.enc_string b (Op.Spawn.stamp s);
    Binc.enc_option (Binc.enc_result ~ok:Binc.enc_string ~error:Binc.enc_string)
      b (Op.Spawn.stdo_ui s);
    Binc.enc_result ~ok:enc_os_cmd_status ~error:Binc.enc_string
      b (Op.Spawn.result s)

  let dec_spawn s i =
    let i, env = Binc.dec_list Binc.dec_string s i in
    let i, relevant_env = Binc.dec_list Binc.dec_string s i in
    let i, cwd = Binc.dec_fpath s i in
    let i, stdin = Binc.dec_option Binc.dec_fpath s i in
    let i, stdout = dec_spawn_stdo s i in
    let i, stderr = dec_spawn_stdo s i in
    let i, success_exits = Binc.dec_list Binc.dec_int s i in
    let i, tool = Binc.dec_fpath s i in
    let i, args = dec_cmd s i in
    let i, stamp = Binc.dec_string s i in
    let i, stdo_ui =
      let ok = Binc.dec_string and error = Binc.dec_string in
      Binc.dec_option (Binc.dec_result ~ok ~error) s i
    in
    let i, result =
      let ok = dec_os_cmd_status and error = Binc.dec_string in
      Binc.dec_result ~ok ~error s i
    in
    i, Op.Spawn.v ~env ~relevant_env ~cwd ~stdin ~stdout ~stderr ~success_exits
      tool args ~stamp ~stdo_ui ~result

  let enc_wait_files b wait = Binc.enc_unit b ()
  let dec_wait_files s i =
    let i, () = Binc.dec_unit s i in
    i, Op.Wait_files.v ()

  let enc_write b w =
    Binc.enc_string b (Op.Write.stamp w);
    Binc.enc_int b (Op.Write.mode w);
    Binc.enc_fpath b (Op.Write.file w)

  let dec_write s i =
    let i, stamp = Binc.dec_string s i in
    let i, mode = Binc.dec_int s i in
    let i, file = Binc.dec_fpath s i in
    let data () = Error "deserialized op, data fun not available" in
    i, Op.Write.v ~stamp ~mode ~file ~data

  let enc_kind b = function
  | Op.Copy c -> Binc.enc_byte b 0; enc_copy b c
  | Op.Delete d -> Binc.enc_byte b 1; enc_delete b d
  | Op.Mkdir m -> Binc.enc_byte b 2; enc_mkdir b m
  | Op.Notify n -> Binc.enc_byte b 3; enc_notify b n
  | Op.Read r -> Binc.enc_byte b 4; enc_read b r
  | Op.Spawn s -> Binc.enc_byte b 5; enc_spawn b s
  | Op.Wait_files w -> Binc.enc_byte b 6; enc_wait_files b w
  | Op.Write w -> Binc.enc_byte b 7; enc_write b w

  let dec_kind s i =
    let kind = "Op.kind" in
    let next, b = Binc.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = dec_copy s next in i, Op.Copy c
    | 1 -> let i, d = dec_delete s next in i, Op.Delete d
    | 2 -> let i, m = dec_mkdir s next in i, Op.Mkdir m
    | 3 -> let i, n = dec_notify s next in i, Op.Notify n
    | 4 -> let i, r = dec_read s next in i, Op.Read r
    | 5 -> let i, s = dec_spawn s next in i, Op.Spawn s
    | 6 -> let i, w = dec_wait_files s next in i, Op.Wait_files w
    | 7 -> let i, w = dec_write s next in i, Op.Write w
    | b -> Binc.err_byte ~kind i b

  let enc b o =
    Binc.enc_int b (Op.id o);
    Binc.enc_string b (Op.group o);
    enc_time_span b (Op.time_created o);
    enc_time_span b (Op.time_started o);
    enc_time_span b (Op.duration o);
    Binc.enc_bool b (Op.revived o);
    enc_status b (Op.status o);
    Binc.enc_list Binc.enc_fpath b (Op.reads o);
    Binc.enc_list Binc.enc_fpath b (Op.writes o);
    enc_hash b (Op.hash o);
    enc_kind b (Op.kind o);
    ()

  let dec s i =
    let k o = invalid_arg "deserialized op, no kontinuation" in
    let i, id = Binc.dec_int s i in
    let i, group = Binc.dec_string s i in
    let i, time_created = dec_time_span s i in
    let i, time_started = dec_time_span s i in
    let i, duration = dec_time_span s i in
    let i, revived = Binc.dec_bool s i in
    let i, status = dec_status s i in
    let i, reads = Binc.dec_list Binc.dec_fpath s i in
    let i, writes = Binc.dec_list Binc.dec_fpath s i in
    let i, hash = dec_hash s i in
    let i, kind = dec_kind s i in
    i, Op.v id ~group ~time_created ~time_started ~duration ~revived
      ~status ~reads ~writes ~hash ~k kind
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
