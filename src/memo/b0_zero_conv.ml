(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_zero

module Op = struct

  let failure_to_string = function
  | Op.Exec None -> "failed"
  | Op.Exec (Some msg) -> Fmt.str "failed: %s" msg
  | Op.Missing_writes fs ->
      Fmt.str "@[<v>failed: Did not write:@,%a@]" (Fmt.list Fpath.pp_quoted) fs
  | Op.Missing_reads fs ->
      Fmt.str "@[<v>failed: Could not read:@,%a@]" (Fmt.list Fpath.pp_quoted) fs

  let status_to_string = function
  | Op.Aborted -> "aborted" | Op.Success -> "success" | Op.Waiting -> "waiting"
  | Op.Failed f -> failure_to_string f

  let notify_kind_to_string = function
  | `End -> "end" | `Fail -> "fail" | `Info -> "info" | `Start -> "start"
  | `Warn -> "warn"

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
  let style_status_failed = [`Fg (`Hi `Red)]
  let style_status_aborted = [`Fg `Cyan]
  let style_status_waiting = [`Fg `Cyan]
  let style_kind_name = [`Bold]
  let style_err = [`Fg `Red]
  let style_op_howto = [`Faint]

  let pp_file = Fpath.pp_quoted
  let pp_file_read = pp_file
  let pp_file_write = Fmt.tty' style_file_write pp_file
  let pp_file_delete = Fmt.tty' style_file_delete pp_file
  let pp_hash = Fmt.tty' style_hash Hash.pp
  let pp_subfield_label = Fmt.tty style_subfield
  let pp_subfield s f pp = Fmt.field ~label:pp_subfield_label ~sep:Fmt.sp s f pp
  let pp_did_not_write ppf fs =
    Fmt.pf ppf "@[<v>Did not write:@,%a@]" (Fmt.list pp_file_write) fs

  let pp_cannot_read ppf fs =
    Fmt.pf ppf "@[<v>Cannot read:@,%a@]" (Fmt.list pp_file_write) fs

  let pp_spawn_exit ppf = function
  | Some (`Exited c) -> (Fmt.brackets Fmt.int) ppf c
  | Some (`Signaled c) -> (Fmt.brackets Fmt.(any "signaled:" ++ Fmt.int)) ppf c
  | None -> ()

  let pp_spawn_cmd ~single_line ppf s =
    let args = Cmd.to_list (Op.Spawn.args s) in
    let quote = Filename.quote in
    let pquote p = Filename.quote (Fpath.to_string p) in
    let pp_brack = Fmt.tty style_cmd_brackets in
    let pp_tool ppf t = Fmt.tty style_cmd_tool ppf (pquote t) in
    let pp_arg ppf a = Fmt.pf ppf "%s" (quote a) in
    let pp_o_arg ppf a = Fmt.tty style_file_write ppf (quote a) in
    let pp_sep ppf ~last = match last with
    | "-I" | "-L" | "-o" | "-f" | "-d" -> Fmt.char ppf ' '
    | _ -> Fmt.sp ppf ()
    in
    let rec pp_args ~last ppf = function
    | [] -> ()
    | a :: args ->
        pp_sep ppf ~last;
        if String.equal last "-o" then pp_o_arg ppf a else pp_arg ppf a;
        pp_args ~last:a ppf args
    in
    let pp_stdin ppf = function
    | None -> () | Some file -> Fmt.pf ppf "< %s" (pquote file)
    in
    let pp_stdo redir ppf = function
    | `Ui -> ()
    | `Tee f | `File f ->
        Fmt.pf ppf "@ %s %a" redir pp_o_arg (Fpath.to_string f)
    in
    (if single_line then Fmt.pf ppf "@[<h>" else Fmt.pf ppf "@[<1>");
    pp_brack ppf "[";
    pp_tool ppf (Op.Spawn.tool s); pp_args ~last:"" ppf args;
    pp_stdin ppf (Op.Spawn.stdin s);
    pp_stdo ">" ppf (Op.Spawn.stdout s);
    pp_stdo "2>" ppf (Op.Spawn.stderr s);
    pp_brack ppf "]";
    Fmt.pf ppf "@]"

  let pp_spawn_and_exit =
    Fmt.(pp_spawn_cmd ~single_line:true ++ using Op.Spawn.exit pp_spawn_exit)

  let pp_spawn_multi_line_and_exit =
    Fmt.(pp_spawn_cmd ~single_line:false ++ using Op.Spawn.exit pp_spawn_exit)

  let pp_notify ppf o =
    let n = Op.Notify.get o in
    let label, s = match Op.Notify.kind n with
    | `Fail -> "FAIL", style_err
    | `Warn -> "WARN", style_notify_warn
    | `Start -> "START", style_notify_info
    | `End -> "END", style_notify_info
    | `Info -> "NOTE", style_notify_info
    in
    let pp_mark ppf o = match Op.mark o with
    | "" -> () | m -> Fmt.sp ppf (); Fmt.pf ppf "[%a]" Fmt.code m
    in
    Fmt.pf ppf "@[@[<h>[%a]%a@]:@ @[%a@]@]"
      (Fmt.tty s) label pp_mark o Fmt.lines (Op.Notify.msg n)

  let pp_header ppf o =
    let pp_status ppf o = match Op.status o with
    | Op.Success -> ()
    | Op.Failed _ -> Fmt.tty style_status_failed ppf "FAILED "
    | Op.Aborted -> Fmt.tty style_status_aborted ppf "ABORTED "
    | Op.Waiting -> Fmt.tty style_status_waiting ppf "WAITING "
    in
    let pp_id ppf o =
      let pp_id ppf id = Fmt.pf ppf "%03d" id in
      Fmt.tty' style_op_id pp_id ppf (Op.id o)
    in
    let pp_kind_name ppf o =
      (Fmt.tty style_kind_name) ppf (Op.kind_name (Op.kind o))
    in
    let pp_revived ppf o = match Op.revived o with
    | false -> Fmt.tty style_status_exec ppf "e"
    | true -> Fmt.tty style_status_exec_revived ppf "r"
    in
    let pp_op_hash ppf o =
      let h = Op.hash o in if Hash.is_nil h then () else
      (Fmt.sp ppf (); pp_revived ppf o; Fmt.char ppf ':'; pp_hash ppf h)
    in
    let pp_mark ppf o = match Op.mark o with
    | "" -> () | g -> Fmt.sp ppf (); Fmt.code ppf g
    in
    let pp_dur ppf o = match Op.status o with
    | Op.Failed _ | Op.Success ->
        Fmt.sp ppf (); Mtime.Span.pp ppf (Op.duration o)
    | _ -> ()
    in
    Fmt.pf ppf "[%a%a:%a%a%a%a]"
      pp_status o pp_id o pp_kind_name o pp_dur o pp_mark o pp_op_hash o

  let pp_op_extra ppf o =
    let pp_spawn_stdo_ui ppf s = match (Op.Spawn.stdo_ui s) with
    | None -> Fmt.none ppf ()
    | Some (Ok d) -> Fmt.lines ppf (String.trim d)
    | Some (Error e) ->
        Fmt.pf ppf "@[%a: @[%a@]@]"
          (Fmt.tty style_err) "error" Fmt.lines e
    in
    begin match Op.kind o with
    | Op.Spawn s ->
        begin match Op.Spawn.stdo_ui s with
        | None -> ()
        | Some _ -> Fmt.cut ppf (); pp_spawn_stdo_ui ppf s; Fmt.cut ppf ()
        end
    | _ -> ()
    end;
    match Op.status o with
    | Op.Failed failure ->
        begin match failure with
        | Op.Exec None -> ()
        | Op.Exec (Some m) -> Fmt.cut ppf (); Fmt.lines ppf m
        | Op.Missing_writes fs -> Fmt.cut ppf (); pp_did_not_write ppf fs
        | Op.Missing_reads fs -> Fmt.cut ppf (); pp_cannot_read ppf fs
        end
    | _ -> ()

  let pp_kind_line ppf o = match Op.kind o with
  | Op.Copy c ->
      Fmt.pf ppf "%a to %a"
        pp_file_read (Op.Copy.src c) pp_file_write (Op.Copy.dst c)
  | Op.Delete d -> pp_file_delete ppf (Op.Delete.path d)
  | Op.Mkdir m -> pp_file_write ppf (Op.Mkdir.dir m)
  | Op.Notify _ -> pp_notify ppf o
  | Op.Read r -> pp_file_read ppf (Op.Read.file r)
  | Op.Spawn s -> pp_spawn_and_exit ppf s
  | Op.Wait_files _ -> (Fmt.vbox @@ Fmt.list Fpath.pp_quoted) ppf (Op.reads o)
  | Op.Write w -> pp_file_write ppf (Op.Write.file w)

  (* Line formatting *)

  let pp_line ppf o = match Op.kind o with
  | Op.Notify _ -> pp_notify ppf o
  | _ -> Fmt.pf ppf "@[<h>%a %a@]" pp_header o pp_kind_line o

  let pp_line_and_ui ppf o = match Op.kind o with
  | Op.Notify _ -> pp_notify ppf o
  | _ -> Fmt.pf ppf "@[<v>%a%a@]" pp_line o pp_op_extra o

  (* Ui formatting *)

  let pp_ui ~sep ~op_howto ppf o =
    let is_failed = function Op.Failed _ -> true | _ -> false in
    let is_spawn_with_ui o = match Op.kind o with
    | Op.Spawn s when Op.Spawn.stdo_ui s <> None -> true
    | _ -> false
    in
    let pp_ui_header ~op_howto ppf o =
      let pp_ui_kind ppf o = match Op.kind o with
      | Op.Spawn s ->
          let name = Fpath.basename (Op.Spawn.tool s) in
          (Fmt.tty style_cmd_tool) ppf name
      | k -> pp_kind_line ppf o
      in
      let pp_mark ppf o = match Op.mark o with
      | "" -> () | g -> Fmt.code ppf g; Fmt.sp ppf ()
      in
      let pp_status ppf o = match Op.status o with
      | Op.Failed _ ->
          Fmt.pf ppf "[%a] " (Fmt.tty style_status_failed) "FAIL"
      | _ -> ()
      in
      let if_spawn_pp_spawn_exit ppf o = match Op.kind o with
      | Op.Spawn s -> pp_spawn_exit ppf (Op.Spawn.exit s)
      | _ -> ()
      in
      Fmt.pf ppf "@[%a[%a%a]%a: %a@]"
        pp_status o pp_mark o pp_ui_kind o if_spawn_pp_spawn_exit o
        (Fmt.tty' [`Faint] (Fmt.parens op_howto)) o
    in
    match Op.kind o with
    | Op.Notify _ -> pp_notify ppf o; sep ppf ()
    | _ when is_failed (Op.status o) || is_spawn_with_ui o ->
        Fmt.pf ppf "@[<v>%a%a@]%a"
          (pp_ui_header ~op_howto) o pp_op_extra o sep ()
    | _ -> ()

  (* Full formatting *)

  let pp_file_mode ppf m = Fmt.pf ppf "%o" m
  let pp_reads = Fmt.vbox (Fmt.list pp_file_read)
  let pp_writes = Fmt.vbox (Fmt.list pp_file_write)
  let pp_timings =
    let pp_span = Mtime.Span.pp in
    let wait o = Mtime.Span.abs_diff (Op.time_created o) (Op.time_started o) in
    Fmt.field "timings" Fmt.id @@
    Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
    [ pp_subfield "duration" Op.duration pp_span;
      pp_subfield "created" Op.time_created pp_span;
      pp_subfield "started" Op.time_started pp_span;
      pp_subfield "waited" wait pp_span; ]

  let pp_timing_created =
    Fmt.field "timings" Fmt.id @@
    Fmt.box @@ pp_subfield "created" Op.time_created Mtime.Span.pp

  let pp_timings ppf o =
    match Mtime.Span.equal (Op.time_started o) Mtime.Span.max_span with
    | true -> pp_timing_created ppf o
    | false -> pp_timings ppf o

  let maybe_failure_and pp ppf o = match Op.status o with
  | Failed f ->
      begin match f with
      | Op.Exec None -> ()
      | Op.Exec (Some e) ->
          Fmt.pf ppf "@[%a: @[%a@]@]@,"
            (Fmt.tty style_err) "error" Fmt.lines e;
      | Op.Missing_writes fs -> pp_did_not_write ppf fs; Fmt.cut ppf ()
      | Op.Missing_reads fs -> pp_cannot_read ppf fs; Fmt.cut ppf ()
      end;
      pp ppf o
  | _ -> pp ppf o

  let pp_copy_full =
    let pp_info =
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ pp_subfield "mode" Op.Copy.mode pp_file_mode;
        pp_subfield "linenum" Op.Copy.linenum Fmt.(option ~none:none int) ]
    in
    Fmt.record
      [ maybe_failure_and @@
        Fmt.field "src" Op.reads pp_reads;
        Fmt.field "dst" Op.writes pp_writes;
        Fmt.using Op.Copy.get pp_info;
        pp_timings; ]

  let pp_delete_full =
    let pp_path = Fmt.using Op.Delete.path pp_file_delete in
    Fmt.record
      [ maybe_failure_and @@ Fmt.using Op.Delete.get pp_path; pp_timings]

  let pp_mkdir_full =
    let pp_path = Fmt.using Op.Mkdir.dir pp_file_write in
    Fmt.record
      [ maybe_failure_and @@ Fmt.using Op.Mkdir.get pp_path; pp_timings ]

  let pp_notify_full =
    Fmt.record [ pp_notify; pp_timings]

  let pp_read_full =
    let pp_path = Fmt.using Op.Read.file pp_file_read in
    Fmt.record
      [ maybe_failure_and @@ Fmt.using Op.Read.get pp_path; pp_timings ]

  let pp_spawn_full =
    let stamped_vars s =
      let add_var acc ass = match String.cut_left ~sep:"=" ass with
      | None -> acc | Some (var, _) -> var :: acc
      in
      List.fold_left add_var [] (Op.Spawn.stamped_env s)
    in
    let pp_env = Fmt.(vbox @@ list string) in
    let pp_env = Fmt.using Op.Spawn.env pp_env in
    let pp_cwd = Fmt.using Op.Spawn.cwd Fpath.pp_quoted in
    let pp_success_exits =
      let pp_spawn_success_exits ppf = function
      | [] -> Fmt.string ppf "any"
      | cs -> Fmt.(list ~sep:comma int) ppf cs
      in
      Fmt.using Op.Spawn.success_exits pp_spawn_success_exits
    in
    let pp_stamps =
      let cmd_stamp s = Cmd.to_stamp (Op.Spawn.args s) in
      let pp_stamp ppf s = match Op.Spawn.stamp s with
      | "" -> ()
      | stamp -> pp_subfield "stamp" Op.Spawn.stamp Fmt.string ppf s
      in
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ pp_stamp;
        pp_subfield "env" stamped_vars Fmt.(list ~sep:sp Fmt.string);
        pp_subfield "args" cmd_stamp Fmt.(list ~sep:sp string) ]
    in
    let pp_spawn_base ppf o =
      pp_spawn_multi_line_and_exit ppf (Op.Spawn.get o); pp_op_extra ppf o;
    in
    let pp_spawn_stdio =
      let pp_spawn_stdo ppf = function
      | `Ui -> Fmt.pf ppf "<ui>"
      | `File f -> Fpath.pp_quoted ppf f
      | `Tee f -> Fmt.pf ppf "@[<hov><ui> and@ %a@]" Fpath.pp_quoted f
      in
      let pp_path_option = Fmt.(option ~none:none) Fpath.pp_quoted in
      Fmt.box @@ Fmt.concat ~sep:Fmt.sp @@
      [ pp_subfield "stdin" Op.Spawn.stdin pp_path_option;
        pp_subfield "stderr" Op.Spawn.stderr pp_spawn_stdo;
        pp_subfield "stdout" Op.Spawn.stdout pp_spawn_stdo; ]
    in
    let pp_writes ppf o = match Op.writes_manifest_root o with
    | None -> pp_writes ppf (Op.writes o)
    | Some root ->
        Fmt.pf ppf "@[<v>@[%a %a@]@,%a@]"
          pp_subfield_label "manifest root" pp_file root pp_writes (Op.writes o)
    in
    Fmt.record [
      Fmt.using Fmt.id pp_spawn_base;
      Fmt.field "writes" Fmt.id pp_writes;
      Fmt.field "reads" Op.reads pp_reads;
      Fmt.field "success-exits" Op.Spawn.get pp_success_exits;
      Fmt.field "stdio" Op.Spawn.get pp_spawn_stdio;
      Fmt.field "cwd" Op.Spawn.get pp_cwd;
      Fmt.field "env" Op.Spawn.get pp_env;
      Fmt.field "stamps" Op.Spawn.get pp_stamps;
      pp_timings ]

  let pp_wait_files_full = Fmt.record [Fmt.using Op.reads pp_reads; pp_timings]
  let pp_write_full =
    let pp_stamp = Fmt.using Op.Write.stamp Fmt.string in
    let pp_mode = Fmt.using Op.Write.mode pp_file_mode in
    let pp_write = Fmt.using Op.Write.file pp_file_write in
    Fmt.record
      [ maybe_failure_and @@ Fmt.using Op.Write.get pp_write;
        Fmt.field "reads" Op.reads pp_reads;
        Fmt.field "stamp" Op.Write.get pp_stamp;
        Fmt.field "mode" Op.Write.get pp_mode;
        pp_timings; ]

  let pp ppf o =
    let pp_kind_full ppf o = match Op.kind o with
    | Op.Copy _ -> pp_copy_full ppf o
    | Op.Delete _ -> pp_delete_full ppf o
    | Op.Mkdir _ -> pp_mkdir_full ppf o
    | Op.Notify _ -> pp_notify_full ppf o
    | Op.Read _ -> pp_read_full ppf o
    | Op.Spawn _ -> pp_spawn_full ppf o
    | Op.Wait_files _ -> pp_wait_files_full ppf o
    | Op.Write _ -> pp_write_full ppf o
    in
    Fmt.pf ppf "@[<v>@[<h>%a@]@, @[%a@]@,@]" pp_header o pp_kind_full o

  (* Binary serialization *)

  let enc_failure b = function
  | Op.Exec msg ->
      B0_bincode.enc_byte b 0; B0_bincode.(enc_option enc_string) b msg
  | Op.Missing_writes fs ->
      B0_bincode.enc_byte b 1; B0_bincode.(enc_list enc_fpath) b fs
  | Op.Missing_reads fs ->
      B0_bincode.enc_byte b 2; B0_bincode.(enc_list enc_fpath) b fs

  let dec_failure s i =
    let kind = "Op.failure" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 ->
        let i, msg = (B0_bincode.dec_option B0_bincode.dec_string) s next in
        i, Op.Exec msg
    | 1 ->
        let i, fs = B0_bincode.dec_list B0_bincode.dec_fpath s next in
        i, Op.Missing_writes fs
    | 2 ->
        let i, fs = B0_bincode.dec_list B0_bincode.dec_fpath s next in
        i, Op.Missing_reads fs
    | b -> B0_bincode.err_byte ~kind i b

  let enc_status b = function
  | Op.Aborted -> B0_bincode.enc_byte b 0
  | Op.Success -> B0_bincode.enc_byte b 1
  | Op.Failed f -> B0_bincode.enc_byte b 2; enc_failure b f
  | Op.Waiting -> B0_bincode.enc_byte b 3

  let dec_status s i =
    let kind = "Op.status" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> next, Op.Aborted
    | 1 -> next, Op.Success
    | 2 -> let i, f = dec_failure s next in i, Op.Failed f
    | 3 -> next, Op.Waiting
    | b -> B0_bincode.err_byte ~kind i b

  let enc_copy b c =
    B0_bincode.enc_fpath b (Op.Copy.src c);
    B0_bincode.enc_fpath b (Op.Copy.dst c);
    B0_bincode.enc_int b (Op.Copy.mode c);
    B0_bincode.enc_option B0_bincode.enc_int b (Op.Copy.linenum c)

  let dec_copy s i =
    let i, src = B0_bincode.dec_fpath s i in
    let i, dst = B0_bincode.dec_fpath s i in
    let i, mode = B0_bincode.dec_int s i in
    let i, linenum = B0_bincode.dec_option B0_bincode.dec_int s i in
    i, Op.Copy.make ~src ~dst ~mode ~linenum

  let enc_delete b d = B0_bincode.enc_fpath b (Op.Delete.path d)
  let dec_delete s i =
    let i, path = B0_bincode.dec_fpath s i in
    i, Op.Delete.make ~path

  let enc_mkdir b m =
    B0_bincode.enc_fpath b (Op.Mkdir.dir m);
    B0_bincode.enc_int b (Op.Mkdir.mode m)

  let dec_mkdir s i =
    let i, dir = B0_bincode.dec_fpath s i in
    let i, mode = B0_bincode.dec_int s i in
    i, Op.Mkdir.make ~mode ~dir

  let enc_notify_kind b = function
  | `End -> B0_bincode.enc_byte b 0
  | `Fail -> B0_bincode.enc_byte b 1
  | `Info -> B0_bincode.enc_byte b 2
  | `Start -> B0_bincode.enc_byte b 3
  | `Warn -> B0_bincode.enc_byte b 4

  let dec_notify_kind s i =
    let kind = "Op.Notify.kind" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> next, `End
    | 1 -> next, `Fail
    | 2 -> next, `Info
    | 3 -> next, `Start
    | 4 -> next, `Warn
    | b -> B0_bincode.err_byte ~kind i b

  let enc_notify b n =
    enc_notify_kind b (Op.Notify.kind n);
    B0_bincode.enc_string b (Op.Notify.msg n)

  let dec_notify s i =
    let i, kind = dec_notify_kind s i in
    let i, msg = B0_bincode.dec_string s i in
    i, Op.Notify.make ~kind ~msg

  let enc_read b r = B0_bincode.enc_fpath b (Op.Read.file r)
  let dec_read s i =
    let i, file = B0_bincode.dec_fpath s i in
    i, Op.Read.make ~file ~data:""

  let enc_spawn_stdo b = function
  | `Ui -> B0_bincode.enc_byte b 0
  | `File p -> B0_bincode.enc_byte b 1; B0_bincode.enc_fpath b p
  | `Tee p -> B0_bincode.enc_byte b 2; B0_bincode.enc_fpath b p

  let dec_spawn_stdo s i =
    let kind = "Op.spawn_stdo" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> next, `Ui
    | 1 -> let i, p = B0_bincode.dec_fpath s next in i, `File p
    | 2 -> let i, p = B0_bincode.dec_fpath s next in i, `Tee p
    | b -> B0_bincode.err_byte ~kind i b

  let enc_cmd b cmd =
    let arg b a = B0_bincode.enc_byte b 0; B0_bincode.enc_string b a in
    let unstamp b = B0_bincode.enc_byte b 1 in
    let append b = B0_bincode.enc_byte b 2 in
    let empty b = B0_bincode.enc_byte b 3 in
    Cmd.iter_enc ~arg ~unstamp ~append ~empty b cmd

  let rec dec_cmd s i =
    let kind = "Cmd.t" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> let i, s = B0_bincode.dec_string s next in i, Cmd.arg s
    | 1 -> let i, cmd = dec_cmd s next in i, Cmd.unstamp cmd
    | 2 ->
        let i, cmd0 = dec_cmd s next in
        let i, cmd1 = dec_cmd s i in
        i, Cmd.append cmd1 cmd0
    | 3 -> next, Cmd.empty
    | b -> B0_bincode.err_byte ~kind i b

  let enc_os_cmd_status b = function
  | `Exited c -> B0_bincode.enc_byte b 0; B0_bincode.enc_int b c
  | `Signaled c -> B0_bincode.enc_byte b 1; B0_bincode.enc_int b c

  let dec_os_cmd_status s i =
    let kind = "Os.Cmd.status" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = B0_bincode.dec_int s next in i, `Exited c
    | 1 -> let i, c = B0_bincode.dec_int s next in i, `Signaled c
    | b -> B0_bincode.err_byte ~kind i b

  let enc_spawn b s =
    B0_bincode.enc_list B0_bincode.enc_string b (Op.Spawn.env s);
    B0_bincode.enc_list B0_bincode.enc_string b (Op.Spawn.stamped_env s);
    B0_bincode.enc_fpath b (Op.Spawn.cwd s);
    B0_bincode.enc_option B0_bincode.enc_fpath b (Op.Spawn.stdin s);
    enc_spawn_stdo b (Op.Spawn.stdout s);
    enc_spawn_stdo b (Op.Spawn.stderr s);
    B0_bincode.enc_list B0_bincode.enc_int b (Op.Spawn.success_exits s);
    B0_bincode.enc_fpath b (Op.Spawn.tool s);
    enc_cmd b (Op.Spawn.args s);
    B0_bincode.enc_string b (Op.Spawn.stamp s);
    B0_bincode.enc_option
      (B0_bincode.enc_result ~ok:B0_bincode.enc_string ~error:B0_bincode.enc_string)
      b (Op.Spawn.stdo_ui s);
    B0_bincode.enc_option enc_os_cmd_status b (Op.Spawn.exit s)

  let dec_spawn s i =
    let i, env = B0_bincode.dec_list B0_bincode.dec_string s i in
    let i, stamped_env = B0_bincode.dec_list B0_bincode.dec_string s i in
    let i, cwd = B0_bincode.dec_fpath s i in
    let i, stdin = B0_bincode.dec_option B0_bincode.dec_fpath s i in
    let i, stdout = dec_spawn_stdo s i in
    let i, stderr = dec_spawn_stdo s i in
    let i, success_exits = B0_bincode.dec_list B0_bincode.dec_int s i in
    let i, tool = B0_bincode.dec_fpath s i in
    let i, args = dec_cmd s i in
    let i, stamp = B0_bincode.dec_string s i in
    let i, stdo_ui =
      let ok = B0_bincode.dec_string and error = B0_bincode.dec_string in
      B0_bincode.dec_option (B0_bincode.dec_result ~ok ~error) s i
    in
    let i, exit = B0_bincode.dec_option dec_os_cmd_status s i in
    i, Op.Spawn.make ~env ~stamped_env ~cwd ~stdin ~stdout ~stderr
      ~success_exits tool args ~stamp ~stdo_ui ~exit

  let enc_wait_files b wait = B0_bincode.enc_unit b ()
  let dec_wait_files s i =
    let i, () = B0_bincode.dec_unit s i in
    i, Op.Wait_files.make ()

  let enc_write b w =
    B0_bincode.enc_string b (Op.Write.stamp w);
    B0_bincode.enc_int b (Op.Write.mode w);
    B0_bincode.enc_fpath b (Op.Write.file w)

  let dec_write s i =
    let i, stamp = B0_bincode.dec_string s i in
    let i, mode = B0_bincode.dec_int s i in
    let i, file = B0_bincode.dec_fpath s i in
    let data () = Error "deserialized op, data fun not available" in
    i, Op.Write.make ~stamp ~mode ~file ~data

  let enc_kind b = function
  | Op.Copy c -> B0_bincode.enc_byte b 0; enc_copy b c
  | Op.Delete d -> B0_bincode.enc_byte b 1; enc_delete b d
  | Op.Mkdir m -> B0_bincode.enc_byte b 2; enc_mkdir b m
  | Op.Notify n -> B0_bincode.enc_byte b 3; enc_notify b n
  | Op.Read r -> B0_bincode.enc_byte b 4; enc_read b r
  | Op.Spawn s -> B0_bincode.enc_byte b 5; enc_spawn b s
  | Op.Wait_files w -> B0_bincode.enc_byte b 6; enc_wait_files b w
  | Op.Write w -> B0_bincode.enc_byte b 7; enc_write b w

  let dec_kind s i =
    let kind = "Op.kind" in
    let next, b = B0_bincode.dec_byte ~kind s i in
    match b with
    | 0 -> let i, c = dec_copy s next in i, Op.Copy c
    | 1 -> let i, d = dec_delete s next in i, Op.Delete d
    | 2 -> let i, m = dec_mkdir s next in i, Op.Mkdir m
    | 3 -> let i, n = dec_notify s next in i, Op.Notify n
    | 4 -> let i, r = dec_read s next in i, Op.Read r
    | 5 -> let i, s = dec_spawn s next in i, Op.Spawn s
    | 6 -> let i, w = dec_wait_files s next in i, Op.Wait_files w
    | 7 -> let i, w = dec_write s next in i, Op.Write w
    | b -> B0_bincode.err_byte ~kind i b

  let enc b o =
    B0_bincode.enc_int b (Op.id o);
    B0_bincode.enc_string b (Op.mark o);
    B0_bincode.enc_mtime_span b (Op.time_created o);
    B0_bincode.enc_mtime_span b (Op.time_started o);
    B0_bincode.enc_mtime_span b (Op.duration o);
    B0_bincode.enc_bool b (Op.revived o);
    enc_status b (Op.status o);
    B0_bincode.enc_list B0_bincode.enc_fpath b (Op.reads o);
    B0_bincode.enc_list B0_bincode.enc_fpath b (Op.writes o);
    B0_bincode.enc_option B0_bincode.enc_fpath b (Op.writes_manifest_root o);
    B0_bincode.enc_hash b (Op.hash o);
    enc_kind b (Op.kind o);
    ()

  let dec s i =
    let k o = invalid_arg "deserialized op, no kontinuation" in
    let i, id = B0_bincode.dec_int s i in
    let i, mark = B0_bincode.dec_string s i in
    let i, time_created = B0_bincode.dec_mtime_span s i in
    let i, time_started = B0_bincode.dec_mtime_span s i in
    let i, duration = B0_bincode.dec_mtime_span s i in
    let i, revived = B0_bincode.dec_bool s i in
    let i, status = dec_status s i in
    let i, reads = B0_bincode.dec_list B0_bincode.dec_fpath s i in
    let i, writes = B0_bincode.dec_list B0_bincode.dec_fpath s i in
    let i, writes_manifest_root =
      (B0_bincode.dec_option B0_bincode.dec_fpath) s i in
    let i, hash = B0_bincode.dec_hash s i in
    let i, kind = dec_kind s i in
    i, Op.make id ~mark ~time_created ~time_started ~duration ~revived
      ~status ~reads ~writes ~writes_manifest_root ~hash ~k kind

  let bincode = B0_bincode.make enc dec

  (* Aggregate errors *)

  let howto_file howto = Fmt.(tty' style_op_howto howto ++ pp_file_write)
  let writes_cycle os =
    let deps prev next =
      let prev_writes = Fpath.Set.of_list (B0_zero.Op.writes prev) in
      let next_reads = Fpath.Set.of_list (B0_zero.Op.reads next) in
      Fpath.Set.inter prev_writes next_reads
    in
    match os with
    | [] -> []
    | [o] -> [deps o o]
    | first :: _ ->
        let rec loop first acc = function
        | prev :: (next :: _ as os) -> loop first (deps prev next :: acc) os
        | prev :: [] -> List.rev (deps prev first :: acc)
        | [] -> assert false
        in
        loop first [] os

  let pp_failed ppf () = Fmt.(tty style_err) ppf "FAILED"
  let pp_ops_cycle ?(write_howto = Fmt.any "") ppf os =
    let pp_self_cycle ~write_howto ppf writes =
      let these_file, them = match Fpath.Set.cardinal writes with
      | 1 -> "This file is", "it"
      | _ -> "These files are", "them"
      in
      Fmt.pf ppf
        "%s read and written by the same operation:@,\
        \ @[<v>%a@,See the operation writing %s for details.@]@]"
        these_file (Fpath.Set.pp (howto_file write_howto)) writes them
    in
    let pp_cycle ~write_howto ppf ws =
      Fmt.pf ppf
        "Operations writing these files form \
         a cycle:@, @[<v>%a@,\
         The last written file is read by the operation writing the first \
         one.@,\
         See operations writing them for details.@]"
        (Fmt.list (howto_file write_howto)) ws
    in
    let pp_ops ppf os =
      let writes = writes_cycle os in
      match os with
      | [] -> assert false
      | [o] -> pp_self_cycle ~write_howto ppf (List.hd writes)
      | os ->
          let writes = try List.(rev @@ rev_map Fpath.Set.choose writes) with
          | Not_found -> assert false
          in
          pp_cycle ~write_howto ppf writes
    in
    Fmt.pf ppf "@[<v>[%a] %a@]" pp_failed () pp_ops os

  let pp_never_ready ?(read_howto = Fmt.any "") ppf fs =
    let err = match Fpath.Set.cardinal fs with
    | 1 -> "This file never became ready"
    | _ -> "These files never became ready"
    in
    Fmt.pf ppf "@[<v>[%a] %s:@,\
               \ @[<v>%a@,See operations reading them for details.@]@]"
      pp_failed () err (Fpath.Set.pp (howto_file read_howto)) fs

  let pp_aggregate_error ?(sep = Fmt.flush_nl) ?read_howto ?write_howto () ppf =
    function
    | B0_zero.Op.Failures -> ()
    | B0_zero.Op.Cycle ops -> pp_ops_cycle ?write_howto ppf ops; sep ppf ()
    | B0_zero.Op.Never_became_ready fs ->
        pp_never_ready ?read_howto ppf fs; sep ppf ()
end
