(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* This is only data structures, printers and predicates, no execution or
   caching logic occurs here. *)

open B0_result

let invalid_result = Error (`Msg "operation not submitted")

(* Common pretty printers. *)

let dump_error_msg ppf m = B0_fmt.pf ppf "@[error: %s@]" m
let dump_unit_result ppf = function
| Error (`Msg m) -> dump_error_msg ppf m
| Ok () -> B0_fmt.pf ppf "ok"

let dump_file_contents ppf d =
  let last = 40 in
  match B0_string.length d - 1 with
  | max_idx when max_idx <= last -> B0_fmt.pf ppf "%S" d
  | max_idx -> B0_fmt.pf ppf "%S" (B0_string.with_index_range ~last d ^ "...")

let pp_cmd ppf cmd =
  let bold_green = [`Faint; `Fg `Green;] in
  let pp_brack = B0_tty.pp_str [`Fg `Yellow] in
  let pp_exe ppf e = B0_tty.pp_str [`Fg `Red; `Bold] ppf (Filename.quote e) in
  let pp_arg ppf a = B0_fmt.pf ppf "%s" (Filename.quote a) in
  let pp_o_arg ppf a = B0_tty.pp_str bold_green ppf (Filename.quote a) in
  let rec pp_args last_was_o ppf = function
  | [] -> ()
  | a :: args ->
      if args <> [] then B0_fmt.char ppf ' ';
      if last_was_o then pp_o_arg ppf a else pp_arg ppf a;
      pp_args (B0_string.equal a "-o") ppf args
  in
  let exec, args = match B0_cmd.to_list cmd with
  | exec :: args -> exec, args | _ -> assert false
  in
  B0_fmt.pf ppf "@[<h>";
  pp_brack ppf "["; pp_exe ppf exec; pp_args false ppf args; pp_brack ppf "]";
  B0_fmt.pf ppf "@]"

(* Operation unique ids *)

type id = int
let uid =
  let id = ref (-1) in
  fun () -> incr id; !id

(* Operation constructors and accessors.  *)

type spawn_stdo = [ `Ui | `File of B0_fpath.t | `Tee of B0_fpath.t ]
type spawn_stdo_ui =
  [ `Tmp_file of B0_fpath.t | `Stdo of string result | `None ]

type spawn_pid = int
type spawn_env = string array
type spawn_success_codes = int list option
type spawn =
  { cmd : B0_cmd.t;
    env : spawn_env;
    cwd : B0_fpath.t;
    stdin : B0_fpath.t option;
    stdout : spawn_stdo;
    stderr : spawn_stdo;
    success : spawn_success_codes;
    mutable stdo_ui : spawn_stdo_ui;
    mutable spawn_result : (spawn_pid * B0_os.Cmd.status) result; }

type read =
  { r_file : B0_fpath.t;
    mutable r_result : string result }

type write =
  { w_file : B0_fpath.t;
    mutable w_data : string;
    mutable w_result : unit result }

type copy_file =
  { c_src : B0_fpath.t;
    c_dst : B0_fpath.t;
    c_linenum : int option;
    mutable c_result : unit result }

type delete =
  { d_file : B0_fpath.t;
    mutable d_result : unit result }

type mkdir =
  { m_dir : B0_fpath.t;
    mutable m_result : unit result }

type sync =
  { s_units : B0_unit.Idset.t; }

type kind =
| Spawn of spawn
| Read of read
| Write of write
| Copy_file of copy_file
| Delete of delete
| Mkdir of mkdir
| Sync of sync

let kind_to_string = function
| Spawn _ -> "spawn" | Read _ -> "read" | Write _ -> "write"
| Copy_file _ -> "copy" | Delete _ -> "delete" | Mkdir _ -> "mkdir"
| Sync _ -> "sync"

type status =
| Guarded
| Ready
| Executed
| Finished

let status_to_string = function
| Guarded -> "guarded" | Ready -> "ready"  | Executed -> "executed"
| Finished -> "finished"

type t =
  { id : id;
    unit_id : B0_unit.id;
    aim : B0_conf.build_aim;
    mutable reads : B0_fpath.set;
    mutable writes : B0_fpath.set;
    kind : kind;
    creation_time : B0_time.span;
    mutable exec_start_time : B0_time.span;
    mutable exec_end_time : B0_time.span;
    mutable cached : bool;
    mutable status : status;
    mutable stamp : B0_stamp.t; }

let v
    unit aim creation_time ?(reads = B0_fpath.Set.empty)
    ?(writes = B0_fpath.Set.empty) kind
  =
  let id = uid () in
  let unit_id = B0_unit.id unit in
  let exec_start_time = B0_time.zero in
  let exec_end_time = B0_time.zero in
  { id; unit_id; aim; reads; writes; kind; creation_time; exec_start_time;
    exec_end_time; cached = false; status = Guarded;
    stamp = B0_stamp.zero  }

let id o = o.id
let unit_id o = o.unit_id
let aim o = o.aim
let reads o = o.reads
let set_reads o rs = o.reads <- rs
let writes o = o.writes
let set_writes o ws = o.writes <- ws
let creation_time o = o.creation_time
let exec_start_time o = o.exec_start_time
let set_exec_start_time o t = o.exec_start_time <- t
let exec_end_time o = o.exec_end_time
let set_exec_end_time o t = o.exec_end_time <- t
let status o = o.status
let set_status o s = o.status <- s
let stamp o = o.stamp
let set_stamp o s = o.stamp <- s
let cached o = o.cached
let set_cached o c = o.cached <- c

let pp_status ppf st = B0_fmt.string ppf (status_to_string st)

let dump_op ppf o =
  B0_fmt.pf ppf
    "id: %d@,unit id: %d@,reads: @[%a@]@,writes: @[%a@]\
     @, creation time: %a\
     @,    exec start: %a\
     @,      exec end: %a@,status: %a@,cached: %b stamp: %a"
    o.id o.unit_id B0_fpath.Set.dump o.reads
    B0_fpath.Set.dump o.writes
    B0_time.pp_span_uint_ns o.creation_time
    B0_time.pp_span_uint_ns o.exec_start_time
    B0_time.pp_span_uint_ns o.exec_end_time
    pp_status o.status
    o.cached B0_stamp.pp o.stamp

(* Spawn *)

let spawn
    u aim time ~reads ~writes ~success ~stdin ~stdout ~stderr ~cwd env cmd
  =
  let spawn_result = invalid_result in
  let stdo_ui = `None in
  let s =
    { cmd; env; cwd; stdin; stderr; stdout; stdo_ui; success; spawn_result }
  in
  v u aim time ~reads ~writes (Spawn s)

let spawn_cmd s = s.cmd
let spawn_env s = s.env
let spawn_cwd s = s.cwd
let spawn_stdin s = s.stdin
let spawn_stdout s = s.stdout
let spawn_stderr s = s.stderr
let spawn_success_codes s = s.success
let spawn_stdo_ui s = s.stdo_ui
let set_spawn_stdo_ui s ui = s.stdo_ui <- ui
let spawn_result s = s.spawn_result
let set_spawn_result s r = s.spawn_result <- r

let dump_spawn ppf s =
  let dump_spawn_stdo ppf = function
  | `Ui -> B0_fmt.pf ppf "<ui>"
  | `File f -> B0_fpath.dump ppf f
  | `Tee f -> B0_fmt.pf ppf "@[<hov><ui> and@ %a@]" B0_fpath.dump f
  in
  let dump_opt_path = B0_fmt.(option ~none:none_str) B0_fpath.dump in
  let dump_stdo_ui ppf = function
  | `Tmp_file f -> B0_fpath.dump ppf f
  | `Stdo (Ok d) -> dump_file_contents ppf d
  | `Stdo (Error (`Msg e)) -> B0_fmt.pf ppf "error: %s" e
  | `None -> B0_fmt.none_str ppf ()
  in
  let dump_spawn_success_codes ppf = function
  | None -> B0_fmt.string ppf "0"
  | Some [] -> B0_fmt.string ppf "any"
  | Some cs -> B0_fmt.(list ~sep:comma int) ppf cs
  in
  let dump_spawn_result ppf = function
  | Error (`Msg m) -> dump_error_msg ppf m
  | Ok (pid, st) -> B0_fmt.pf ppf "@[pid:%d %a@]" pid B0_os.Cmd.pp_status st
  in
  let dump_env = B0_fmt.(vbox @@ array string) in
  B0_fmt.field "cmd" pp_cmd ppf s.cmd; B0_fmt.cut ppf ();
  B0_fmt.field "env" dump_env ppf s.env; B0_fmt.cut ppf ();
  B0_fmt.field "cwd" B0_fpath.dump ppf s.cwd; B0_fmt.cut ppf ();
  B0_fmt.field "stdin" dump_opt_path ppf s.stdin; B0_fmt.cut ppf ();
  B0_fmt.field "stdout" dump_spawn_stdo ppf s.stdout; B0_fmt.cut ppf ();
  B0_fmt.field "stderr" dump_spawn_stdo ppf s.stderr; B0_fmt.cut ppf ();
  B0_fmt.field "stdo ui" dump_stdo_ui ppf s.stdo_ui; B0_fmt.cut ppf ();
  B0_fmt.field "success codes" dump_spawn_success_codes ppf s.success;
  B0_fmt.cut ppf ();
  B0_fmt.field "result" dump_spawn_result ppf s.spawn_result

(* Read *)

let read u aim time r_file =
  let reads = B0_fpath.Set.singleton r_file in
  let r = { r_file; r_result = invalid_result } in
  v u aim time ~reads (Read r)

let read_file r = r.r_file
let read_result r = r.r_result
let get_read_data r = match r.r_result with
| Error (`Msg e) -> invalid_arg ("read failed with: " ^ e)
| Ok d -> d

let set_read_result s r = s.r_result <- r
let dump_read ppf r =
  let dump_read_result ppf = function
  | Error (`Msg m) -> dump_error_msg ppf m
  | Ok d -> dump_file_contents ppf d
  in
  B0_fmt.field "file" B0_fpath.dump ppf r.r_file; B0_fmt.cut ppf ();
  B0_fmt.field "result" dump_read_result ppf r.r_result

(* Writes *)

let write u aim time ~reads w_file =
  let writes = B0_fpath.Set.singleton w_file in
  let w = { w_file; w_data = ""; w_result = invalid_result } in
  v u aim time  ~reads ~writes (Write w)

let write_file w = w.w_file
let write_data w = w.w_data
let set_write_data w d = w.w_data <- d
let write_result w = w.w_result
let set_write_result s r = s.w_result <- r
let dump_write ppf w =
  B0_fmt.field "file" B0_fpath.dump ppf w.w_file; B0_fmt.cut ppf ();
  B0_fmt.field "data" dump_file_contents ppf w.w_data; B0_fmt.cut ppf ();
  B0_fmt.field "result" dump_unit_result ppf w.w_result

(* Copy file *)

let copy_file ?linenum:c_linenum u aim time c_src c_dst =
  let reads = B0_fpath.Set.singleton c_src in
  let writes = B0_fpath.Set.singleton c_dst in
  let c = { c_src; c_dst; c_linenum; c_result = invalid_result } in
  v u aim time ~reads ~writes (Copy_file c)

let copy_file_src c = c.c_src
let copy_file_dst c = c.c_dst
let copy_file_linenum c = c.c_linenum
let copy_file_result c = c.c_result
let set_copy_file_result c r = c.c_result <- r

let dump_copy_file_linenum ppf = function
| None -> B0_fmt.nop ppf ()
| Some l -> B0_fmt.pf ppf "linenum: %d@," l

let dump_copy_file ppf c =
  B0_fmt.field "src" B0_fpath.dump ppf c.c_src; B0_fmt.cut ppf ();
  B0_fmt.field "dst" B0_fpath.dump ppf c.c_dst; B0_fmt.cut ppf ();
  B0_fmt.field "linenum" dump_copy_file_linenum ppf c.c_linenum;
  B0_fmt.cut ppf ();
  B0_fmt.field "result" dump_unit_result ppf c.c_result

(* Delete *)

let delete u aim time d_file =
  let reads = B0_fpath.Set.singleton d_file in
  let d = { d_file; d_result = invalid_result } in
  v u aim time ~reads (Delete d)

let delete_file d = d.d_file
let delete_result d = d.d_result
let set_delete_result s r = s.d_result <- r
let dump_delete ppf d =
  B0_fmt.field "file" B0_fpath.dump ppf d.d_file; B0_fmt.cut ppf ();
  B0_fmt.field "result:" dump_unit_result ppf d.d_result

(* Mkdir *)

let mkdir u aim time m_dir =
  let d = { m_dir; m_result = invalid_result } in
  v u aim time (Mkdir d)

let mkdir_dir m = m.m_dir
let mkdir_result m = m.m_result
let set_mkdir_result m r = m.m_result <- r
let dump_mkdir ppf m =
  B0_fmt.field "dir" B0_fpath.dump ppf m.m_dir; B0_fmt.cut ppf ();
  B0_fmt.field "result" dump_unit_result ppf m.m_result

(* Sync *)

let sync u aim time s_units = v u aim time (Sync { s_units })
let sync_units s = s.s_units
let dump_sync ppf s =
  B0_fmt.field "unit ids:" (B0_unit.Idset.pp ~sep:B0_fmt.comma B0_fmt.int)
    ppf s.s_units

(* Kind *)

let kind o = o.kind
let invalid k = raise (Invalid_argument ("Not a " ^ k))
let get_spawn o = match kind o with Spawn s -> s | _ -> invalid "spawn"
let get_read o = match kind o with Read r -> r | _ -> invalid "read"
let get_write o = match kind o with Write w -> w | _ -> invalid "write"
let get_copy_file o =
  match kind o with Copy_file w -> w | _ -> invalid "copy file"

let get_delete o = match kind o with Delete d -> d | _ -> invalid "delete"
let get_mkdir o = match kind o with Mkdir m -> m | _ -> invalid "mkdir"
let get_sync o = match kind o with Sync s -> s | _ -> invalid "sync"

let dump_kind ppf = function
| Spawn s -> dump_spawn ppf s
| Read r -> dump_read ppf r
| Write w -> dump_write ppf w
| Copy_file c -> dump_copy_file ppf c
| Delete d -> dump_delete ppf d
| Mkdir m -> dump_mkdir ppf m
| Sync s -> dump_sync ppf s

(* Predicates *)

let cycle o0 o1 =
  let o0_needs_o1 = B0_fpath.Set.inter (reads o0) (writes o1) in
  let o1_needs_o0 = B0_fpath.Set.inter (reads o1) (writes o0) in
  try Some (B0_fpath.Set.choose o0_needs_o1, B0_fpath.Set.choose o1_needs_o0)
  with Not_found -> None

let equal o0 o1 = (( = ) : int -> int -> bool) o0.id o1.id
let compare o0 o1 = (compare : int -> int -> int) o0.id o1.id
let compare_exec_start_time o0 o1 =
  B0_time.compare_span o0.exec_start_time o1.exec_start_time

(* Pretty-printing *)

let dump ppf o =
  B0_fmt.pf ppf "@[<v>%a@,%a@]" dump_op o dump_kind o.kind

let pp = dump

let pp_spawn_fail ppf o =
  (* FIXME I think we mostly only want writes and
     the cmd and output a b0 command that allows to inspect
     the build op. *)
  let pp_exp ppf s = match s.success with
  | None -> ()
  | Some [c] -> B0_fmt.pf ppf " expected %d" c
  | Some cs ->
      B0_fmt.pf ppf " expected one of @[%a@]"
        B0_fmt.(list ~sep:comma int) cs
  in
  let pp_stdo_ui ppf s = match s.stdo_ui with
  | `None -> ()
  | `Stdo (Error (`Msg e)) -> B0_fmt.string ppf e
  | `Stdo (Ok ui) ->
      (* FIXME unesc if needed *)
      B0_fmt.lines ppf (B0_string.trim ui)
  | `Tmp_file _ -> assert false
  in
  let pp_signaled ppf c = B0_fmt.pf ppf "signaled with %d" c in
  let pp_exited ppf c = B0_fmt.pf ppf "exited with %d" c in
  let pp_status ppf s = match s.spawn_result with
  | Ok (_, `Signaled c) -> B0_tty.pp [`Fg `Red] pp_signaled ppf c
  | Ok (_, `Exited c) ->
      B0_fmt.pf ppf "%a%a" (B0_tty.pp [`Fg `Red] pp_exited) c pp_exp s
  | Error (`Msg m) ->
      B0_fmt.pf ppf "%s" m
  in
  let pp_writes ppf s =
    B0_fpath.Set.pp (B0_tty.pp [`Fg `Cyan] B0_fpath.dump) ppf s
  in
  match o.kind with
  | Spawn s ->
      B0_fmt.pf ppf
        "@[<v>reads: @[%a@]@,writes: @[%a@]@,cwd: %a@,\
         env: @[%a@]@,@,%a %a:@,@,%a@]"
        B0_fpath.Set.dump o.reads pp_writes o.writes
        B0_fpath.dump s.cwd B0_fmt.(array string) s.env pp_cmd s.cmd
        pp_status s pp_stdo_ui s
  | _ -> assert false

(* FIXME enhance this
   We need a id -> name map here
   Path to build dir should be factored out as $B *)

let op_kind ppf k = B0_tty.pp_str [`Fg `Green] ppf k
let op_cached ppf o = match cached o with
| false -> B0_fmt.nop ppf ()
| true -> B0_fmt.pf ppf "[%a]" (B0_tty.pp_str [`Fg `Green]) "CACHED"

let op_syn ppf o =
  B0_fmt.pf ppf "@[%a[%a:%d]@]" op_cached o op_kind (kind_to_string o.kind) o.id

let pp_kind_log_line ppf o = match kind o with
| Spawn s ->  pp_cmd ppf s.cmd
| Read r -> B0_fpath.pp ppf r.r_file
| Write w ->  B0_fpath.pp ppf w.w_file
| Copy_file c ->B0_fmt.pf ppf "%a to %a" B0_fpath.pp c.c_src B0_fpath.pp c.c_dst
| Delete d -> B0_fpath.pp ppf d.d_file
| Mkdir m -> B0_fpath.pp ppf m.m_dir
| Sync s ->
    B0_fmt.pf ppf "units %a" B0_unit.Idset.(pp ~sep:B0_fmt.comma B0_fmt.int)
      s.s_units

let pp_log_line ppf o =
  B0_fmt.pf ppf "@[<h>%a %a@]" op_syn o pp_kind_log_line o

let pp_long ppf o =
  let pp_span ppf s =
    B0_fmt.pf ppf "%a (%ans)" B0_time.pp_span s B0_time.pp_span_uint_ns s
  in
  let pp_op ppf o =
    let duration = B0_time.abs_diff o.exec_end_time o.exec_start_time in
    B0_fmt.field "writes" B0_fpath.Set.dump ppf o.writes; B0_fmt.cut ppf ();
    B0_fmt.field "reads"B0_fpath.Set.dump ppf o.reads; B0_fmt.cut ppf ();
    B0_fmt.field "created" pp_span ppf o.creation_time; B0_fmt.cut ppf ();
    B0_fmt.field "start" pp_span ppf o.exec_start_time; B0_fmt.cut ppf ();
    B0_fmt.field "duration" pp_span ppf duration; B0_fmt.cut ppf ();
    B0_fmt.field "stamp" B0_stamp.pp ppf o.stamp; B0_fmt.cut ppf ();
    B0_fmt.field "kind" B0_fmt.string ppf (kind_to_string o.kind)
  in
  B0_fmt.pf ppf "@[<v>%a@, @[<v>%a@]@, @[<v>%a@]@]"
    op_syn o pp_op o dump_kind o.kind

(* Sets and maps  *)

module Op = struct
  type nonrec t = t
  let compare = compare
end

module Set = Set.Make (Op)
module Map = struct
  include Map.Make (Op)
  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  let of_list us = List.fold_left (fun m (k,v) -> add k v m) empty us
end

type set = Set.t
type 'a map = 'a Map.t

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
