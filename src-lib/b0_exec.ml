(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* File operations are synchronous in this implementation. But the API
   allows for asynchronous IO or thread pool implementations. *)

open B0_result

(* Operation handlers *)

type handler =
  { tmp_path : B0_fpath.t;
    dur_counter : B0_time.counter;
    todo : B0_op.t B0_rqueue.t; (* Operations waiting for OS submission *)
    collectable : B0_op.t Queue.t; (* Collectable operations *)
    to_spawn : (* Spawn ops dequeued from [todo] waiting to spawn *)
      B0_op.t Queue.t;
    max_spawn : int; (* Max number of spawned processes *)
    mutable spawn_count : int; (* Number of spawned processes *)
    mutable spawns : (* Spawned processes *)
      (B0_os.Cmd.spawn_pid * B0_op.t) list;  }

let handler
    ?(rand = Random.State.make_self_init ()) ~max_spawn ~dur_counter ~tmp_path
    ()
  =
  let todo = B0_rqueue.empty ~rand () in
  let collectable = Queue.create () in
  let to_spawn = Queue.create () in
  { tmp_path; dur_counter; todo; collectable; to_spawn;
    max_spawn; spawn_count = 0; spawns = []; }

let all_done h = (* [true] iff nothing left todo in the handler *)
  B0_rqueue.length h.todo = 0 &&
  Queue.length h.to_spawn = 0 &&
  h.spawn_count = 0

let incr_spawn_count h = h.spawn_count <- h.spawn_count + 1
let decr_spawn_count h = h.spawn_count <- h.spawn_count - 1
let time_stamp h = B0_time.count h.dur_counter

let complete_op h o =
  B0_op.(set_status o Executed);
  B0_op.set_exec_end_time o (time_stamp h);
  Queue.add o h.collectable

let exec_op h o =
  B0_log.debug (fun m -> m ~header:"EXEC" "%a" B0_op.pp_log_line o);
  B0_op.set_exec_start_time o (time_stamp h)

let complete_spawn_stdo_ui o s =
  let get_ui_file s = match B0_op.spawn_stdo_ui s with
  | `Tmp_file f -> f | `Stdo _ | `None -> assert false
  in
  let append f0 f1 =
    B0_os.File.read f0 >>= fun f0 ->
    B0_os.File.read f1 >>= fun f1 -> Ok (B0_string.strf "%s\n%s" f0 f1)
  in
  let set ui = B0_op.set_spawn_stdo_ui s (`Stdo ui) in
  match B0_op.spawn_stdout s, B0_op.spawn_stderr s with
  | `Ui, `Ui -> set @@ B0_os.File.read (get_ui_file s)
  | `Ui, `File _ | `File _, `Ui -> set @@ B0_os.File.read (get_ui_file s)
  | `File _, `File _ -> ()
  | `Ui, `Tee f | `Tee f, `Ui -> set @@ append f (get_ui_file s)
  | `Tee f0, `Tee f1 -> set @@ append f0 f1
  | `Tee f, `File _ | `File _, `Tee f -> set @@ B0_os.File.read f

let complete_spawn h o result =
  let s = B0_op.get_spawn o in
  decr_spawn_count h;
  B0_op.set_spawn_result s result;
  complete_spawn_stdo_ui o s;
  complete_op h o

let get_stdo_ui_file h = match B0_os.File.open_tmp_path h.tmp_path with
| Ok (file, fd) -> `Fd (fd, true), `Tmp_file file
| Error (`Msg e) -> failwith e

let exec_spawn h o =
  let spawn s =
    try
      let cmd = B0_op.spawn_cmd s in
      let env = B0_op.spawn_env s in
      let cwd = B0_op.spawn_cwd s in
      let stdin = match B0_op.spawn_stdin s with
      | None -> `Fd (Unix.stdin, false)
      | Some f -> `File f
      in
      let stdout, ui = match B0_op.spawn_stdout s with
      | `File f | `Tee f -> `File f, `None
      | `Ui -> get_stdo_ui_file h
      in
      let stderr, ui = match B0_op.spawn_stderr s with
      | `File f | `Tee f -> `File f, ui
      | `Ui ->
          match ui with
          | `Tmp_file _ -> stdout, ui
          | `None -> get_stdo_ui_file h
      in
      B0_op.set_spawn_stdo_ui s ui;
      match B0_os.Cmd.spawn env ~cwd ~stdin ~stdout ~stderr cmd with
      | Ok pid -> h.spawns <- (pid, o) :: h.spawns
      | Error (`Msg e) -> failwith e
    with Failure e -> complete_spawn h o (Error (`Msg e))
  in
  incr_spawn_count h;
  exec_op h o;
  spawn (B0_op.get_spawn o)

let rec collect_spawns ~block h = match h.spawn_count = 0 with
| true -> ()
| false ->
    (* We don't collect with -1 or 0 because library-wise we might collect
       things we did not spawn. On Windows there wouldn't be the choice
       anyways. This means that on a blocking collection there's a bit
       of busy waiting involved. FIXME use sigchld + self-pipe trick ?
       would that work on windows ? We could also sleep a bit via select. *)
    let old_spawn_count = h.spawn_count in
    let collect spawns (pid, o as p) =
      match B0_os.Cmd.collect ~block:false pid with
      | Error _ as e -> complete_spawn h o e; spawns
      | Ok None -> p :: spawns
      | Ok (Some ret) -> complete_spawn h o (Ok ret); spawns
    in
    h.spawns <- List.fold_left collect [] h.spawns;
    match block && old_spawn_count = h.spawn_count with
    | true -> collect_spawns ~block h (* busy waiting *)
    | false -> ()

let exec_spawns h =
  let free = h.max_spawn - h.spawn_count in
  let may_spawn = Queue.length h.to_spawn in
  let to_spawn = if may_spawn > free then free else may_spawn in
  let rec loop = function
  | 0 -> ()
  | n ->
      match Queue.take h.to_spawn with
      | exception Queue.Empty -> ()
      | o ->
          exec_spawn h o;
          match B0_op.status o with
          | B0_op.Executed (* Error'ed *) -> loop n
          | _ -> loop (n - 1)
  in
  loop to_spawn

let exec_read h o r = (* synchronous *)
  exec_op h o;
  B0_op.set_read_result r (B0_os.File.read @@ B0_op.read_file r);
  complete_op h o

let exec_write h o w = (* synchronous *)
  let data = B0_op.write_data w in
  exec_op h o;
  B0_op.set_write_result w (B0_os.File.write (B0_op.write_file w) data);
  complete_op h o

let exec_copy_file h o c = (* synchronous *)
  exec_op h o;
  B0_op.set_copy_file_result c begin
    let src = B0_op.copy_file_src c in
    B0_os.File.read src >>= fun data ->
    let data = match B0_op.copy_file_linenum c with
    | None -> data
    | Some l -> (B0_string.strf "#line %d \"%a\"\n" l B0_fpath.pp src) ^ data
    in
    B0_os.File.write (B0_op.copy_file_dst c) data
  end;
  complete_op h o

let exec_delete h o d = (* synchronous *)
  exec_op h o;
  B0_op.set_delete_result d (B0_os.File.delete @@ B0_op.delete_file d);
  complete_op h o

let exec_mkdir h o m = (* synchronous *)
  let mkdir d = B0_os.Dir.create ~path:true d >>| fun _ -> () in
  exec_op h o;
  B0_op.set_mkdir_result m (mkdir @@ B0_op.mkdir_dir m);
  complete_op h o

let exec_sync h o _ =
  exec_op h o;
  complete_op h o

let submit h o = B0_rqueue.add h.todo o

let rec stir ~block h =
  (* If block is [true] and [all_done h] is [false]. [h.completed]
     must be non-empty when the function returns. N.B. as it stands
     this implementation does all the synchronous operations before
     returning. *)
  match B0_rqueue.take h.todo with
  | None ->
      begin match all_done h with
      | true -> ()
      | false -> exec_spawns h; collect_spawns ~block h;
      end
  | Some o ->
      collect_spawns ~block:false h;
      begin match B0_op.kind o with
      | B0_op.Spawn _ -> Queue.add o h.to_spawn
      | B0_op.Read r -> exec_read h o r
      | B0_op.Write w -> exec_write h o w
      | B0_op.Copy_file c -> exec_copy_file h o c
      | B0_op.Delete d -> exec_delete h o d
      | B0_op.Mkdir m -> exec_mkdir h o m
      | B0_op.Sync s -> exec_sync h o s
      end;
      exec_spawns h;
      stir ~block h

(* Collectable operations *)

let rec collect h ~block =
  stir ~block:false h; (* First stir a bit, it might schedule/collect ops. *)
  match Queue.take h.collectable with
  | op -> Some op
  | exception Queue.Empty ->
      match not block || all_done h with
      | true -> None
      | false -> stir ~block h; Some (Queue.take h.collectable)

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
