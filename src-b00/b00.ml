(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B000

module Env = struct
  type tool_lookup = Cmd.tool -> (Fpath.t, string) result

  let env_tool_lookup ?sep ?(var = "PATH") env =
    let search_path = match String.Map.find var env with
    | exception Not_found -> "" | s -> s
    in
    match Fpath.list_of_search_path ?sep search_path with
    | Error _ as e -> fun _ -> e
    | Ok search -> fun tool -> Os.Cmd.must_find_tool ~search tool

  type t =
    { env : Os.Env.t;
      forced_env : Os.Env.t;
      lookup : tool_lookup }

  let memo lookup =
    let memo = Hashtbl.create 91 in
    fun tool -> match Hashtbl.find memo tool with
    | exception Not_found -> let p = lookup tool in Hashtbl.add memo tool p; p
    | p -> p

  let v ?lookup ?(forced_env = String.Map.empty) env =
    let lookup = match lookup with None -> env_tool_lookup env | Some l -> l in
    let lookup = memo lookup in
    { env; forced_env; lookup }

  let env e = e.env
  let forced_env e = e.forced_env
  let tool e l = e.lookup l
end

module Tool = struct
  type env_vars = string list
  let tmp_vars = ["TMPDIR"; "TEMP"; "TMP"]

  type response_file =
    { to_file : Cmd.t -> string;
      cli : Fpath.t -> Cmd.t; }

  let response_file_of to_file cli = { to_file; cli }
  let args0 =
    let to_file cmd = String.concat "\x00" (Cmd.to_list cmd) ^ "\x00" in
    let cli f = Cmd.(arg "-args0" %% path f) in
    { to_file; cli }

  type t =
    { name : Fpath.t;
      vars : env_vars;
      shielded_vars : env_vars;
      response_file : response_file option; }

  let v ?response_file ?(shielded_vars = tmp_vars) ?(vars = []) name =
    { name; vars; shielded_vars; response_file }

  let by_name ?response_file ?shielded_vars ?vars name =
    match Fpath.is_seg name with
    | false -> Fmt.invalid_arg "%S: tool is not a path segment" name
    | true -> v ?shielded_vars ?vars (Fpath.v name)

  let name t = t.name
  let vars t = t.vars
  let shielded_vars t = t.shielded_vars
  let response_file t = t.response_file
  let read_env t env =
    let add_var acc var = match String.Map.find var env with
    | v -> String.Map.add var v acc
    | exception Not_found -> acc
    in
    let relevant = List.fold_left add_var String.Map.empty t.vars in
    let all = List.fold_left add_var relevant t.shielded_vars in
    all, relevant
end

module Futs : sig
  type t
  type 'a fut
  type 'a fut_set
  val create : ?rand:Random.State.t -> unit -> t
  val fut : t -> 'a fut * 'a fut_set
  val fut_value : 'a fut -> 'a option
  val fut_wait : 'a fut -> ('a -> unit) -> unit
  val fut_set : 'a fut_set -> 'a -> unit
  val stir : t -> (unit -> unit) option
end = struct
  module Fut_id = struct
    type t = int
    let compare : int -> int -> int = compare
  end
  module Fmap = Map.Make (Fut_id)

  type 'a fut =
    { id : Fut_id.t;
      mutable value : 'a option;
      mutable konts : ('a -> unit) list;
      futs : t; }
  and 'a fut_set = 'a fut
  and e_fut = F : 'a fut -> e_fut
  and t =
    { mutable next : Fut_id.t;
      mutable waiting : e_fut Fmap.t;
      mutable kready : (unit -> unit) Rqueue.t; }

  let create ?rand () =
    { next = 0; waiting = Fmap.empty ; kready = Rqueue.empty ?rand () }

  let fut fs =
    let id = fs.next in
    let f = { id; value = None; konts = []; futs = fs} in
    (fs.next <- fs.next + 1; fs.waiting <- Fmap.add id (F f) fs.waiting; f, f)

  let fut_value f = f.value
  let fut_wait f k = match f.value with
  | Some v -> Rqueue.add f.futs.kready (fun () -> k v)
  | None -> f.konts <- k :: f.konts

  let fut_set f v = match f.value with
  | Some _ -> invalid_arg "fut value already set"
  | None ->
      f.value <- Some v;
      match Fmap.find f.id f.futs.waiting with
      | exception Not_found -> assert false
      | (F f) ->
          f.futs.waiting <- Fmap.remove f.id f.futs.waiting;
          let v = Option.get f.value in
          let add_kont k = Rqueue.add f.futs.kready (fun () -> k v) in
          List.iter add_kont f.konts;
          ()

  let stir fs = Rqueue.take fs.kready
end

module Memo = struct
  type feedback =
  [ `Fiber_exn of exn * Printexc.raw_backtrace
  | `Fiber_fail of string
  | `Miss_tool of Tool.t * string
  | `Op_cache_error of Op.t * string
  | `Op_complete of Op.t ]

  type t = { c : ctx; m : memo }
  and ctx = { group : Op.group }
  and memo =
    { clock : Time.counter;
      cpu_clock : Time.cpu_counter;
      feedback : feedback -> unit;
      cwd : Fpath.t;
      env : Env.t ;
      guard : Guard.t;
      reviver : Reviver.t;
      exec : Exec.t;
      futs : Futs.t;
      mutable op_id : int;
      mutable ops : Op.t list; }

  let create ?clock ?cpu_clock:cc ~feedback ~cwd env guard reviver exec =
    let clock = match clock with None -> Time.counter () | Some c -> c in
    let cpu_clock = match cc with None -> Time.cpu_counter () | Some c -> c in
    let futs = Futs.create () and op_id = 0 and  ops = [] in
    let c = { group = "" } in
    let m =
      { clock; cpu_clock; feedback; cwd; env; guard; reviver; exec; futs;
        op_id; ops; }
    in
    { c; m }

  let memo
      ?(hash_fun = (module Hash.Xxh_64 : Hash.T)) ?env ?cwd ?cache_dir
      ?trash_dir ?(jobs = 4) ?feedback ()
    =
    let feedback = match feedback with | Some f -> f | None -> fun _ -> () in
    let fb_cache = (feedback :> File_cache.feedback -> unit) in
    let fb_exec = (feedback :> Exec.feedback -> unit) in
    let fb_memo = (feedback :> feedback -> unit) in
    let clock = Time.counter () in
    let env = match env with None -> Os.Env.current () | Some env -> Ok env in
    let cwd = match cwd with None -> Os.Dir.cwd () | Some cwd -> Ok cwd in
    Result.bind env @@ fun env ->
    Result.bind cwd @@ fun cwd ->
    let cache_dir = match cache_dir with
    | None -> Fpath.(cwd / "_b0" / ".cache") | Some d -> d
    in
    let trash_dir = match trash_dir with
    | None -> Fpath.(cwd / "_b0" / ".trash") | Some d -> d
    in
    Result.bind (File_cache.create ~feedback:fb_cache cache_dir) @@ fun cache ->
    let env = Env.v env in
    let guard = Guard.create () in
    let reviver = Reviver.create clock hash_fun cache in
    let trash = Trash.create trash_dir in
    let exec = Exec.create ~clock ~feedback:fb_exec ~trash ~jobs () in
    Ok (create ~clock ~feedback:fb_memo ~cwd env guard reviver exec)

  let clock m = m.m.clock
  let cpu_clock m = m.m.cpu_clock
  let env m = m.m.env
  let reviver m = m.m.reviver
  let guard m = m.m.guard
  let exec m = m.m.exec
  let trash m = Exec.trash m.m.exec
  let hash_string m s = Reviver.hash_string m.m.reviver s
  let hash_file m f = Reviver.hash_file m.m.reviver f
  let ops m = m.m.ops
  let timestamp m = Time.count m.m.clock
  let new_op_id m = let id = m.m.op_id in m.m.op_id <- id + 1; id
  let group m = m.c.group
  let with_group m group = { c = { group }; m = m.m }

  exception Fail of string
  let trap_kont_exn k m o = try k m o with
  | Fail e -> m.m.feedback (`Fiber_fail e)
  | Stack_overflow as e -> raise e
  | Out_of_memory as e -> raise e
  | Sys.Break as e -> raise e
  | e -> m.m.feedback (`Fiber_exn (e, Printexc.get_raw_backtrace ()))

  let continue_op m o =
    List.iter (Guard.set_file_ready m.m.guard) (Op.writes o);
    m.m.feedback (`Op_complete o);
    trap_kont_exn (fun m o -> Op.kontinue o) m o

  let discontinue_op m o =
    List.iter (Guard.set_file_never m.m.guard) (Op.writes o);
    Op.diskontinue o; m.m.feedback (`Op_complete o)

  let finish_op m o = match Op.status o with
  | Op.Executed ->
      if Op.revived o then continue_op m o else
      begin match Hash.equal (Op.hash o) Hash.nil with
      | true ->
          begin match Op.did_not_write o with
          | [] -> continue_op m o
          | miss ->
              Op.set_status o (Op.Failed (Op.Missing_writes miss));
              discontinue_op m o
          end
      | false ->
          match Reviver.record m.m.reviver o with
          | Ok true -> continue_op m o
          | Ok false ->
              let miss = Op.did_not_write o in
              Op.set_status o (Op.Failed (Op.Missing_writes miss));
              discontinue_op m o
          | Error e ->
              m.m.feedback (`Op_cache_error (o, e));
              begin match Op.did_not_write o with
              | [] -> continue_op m o
              | miss ->
                  Op.set_status o (Op.Failed (Op.Missing_writes miss));
                  discontinue_op m o
              end
      end
  | Op.Aborted | Op.Failed _ -> discontinue_op m o
  | Op.Waiting -> assert false

  let submit_op m o = match Op.status o with
  | Op.Aborted -> finish_op m o
  | Op.Waiting ->
      begin match Reviver.hash_op m.m.reviver o with
      | Error e ->
          (* FIXME Does this report errors cleanly ? We really want to
             be able to say which reads were supposed to be there and are not *)
          m.m.feedback (`Op_cache_error (o, e));
          Op.set_status o Op.Aborted;
          finish_op m o
      | Ok hash ->
          Op.set_hash o hash;
          begin match Reviver.revive m.m.reviver o with
          | Ok None -> Exec.schedule m.m.exec o
          | Ok (Some _) -> finish_op m o
          | Error e ->
              m.m.feedback (`Op_cache_error (o, e));
              Exec.schedule m.m.exec o
          end
      end
  | Op.Executed | Op.Failed _ -> assert false

  (* XXX we may blow stack continuations can add which stirs.
     XXX futures make it even worse. *)

  let rec stir ~block m = match Guard.allowed m.m.guard with
  | Some o -> submit_op m o; stir ~block m
  | None ->
      match Exec.collect m.m.exec ~block with
      | Some o -> finish_op m o; stir ~block m
      | None ->
          match Futs.stir m.m.futs with
          | Some k -> k (); stir ~block m
          | None -> ()

  let add_op m o =
    m.m.ops <- o :: m.m.ops; Guard.add m.m.guard o; stir ~block:false m

  let rec finish m =
    stir ~block:true m;
    match List.exists (fun o -> Op.status o = Op.Waiting) m.m.ops with
    | false ->
        assert (Futs.stir m.m.futs = None);
        Ok ()
    | true ->
        let undecided = Guard.root_undecided_files m.m.guard in
        Fpath.Set.iter (Guard.set_file_never m.m.guard) undecided;
        stir ~block:true m;
        (* TODO a cycle dep between ops will break this assertion. *)
        assert (not (List.exists (fun o -> Op.status o = Op.Waiting) m.m.ops));
        assert (Futs.stir m.m.futs = None);
        Error undecided

  let delete_trash ~block m = Trash.delete ~block (trash m)

  (* Fibers *)

  type 'a fiber = ('a -> unit) -> unit
  let fail fmt = Fmt.kstr (fun s -> raise (Fail s)) fmt
  let fail_error = function Ok v -> v | Error e -> raise (Fail e)

  (* Notify *)

  let notify m kind fmt =
    let op msg =
      let id = new_op_id m and created = timestamp m and k o = () in
      let o = Op.Notify.v_op ~id ~group:m.c.group ~created ~k kind msg in
      add_op m o
    in
    Fmt.kstr op fmt

  (* Files *)

  let file_ready m p = Guard.set_file_ready m.m.guard p
  let read m file k =
    let id = new_op_id m and created = timestamp m in
    let k o =
      let r = Op.Read.get o in
      let data = Op.Read.data r in
      Op.Read.discard_data r; k data
    in
    let o = Op.Read.v_op ~id ~group:m.c.group ~created ~k file in
    add_op m o

  let wait_files m files k =
    let id = new_op_id m and created = timestamp m and k o = k () in
    let o = Op.Wait_files.v_op ~id ~group:m.c.group ~created ~k files in
    add_op m o

  let write m ?(stamp = "") ?(reads = []) ?(mode = 0o644) write d =
    let id = new_op_id m and group = m.c.group and created = timestamp m in
    let k o = () in
    let o = Op.Write.v_op ~id ~group ~created ~k ~stamp ~reads ~mode ~write d in
    add_op m o

  let copy m ?(mode = 0o644) ?linenum ~src dst =
    let id = new_op_id m and group = m.c.group and created = timestamp m in
    let k o = () in
    let o = Op.Copy.v_op ~id ~group ~created ~k ~mode ~linenum ~src dst in
    add_op m o

  let mkdir m ?(mode = 0o755) dir k =
    let id = new_op_id m and created = timestamp m and k o = k () in
    let o = Op.Mkdir.v_op ~id ~group:m.c.group ~created ~k ~mode dir in
    add_op m o

  let delete m p k =
    let id = new_op_id m and created = timestamp m and k o = k () in
    let o = Op.Delete.v_op ~id ~group:m.c.group ~created ~k p in
    add_op m o

  (* FIXME better strategy to deal with builded tools. If the tool is a
     path check for readyness if not add it to the operations reads.
     I also suspect the tool lookup approach is not exactly right at
     the moment. Maybe this will clear up when we get the configuration
     story in. *)
  type _tool =
  { tool : Tool.t;
    tool_file : Fpath.t;
    tool_env : Os.Env.assignments;
    tool_relevant_env : Os.Env.assignments; }

  type tool =
  | Miss of Tool.t * string
  | Tool of _tool

  type cmd = { cmd_tool : tool; cmd_args : Cmd.t }

  let tool_env m t =
    let env = Env.env m.m.env in
    let tool_env, relevant = Tool.read_env t env in
    let forced_env = Env.forced_env m.m.env in
    let tool_env = Os.Env.override tool_env ~by:forced_env in
    let relevant = Os.Env.override relevant ~by:forced_env in
    tool_env, relevant

  let spawn_env m cmd_tool = function
  | None -> cmd_tool.tool_env, cmd_tool.tool_relevant_env
  | Some spawn_env ->
      let env = Env.env m.m.env in
      let tool_env, relevant = Tool.read_env cmd_tool.tool env in
      let forced_env = Env.forced_env m.m.env in
      let tool_env = Os.Env.override tool_env ~by:spawn_env in
      let tool_env = Os.Env.override tool_env ~by:forced_env in
      let relevant = Os.Env.override relevant ~by:spawn_env in
      let relevant = Os.Env.override relevant ~by:forced_env in
      Os.Env.to_assignments tool_env, Os.Env.to_assignments relevant

  let tool m tool =
    let cmd_tool = match Env.tool m.m.env (Tool.name tool) with
    | Error e -> Miss (tool, e)
    | Ok tool_file ->
        let tool_env, tool_relevant_env = tool_env m tool in
        let tool_env = Os.Env.to_assignments tool_env in
        let tool_relevant_env = Os.Env.to_assignments tool_relevant_env in
        Tool { tool; tool_file; tool_env; tool_relevant_env }
    in
    fun cmd_args -> { cmd_tool; cmd_args }

  let tool_opt m t = match Env.tool m.m.env (Tool.name t) with
  | Error e (* FIXME distinguish no lookup from errors *) -> None
  | Ok _ -> Some (tool m t)

  let spawn
      m ?(stamp = "") ?(reads = []) ?(writes = []) ?env ?cwd ?stdin
      ?(stdout = `Ui) ?(stderr = `Ui) ?(success_exits = [0]) ?k cmd
    =
    match cmd.cmd_tool with
    | Miss (tool, e) -> m.m.feedback (`Miss_tool (tool, e))
    | Tool tool ->
        let id = new_op_id m and created = timestamp m in
        let env, relevant_env = spawn_env m tool env in
        let cwd = match cwd with None -> m.m.cwd | Some d -> d in
        let k = match k with
        | None -> fun o -> ()
        | Some k ->
            fun o -> match Op.Spawn.result (Op.Spawn.get o) with
            | Ok (`Exited code) -> k code
            | _ -> assert false
        in
        let o =
          Op.Spawn.v_op ~id ~group:m.c.group ~created ~reads ~writes ~k ~stamp
            ~env ~relevant_env ~cwd ~stdin ~stdout ~stderr ~success_exits
            tool.tool_file cmd.cmd_args
        in
        add_op m o

  module Fut = struct
    type memo = t
    type 'a t = 'a Futs.fut * memo
    type 'a set = 'a Futs.fut_set
    let create m = let f, s = Futs.fut m.m.futs in (f, m), s
    let value (f, _) = Futs.fut_value f
    let set s = Futs.fut_set s
    let wait (f, m) k =
      let trap_kont_exn v = try k v with
      | Fail e -> m.m.feedback (`Fiber_fail e)
      | Stack_overflow as e -> raise e
      | Out_of_memory as e -> raise e
      | Sys.Break as e -> raise e
      | e -> m.m.feedback (`Fiber_exn (e, Printexc.get_raw_backtrace ()))
      in
      Futs.fut_wait f trap_kont_exn
  end
end

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
