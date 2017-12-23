(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* Keys *)

type key = B0_hash.t

let key_of_string s = match B0_hash.of_hex s with
| Some h -> Ok h
| None -> R.error_msgf "%S: not a cache key" s

let key_to_string k = B0_hash.to_hex k
let pp_key ppf k = Format.pp_print_string ppf (B0_hash.to_hex k)

(* Cache elements *)

type elt =
  { variant : string; (* Variant which wrote the file. *)
    age : int; (* Age in which the write was performed. *)
    op : B0_cmd.t; (* Operation that made the write. *)
    key : key; (* Write stamp. *)
    file_path : B0_fpath.t; (* File path written. *)
    file_stamp : B0_stamp.t;  (* File stamp of file path. *) }

let elt ~variant ~age ~op ~key file_path ~file_stamp =
  { variant; age; op; key; file_path; file_stamp }

let elt_variant e = e.variant
let elt_age e = e.age
let elt_op e = e.op
let elt_key e = e.key
let elt_file_path e = e.file_path
let elt_file_stamp e = e.file_stamp

(* Cache *)

type t =
  { dir : B0_fpath.t;
    index_file : B0_fpath.t;
    disable : bool;
    mutable copying : bool; (* [true] if link(2) fails with EXDEV. *)
    mutable dur_counter : B0_time.counter;
    mutable index : elt B0_hash.Map.t;
    mutable file_stamps : B0_stamp.t B0_fpath.map;
    mutable file_stamp_dur : B0_time.span; }

let _file_stamp c file = match B0_fpath.Map.find file c.file_stamps with
| s -> s
| exception Not_found ->
  let t = B0_time.counter () in
  let stamp = B0_stamp.file file in
  let dur = B0_time.count t in
  c.file_stamps <- B0_fpath.Map.add file stamp c.file_stamps;
  c.file_stamp_dur <- B0_time.add dur c.file_stamp_dur;
  stamp

let file_stamp c file = _file_stamp c file (* FIXME catch Sys_error *)

let empty ~index_file ~dir =
  { dir; index_file; index = B0_hash.Map.empty;
    dur_counter = B0_time.counter ();
    disable = false;
    copying = false;
    file_stamps = B0_fpath.Map.empty;
    file_stamp_dur = B0_time.zero; }

let is_empty c = B0_hash.Map.is_empty c.index
let dir c = c.dir
let index_file c = c.index_file

let file_stamp_dur c = c.file_stamp_dur
let file_stamps c = c.file_stamps

let cache_file c key = B0_fpath.(c.dir / B0_hash.to_hex key)

(* Persist *)

let exists ~index_file = B0_os.File.exists index_file
let codec = B0_codec.v ~id:"cache-index"
let load ~index_file ~dir =
  B0_os.Dir.create dir >>= fun _ ->
  B0_os.File.exists index_file >>= function
  | false -> Ok (empty ~index_file ~dir)
  | true ->
      B0_codec.read codec index_file >>= fun index ->
      Ok { dir; index_file; index; disable = false; copying = false;
           dur_counter = B0_time.counter ();
           file_stamps = B0_fpath.Map.empty;
           file_stamp_dur = B0_time.zero; }

let save c =
  B0_os.Dir.create (B0_fpath.parent c.index_file) >>= fun _ ->
  B0_codec.write codec c.index_file c.index

(* Operations *)

let mem c k = B0_hash.Map.mem k c.index
let add c elt =
  let cache_file = cache_file c elt.key in
  B0_os.File.link ~force:true ~target:elt.file_path cache_file >>= fun () ->
  c.index <- B0_hash.Map.add elt.key elt c.index;
  Ok ()

let rem c k =
  c.index <- B0_hash.Map.remove k c.index;
  B0_os.File.delete ~must_exist:false (cache_file c k)

let find c k = match B0_hash.Map.find k c.index with
| exception Not_found -> None
| elt -> Some elt

let use c k = match B0_hash.Map.find k c.index with
| exception Not_found -> Ok false
| elt ->
    let cache_file = cache_file c elt.key in
    B0_os.File.link ~force:true ~target:cache_file elt.file_path >>= fun () ->
    Ok true

let verify ~repair c k = match B0_hash.Map.find k c.index with
| exception Not_found ->
    begin B0_os.File.exists (cache_file c k) >>= function
    | true -> Ok `Miss_index
    | false -> Ok `Unknown
    end
| e ->
    let cached = cache_file c k in
    B0_os.File.exists cached >>= function
    | false ->
        if repair then (c.index <- B0_hash.Map.remove k c.index); Ok `Miss_file
    | true ->
        let cstamp = B0_stamp.file cached in
        match B0_stamp.equal (elt_file_stamp e) cstamp with
        | true -> Ok `Ok
        | false ->
            match repair with
            | false -> Ok `Stamp_mismatch
            | true ->
                let elt = { e with file_stamp = cstamp } in
                c.index <- B0_hash.Map.add k elt c.index;
                Ok `Stamp_mismatch

let foreign ~ignore_keys c =
  B0_os.Dir.contents ~dotfiles:true ~rel:true c.dir >>| fun contents ->
  let rec loop acc = function
  | [] -> List.sort (fun (_, p0) (_, p1) -> B0_fpath.compare p0 p1) acc
  | p :: ps ->
      match B0_fpath.to_string p with
      | "index" -> loop acc ps
      | f ->
          match B0_hash.of_hex f with
          | None -> loop ((`Other, p) :: acc) ps
          | Some h when ignore_keys || B0_hash.Map.mem h c.index -> loop acc ps
          | Some h -> loop ((`Key, p) :: acc) ps
  in
  loop [] contents

(* Traverse *)

let fold f c acc = B0_hash.Map.fold (fun _ elt acc -> f elt acc) c.index acc
let iter f c = B0_hash.Map.iter (fun _ elt -> f elt) c.index
let path_map c =
  let add e m = B0_fpath.Map.add (elt_file_path e) e m in
  fold add c B0_fpath.Map.empty

(* Build ops *)

let set_dur_counter c counter = c.dur_counter <- counter
let time_stamp c = B0_time.count c.dur_counter
let cache_file c key = B0_fpath.(c.dir / B0_stamp.to_hex key)

let uerror = Unix.error_message

let log_xdev () =
  B0_log.warn begin fun m ->
    m "%a" B0_fmt.text
      "Using slow copying cache. Make sure the cache directory and the \
       variant directory are on the same file system."
  end

let rec unlink p =
  try Unix.unlink (B0_fpath.to_string p) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | Unix.Unix_error (Unix.EINTR, _, _) -> unlink p
  | Unix.Unix_error (e, _, _) ->
      failwith (B0_string.strf "unlink %a: %s" B0_fpath.pp p (uerror e))

let rec force_link t p = (* unused for now *)
  try Unix.link (B0_fpath.to_string t) (B0_fpath.to_string p) with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> unlink p; force_link t p
  | Unix.Unix_error (e, _, _) ->
      failwith (B0_string.strf "force link target %a to %a: %s"
                  B0_fpath.pp t B0_fpath.pp p (uerror e))

let rec copy p0 p1 =
  try
    begin
      let mode = (Unix.stat (B0_fpath.to_string p0)).Unix.st_perm in
      B0_os.File.read p0
      >>= fun data -> B0_os.File.write ~mode p1 data
      >>| fun () -> true
    end
    |> R.failwith_error_msg
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | Unix.Unix_error (Unix.EINTR, _, _) -> copy p0 p1
  | Unix.Unix_error (e, _, _) ->
      failwith (B0_string.strf "stat %a: %s" B0_fpath.pp p0 (uerror e))

let rec put c t p = match c.copying with
| true -> copy t p
| false ->
    try Unix.link (B0_fpath.to_string t) (B0_fpath.to_string p); true with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> false
    | Unix.Unix_error (Unix.EINTR, _, _) -> put c t p
    | Unix.Unix_error (Unix.EXDEV, _, _) ->
        log_xdev (); c.copying <- true; copy t p
    | Unix.Unix_error (e, _, _) ->
        failwith (B0_string.strf "link target %a to %a: %s"
                    B0_fpath.pp t B0_fpath.pp p (uerror e))

let op_write_key o file =
  let stamp = B0_stamp.to_byte_string @@ B0_op.stamp o in
  B0_stamp.string (stamp ^ B0_fpath.to_string file)

let log_exec o =
  B0_log.debug (fun m -> m ~header:"EXEC" "%a" B0_op.pp_log_line o)

let rec put_writes_from_cache c o =
  let rec loop o undo = function
  | [] ->
      B0_op.set_cached o true;
      B0_op.(set_status o Executed);
      B0_op.set_exec_end_time o (time_stamp c);
      log_exec o; true;
  | f :: fs ->
      let key = op_write_key o f in
      match put c (cache_file c key) f with
      | true -> loop o (f :: undo) fs
      | false ->
          B0_op.set_exec_start_time o B0_time.zero; (* faux départ *)
          List.iter unlink undo;
          false
  in
  let writes = B0_op.writes o in
  match B0_fpath.Set.is_empty writes with
  | true -> false (* FIXME, once multi writes are partially allowed *)
  | false ->
      B0_op.set_exec_start_time o (time_stamp c);
      loop o [] (B0_fpath.Set.elements (B0_op.writes o))

let rec put_writes_in_cache c o =
  let rec loop o = function
  | [] -> ()
  | f :: fs ->
      let key = op_write_key o f in
      let key = cache_file c key in
      match put c f key with
      | true -> loop o fs
      | false ->
          failwith begin
            B0_string.strf
              "write %a does not exist (key %a)" B0_fpath.pp f B0_fpath.pp key
          end
  in
  loop o (B0_fpath.Set.elements (B0_op.writes o))

let op_stamp_reads ?(init = []) c o =
  let add_read f acc = B0_stamp.to_byte_string (_file_stamp c f) :: acc in
  B0_fpath.Set.fold add_read (B0_op.reads o) init

let spawn_stamp c o s =
  let acc = match B0_op.spawn_stdin s with
  | None -> []
  | Some f -> [B0_fpath.to_string f]
  in
  let acc = op_stamp_reads ~init:acc c o in
  let acc = Array.fold_left (fun acc v -> v :: acc) acc (B0_op.spawn_env s) in
  let acc = List.rev_append (B0_cmd.to_rev_list @@ B0_op.spawn_cmd s) acc in
  match acc with
  | [] -> assert false
  | exe :: _ as acc ->
      let exe_stamp = _file_stamp c (B0_fpath.v exe) in
      let acc = B0_stamp.to_byte_string exe_stamp :: acc in
      B0_stamp.string (String.concat "" acc)

let exec_spawn c o s =
  B0_op.set_stamp o (spawn_stamp c o s);
  put_writes_from_cache c o

let exec c o =
  try match c.disable with
  | true -> false
  | false ->
      match B0_op.kind o with
      | B0_op.Spawn s -> exec_spawn c o s
      | B0_op.Copy_file _
      | B0_op.Read _
      | B0_op.Write _ | B0_op.Delete _ | B0_op.Mkdir _ | B0_op.Sync _ -> false
  with
  | Failure e ->
      B0_log.err (fun m -> m "Cached exec: op %d: %s" (B0_op.id o) e); false

let add_op c o =
  try match c.disable with
  | true -> ()
  | false ->
      match B0_op.cached o with
      | true -> ()
      | false ->
          match B0_op.kind o with
          | B0_op.Spawn _ -> put_writes_in_cache c o
          | B0_op.Copy_file _
          | B0_op.Read _
          | B0_op.Write _ | B0_op.Delete _ | B0_op.Mkdir _ | B0_op.Sync _ -> ()
  with
  | Failure e ->
      B0_log.err (fun m -> m "Cache put: op %d: %s" (B0_op.id o) e)


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
