(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** User interaction fragments.

    User interaction fragments for devising build tools. *)

(** {1:ui User interaction fragments} *)

open B0_std
open Cmdliner

(** {!Cmdliner} fragments. *)
module Cli : sig

  (** {1:out_fmt Specifying output formats} *)

  type out_fmt = [ `Normal | `Short | `Long ]
  (** The type for specifying output format details. *)

  val out_fmt :
    ?docs:string -> ?short_opts:string list -> ?long_opts:string list ->
    unit -> out_fmt Term.t
  (** [out_fmt ~short_opts ~long_opts ()] are mutually exclusive options
      to specify short and long output format, without options this is
      [`Normal]. [short_opts] defaults to [["s"; "short"]] and
      [long_opts] default to [["l"; "long"]]. [docs] is the manual section
      in which options are documented. *)
end

(** {!B000.File_cache} interaction. *)
module File_cache : sig

  (** {1:stats Statistics about the file cache} *)

  module Stats : sig

    (** {1:keys Key statistics} *)

    type keys
    (** The type for statistics about a set of keys *)

    val keys_count : keys -> int
    (** [keys_count s] is the number of keys in the set of keys. *)

    val keys_file_count : keys -> int
    (** [keys_file_count s] is the number of files in the set of keys. *)

    val keys_byte_size : keys -> int
    (** [keys_count s] is the total byte size of the files in the set
        of keys. *)

    val keys_zero : keys
    (** [keys_zero] are zeros. *)

    val keys_sub : keys -> keys -> keys
    (** [keys_sub s0 s1] is he field-wise substraction [s0 - s1]. *)

    val pp_keys : keys Fmt.t
    (** [pp_keys] formats key statistics. *)

    val of_keys :
      B000.File_cache.t -> B000.File_cache.key list -> (keys, string) result
    (** [of_keys ks] are statistics for [ks]. *)

    (** {1:cache Cache statistics} *)

    type cache
    (** The type for cache statistics *)

    val zero : cache
    (** [zero] are zeros. *)

    val all_keys : cache -> keys
    (** [all_keys s] are statistics about all keys in the cache. *)

    val unused_keys : cache -> keys
    (** [unused_keys s] are statistics about unused keys in the cache.
        {b Warning} Only relevant if {!is_unused} is. *)

    val pp : cache Fmt.t
    (** [pp] formats cache statistics. *)

    val of_cache : B000.File_cache.t -> (cache, string) result
    (** [of_cache c] are satistics of [c]. *)
  end

  (** {1:high-level High-level commands.}

      These commands act on a cache directory. They avoid to create
      it via {!B000.File_cache.create} if it doesn't exists and mostly
      return [Ok ()] in these cases. *)

  val delete :
    dir:Fpath.t -> [ `All | `Keys of B000.File_cache.key list ] ->
    (unit, string) result
  (** [delete dir keys] deletes [keys] in [dirs] if an explicit key
      does not exist in [dir] a {!Log.warn} is issued. If [`All] is
      specified [dir] is deleted and recreated. *)

  val gc : dir:Fpath.t -> (unit, string) result
  (** [gc dir] deletes unused keys via {!B00.File_cache.delete_unused}. *)

  val size : dir:Fpath.t -> (unit, string) result
  (** [size dir] shows statistics about the file cache on stdout
      via {!B00.File_cache.Stats.pp}. *)

  val trim :
    dir:Fpath.t -> max_byte_size:int -> pct:int -> (unit, string) result
    (** [trim dir ~max_byte_size ~pct] trims the cache using
        {!B00.File_cache.trim_size}. *)

  (** {1:cli Cli fragments} *)

  val key_arg : B000.File_cache.key Cmdliner.Arg.conv
  (** [key_arg] is an argument converter for cache keys. *)

  val keys_none_is_all :
    ?pos_right:int -> unit ->
    [ `All | `Keys of B000.File_cache.key list ] Cmdliner.Term.t
   (** [keys_none_is_all ~pos_right ()] are the keys at the right
       of position [pos_right] (defaults is all positional arguments).
       If none is specified this is [`All]. *)
end

(** {!B000.Op} interaction. *)
module Op : sig

  (** {1:deps Finding dependencies} *)

  val find_needs :
    ?acc:B000.Op.Set.t -> recursive:bool -> writes:B000.Op.Set.t Fpath.Map.t ->
    B000.Op.Set.t -> B000.Op.Set.t
  (** [find_needs ~recursive ~writes ~acc ops] add to [acc] (defaults
      to {!B00.Op.Set.empty}) the set of operations in the write index
      [writes] that need to be executed for the set of operations
      [ops] to be able to proceed. If [recursive] is [false] only direct
      dependencies are reported. *)

  val find_enables :
    ?acc:B000.Op.Set.t -> recursive:bool -> reads:B000.Op.Set.t Fpath.Map.t ->
    B000.Op.Set.t -> B000.Op.Set.t
  (** [find_enables ~recursive ~writes ~acc ops] add to [acc]
      (defaults to {!B00.Op.Set.empty}) the set of operations in the
      read index [reads] that are enabled by the set of operations
      [ops]. If [recursive] is [false] only direct dependencies are
      reported. *)

  (** {1:query Operation query} *)

  type query = B000.Op.t list -> B000.Op.t list
  (** The type for build operation queries. This is not simply predicate
      because of dependency selection. *)

  val select :
    reads:Fpath.t list -> writes:Fpath.t list -> ids:B000.Op.id list ->
    hashes:Hash.t list -> groups:string list -> B000.Op.t -> bool
  (** [select ~reads ~writes ~ids ~hashes ~groups o] is [true]
      iff [o] reads a file in [reads] or writes a file in [writes]
      or has its id in [ids], or has its hash in [hashes] or has
      its [group] in [groups] or if all these selector lists are empty. *)

  val select_deps :
    needs:bool -> enables:bool -> recursive:bool -> dom:B000.Op.t list ->
    B000.Op.t list -> B000.Op.t list
  (** [select_deps ~needs ~enables ~recusrive  ~dom ops] select the operation
      [needs] and/or [enables] of [ops] [recursive]ly in [dom]. This is [ops]
      if both [needs] and [enables] are [false]. *)

  val filter :
    revived:bool option ->
    statuses:[ `Aborted | `Done | `Failed | `Waiting ] list ->
    kinds:[ `Copy | `Delete | `Mkdir | `Notify | `Read | `Spawn | `Wait_files
          | `Write ] list ->
    B000.Op.t -> bool
  (** [filter ~revived ~statuses ~kinds] is an operation filter for the
      given filters. *)

  val order :
    by:[`Create | `Dur | `Wait | `Start] -> B000.Op.t list -> B000.Op.t list
  (** [order ~by ops] orders [ops] by [by] time. *)

  val query :
    select:(B000.Op.t -> bool) ->
    select_deps: (dom:B000.Op.t list -> B000.Op.t list -> B000.Op.t list) ->
    filter:(B000.Op.t -> bool) ->
    order:(B000.Op.t list -> B000.Op.t list) -> query
  (** [query ~select ~select_deps ~filer ~order] is an operation
      query that [select]s operations, their dependencies (or not)
      according to [select_deps], filters the result with [filter]
      and orders them by [order]. *)

  (** {1:cli Command line interface} *)

  val groups :
    ?opts:string list -> ?docs:string -> ?doc:string -> unit ->
    string list Cmdliner.Term.t

  val select_cli :
    ?docs:string ->
    ?groups:string list Cmdliner.Term.t ->
    unit -> (B000.Op.t -> bool) Cmdliner.Term.t

  val select_deps_cli :
    ?docs:string -> unit ->
    (dom:B000.Op.t list -> B000.Op.t list -> B000.Op.t list) Cmdliner.Term.t
  val filter_cli : ?docs:string -> unit -> (B000.Op.t -> bool) Cmdliner.Term.t
  val order_cli :
    ?docs:string -> unit -> (B000.Op.t list -> B000.Op.t list) Cmdliner.Term.t
  val query_cli : ?docs:string -> unit -> query Cmdliner.Term.t
  val query_man : Cmdliner.Manpage.block list
end

(** {!B00.Memo} interaction. *)
module Memo : sig

  (** {1:feedback Memo feedback} *)

  val pp_leveled_feedback :
    ?sep:unit Fmt.t -> ?op_howto:B000.Op.t Fmt.t -> show_op:Log.level ->
    show_ui:Log.level -> level:Log.level ->
    [B00.Memo.feedback | B000.Exec.feedback] Fmt.t
  (** [pp_leveled_feedback ~sep ~op_howto ~show_spawn_ui ~show_success ~level
      ppf] formats memo feedback on [ppf] followed by [sep] iff something
      is printed (defaults to {!B0_std.Fmt.flush_nl}).
      {ul
      {- {!B0_std.Log.Quiet} formats nothing}
      {- {!B0_std.Log.Debug} report all operations with
         {!B000_conv.Op.pp_short_ui}.}}
      {ul
      {- [show_ui] is the level at which any completed operation
         gets logged with {!B000_conv.Op.pp_ui}.}
      {- [show_op] is the level at which any completed operation gets
         logged with {!B000_conv.Op.pp_short_ui}}}
      The formatter [op_howto] should format a way to got more information
      about an operation, default to {!nop}. *)

  (** {1:dirs_files Directories and files} *)

  (** {2:b0_dir B0 directory} *)

  val b0_dir_env : string
  (** [b0_dir_env] is ["B0_DIR"]. *)

  val b0_dir_name : string
  (** [b0_dir_name] is ["_b0"] the default b0 directory name. *)

  val b0_dir :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Arg.env -> unit -> Fpath.t option Term.t
  (** [b0_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 directory.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["b0-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!b0_dir_env}.}} *)

  val get_b0_dir :
    cwd:Fpath.t -> root:Fpath.t -> b0_dir:Fpath.t option -> Fpath.t
  (** [get_b0_dir ~cwd ~root ~b0_dir] determines a b0 directory. If
      [b0_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
      then this is [Fpath.(root / b0_dir_name)]. *)

  (** {2:cache_dir File cache directory} *)

  val cache_dir_env : string
  (** [b0_dir_env] is ["B0_CACHE_DIR"]. *)

  val cache_dir_name : string
  (** [cache_dir_name] is [".cache"] the default cache directory name
      in the [b0] directory. *)

  val cache_dir :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Arg.env -> unit -> Fpath.t option Term.t
  (** [cache_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 cache directory.
      {ul
      {- [opts] are the cli options to specify it, default to
         [["cache-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!cache_dir_env}.}} *)

  val get_cache_dir :
    cwd:Fpath.t -> b0_dir:Fpath.t -> cache_dir:Fpath.t option -> Fpath.t
  (** [get_cache_dir ~cwd ~b0_dir ~cache_dir] determines a cache directory.
      If [cache_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
      then this is [Fpath.(b0_dir / cache_dir)]. *)

  (** {2:trash_dir Trash directory} *)

  val trash_dir_name : string
  (** [trash_dir_name] is [".trash"] the default trash directoy name
      in the [b0] directory. *)

  val get_trash_dir :
    cwd:Fpath.t -> b0_dir:Fpath.t -> trash_dir:Fpath.t option -> Fpath.t
  (** [get_trash_dir ~cwd ~b0_dir ~trash_dir] dtermiens a trash directory.
      If [trash_dir] is [Some d] then this is [Fpath.(cwd // d]. If
      [None] then this is [Fpath.(b0_dir /trash_dir)]. *)

  (** {2:log_file Log file} *)

  val log_file_env : string
  (** [b0_dir_env] is ["B0_LOG_FILE"]. *)

  val log_file_name : string
  (** [log_file_name] is [".log"] the default log file name in
      the [b0] directory. *)

  val log_file :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Arg.env -> unit -> Fpath.t option Term.t
  (** [log_file ~doc_none ~docs ~doc ~env] is a cli interface for
      specifing a b0 log file.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["log-file"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!cache_dir_env}.}} *)

  val get_log_file :
    cwd:Fpath.t -> b0_dir:Fpath.t -> log_file:Fpath.t option -> Fpath.t
  (** [get_log_file ~cwd ~b0_dir ~log_file] determines a log file.
      If [log_file] is [Some f] then this is [Fpath.(cwd // f)]. If [None]
      then this is [Fpath.(b0_dir /log_file)]. *)

  (** {1:memo Memo parameters} *)

  (** {2:jobs Jobs} *)

  val jobs_env : string
  (** [jobs_env] is ["B0_JOBS"]. *)

  val jobs :
    ?opts:string list ->  ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Arg.env -> unit -> int option Term.t
  (** [jobs] is a cli interface for specifying the maximal number of
      commands to spawn concurrently.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["j";"jobs"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!jobs_env}.}} *)

  val get_jobs : jobs:int option -> int
  (** [get_jobs ~jobs] determines a maximal number of spawns. If jobs
      is [None] then {!B0_std.Os.Cpu.logical_count} is used. *)

  (** {2:hash_fun Hash function} *)

  val hash_fun_env : string
  (** [hash_fun_env] is ["B0_HASH_FUN"]. *)

  val hash_fun :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Arg.env -> unit -> (module Hash.T) option Term.t
  (** [hash_fun] is a cli interface for specfiying hash function
      used for caching.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["hash-fun"]].}
      {- [docs] is where the option is documented, defaults to
         {!Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!hash_fun_env}.}} *)

  val get_hash_fun : hash_fun:(module Hash.T) option -> (module Hash.T)
  (** [get_hash_fun ~hash_fun] determines a hash function. If [hash_fun]
      is [None] then {!B0_std.Hash.Xxh_64} is used. *)

  (** {1:logs Logs} *)

  (** {!B00.Memo} log.

      A {!B00.Memo} log has all the build operations and a few
      global statistics about the memo. *)
  module Log : sig

    (** {1:logs Logs} *)

    type t
    (** The type for {!B00.Memo} logs. *)

    val ops : t -> B000.Op.t list
    (** [ops l] are the operations of the log. *)

    val of_memo : B00.Memo.t -> t
    (** [of_memo m] is a log for memo [m]. *)

    (** {1:io IO} *)

    val bincode : t Bincode.t
    (** [bincode] is a binary codec for logs. *)

    val write : Fpath.t -> t -> (unit, string) result
    (** [write f l] writes log [l] to file [f]. *)

    val read : Fpath.t -> (t, string) result
    (** [read f] read a log from file [f]. *)

    (** {1:fmt Log formatters} *)

    val pp_stats : Op.query -> t Fmt.t
    (** [pp_stats sel] formats statistics stored in the log using
        [query] to select operations that are part of the statistics. *)

    type out_kind = [`Hashed_files | `Op_hashes | `Ops | `Stats | `Trace_event]
    (** The type for output kinds. *)

    val out :
      Format.formatter -> Cli.out_fmt -> out_kind -> Op.query -> t -> unit
    (** [out] formats a log on the given formatter. *)

    (** {1:cli Command line interface} *)

    val out_kind_cli : ?docs:string -> unit -> out_kind Cmdliner.Term.t
    (** [out_kind_cli ~docs ()] are mutually exclusive options to specify
        alternate output information. *)
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
