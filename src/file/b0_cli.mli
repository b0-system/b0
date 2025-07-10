(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line interface fragments. *)

open B0_std

(** {2:def_spec Specifying b0 definitions} *)

val units :
  ?docs:string -> ?doc:string -> unit -> string list Cmdliner.Term.t
(** [units] defines unit names with [-u] and [--unit]. *)

val x_units :
  ?docs:string -> ?doc:string -> unit -> string list Cmdliner.Term.t
(** [x_units] defines unit names to exclude with [-x] and [--x-unit]. *)

val packs :
  ?docs:string -> ?doc:string -> unit -> string list Cmdliner.Term.t
(** [packs] defines pack names with [-p] and [--pack]. *)

val x_packs :
  ?docs:string -> ?doc:string -> unit -> string list Cmdliner.Term.t
  (** [packs] defines pack names to exclude via [-X] and [--x-pack]. *)

val get_excluded_units :
  x_units:string list -> x_packs:string list -> (B0_unit.Set.t, string) result
(** [get_excluded_units ~x_units ~x_packs] is the set of units excluded
    by the given unit and pack names. *)

(** {1:fragments Cli commands and fragments} *)

(** {b TODO.} Maybe all this stuff should be moved to a [b0_memo_cli]. *)

(** {!B0_zero.File_cache} interaction. *)
module File_cache : sig

  (** {1:high-level High-level commands.}

      These commands act on a cache directory. They avoid to create
      it via {!B0_zero.File_cache.make} if it doesn't exists and return
      [Ok false] in that case. *)

  val keys_of_success_ops : B0_zero.Op.t list -> String.Set.t
  (** [keys_of_success_ops ops] are the non-nil hashes of the operations of
      [ops] that are {!B0_zero.Op.Success}. *)

  val delete :
    dir:Fpath.t -> [ `All | `Keys of B0_zero.File_cache.key list ] ->
    (bool, string) result
  (** [delete dir keys] deletes [keys] in [dirs] if an explicit key
      does not exist in [dir] a {!Log.warn} is issued. If [`All] is
      specified [dir] is deleted and recreated. *)

  val gc : dir:Fpath.t -> used:String.Set.t -> (bool, string) result
  (** [gc ~dir ~used] deletes keys that are not in [used]. *)

  val keys : dir:Fpath.t -> (bool, string) result
  (** [keys dir] lists the file cache keys on stdout. *)

  val stats : dir:Fpath.t -> used:String.Set.t -> (bool, string) result
  (** [status ~dir ~used] shows statistics about the file cache on stdout.
      [used] determines keys that are in use. *)

  val trim :
    dir:Fpath.t -> used:String.Set.t -> max_byte_size:int -> pct:int ->
    (bool, string) result
  (** [trim dir ~used ~max_byte_size ~pct] trims the cache using
      {!B0_zero.File_cache.trim_size}. [used] determines keys that
      are assumed to be used. *)

  (** {1:cli Cli fragments} *)

  val key_arg : B0_zero.File_cache.key Cmdliner.Arg.conv
  (** [key_arg] is an argument converter for cache keys. *)

  val keys_none_is_all :
    ?pos_right:int -> unit ->
    [ `All | `Keys of B0_zero.File_cache.key list ] Cmdliner.Term.t
   (** [keys_none_is_all ~pos_right ()] are the keys at the right
       of position [pos_right] (defaults is all positional arguments).
       If none is specified this is [`All]. *)

  val trim_cli :
    ?mb_opts:string list ->
    ?pct_opts:string list ->
    ?docs:string -> unit -> (int * int) Cmdliner.Term.t
  (** [trim_cli ~docs ()] are command line options to specify a maximal
      byte size and percentage to give to {!trim}. *)
end

(** {!B0_zero.Op} interaction. *)
module Op : sig

  (** {1:deps Finding dependencies} *)

  val find_needs :
    ?acc:B0_zero.Op.Set.t -> recursive:bool ->
    writes:B0_zero.Op.Set.t Fpath.Map.t -> B0_zero.Op.Set.t -> B0_zero.Op.Set.t
  (** [find_needs ~recursive ~writes ~acc ops] add to [acc] (defaults
      to {!B0_zero.Op.Set.empty}) the set of operations in the write index
      [writes] that need to be executed for the set of operations
      [ops] to be able to proceed. If [recursive] is [false] only direct
      dependencies are reported. *)

  val find_enables :
    ?acc:B0_zero.Op.Set.t -> recursive:bool ->
    reads:B0_zero.Op.Set.t Fpath.Map.t -> B0_zero.Op.Set.t -> B0_zero.Op.Set.t
  (** [find_enables ~recursive ~writes ~acc ops] add to [acc]
      (defaults to {!B0_zero.Op.Set.empty}) the set of operations in the
      read index [reads] that are enabled by the set of operations
      [ops]. If [recursive] is [false] only direct dependencies are
      reported. *)

  (** {1:query Operation query} *)

  type query = B0_zero.Op.t list -> B0_zero.Op.t list
  (** The type for build operation queries. This is not simply predicate
      because of dependency selection. *)

  val select :
    reads:Fpath.t list -> writes:Fpath.t list -> ids:B0_zero.Op.id list ->
    hashes:B0_hash.t list -> marks:string list -> B0_zero.Op.t -> bool
  (** [select ~reads ~writes ~ids ~hashes ~marks o] is [true]
      iff [o] reads a file in [reads] or writes a file in [writes]
      or has its id in [ids], or has its hash in [hashes] or has
      its [mark] in [marks] or if all these selector lists are empty. *)

  val select_deps :
    needs:bool -> enables:bool -> recursive:bool -> dom:B0_zero.Op.t list ->
    B0_zero.Op.t list -> B0_zero.Op.t list
  (** [select_deps ~needs ~enables ~recusrive  ~dom ops] select the operation
      [needs] and/or [enables] of [ops] [recursive]ly in [dom]. This is [ops]
      if both [needs] and [enables] are [false]. *)

  val filter :
    revived:[`Executed | `Revived | `Unrevived ] option ->
    statuses:[ `Aborted | `Success | `Failed | `Waiting ] list ->
    kinds:[ `Copy | `Delete | `Mkdir | `Notify | `Read | `Spawn | `Wait_files
          | `Write ] list -> B0_zero.Op.t -> bool
  (** [filter ~revived ~statuses ~kinds] is an operation filter for the
      given filters. *)

  val order :
    by:[`Create | `Dur | `Wait | `Start] ->
    B0_zero.Op.t list -> B0_zero.Op.t list
  (** [order ~by ops] orders [ops] by [by] time. *)

  val query :
    select:(B0_zero.Op.t -> bool) ->
    select_deps:
      (dom:B0_zero.Op.t list -> B0_zero.Op.t list -> B0_zero.Op.t list) ->
    filter:(B0_zero.Op.t -> bool) ->
    order:(B0_zero.Op.t list -> B0_zero.Op.t list) -> query
  (** [query ~select ~select_deps ~filer ~order] is an operation
      query that [select]s operations, their dependencies (or not)
      according to [select_deps], filters the result with [filter]
      and orders them by [order]. *)

  (** {1:cli Command line interface} *)

  val marks :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?docv:string -> unit ->
    string list Cmdliner.Term.t

  val select_cli :
    ?docs:string ->
    ?marks:string list Cmdliner.Term.t ->
    unit -> (B0_zero.Op.t -> bool) Cmdliner.Term.t

  val select_deps_cli :
    ?docs:string -> unit ->
    (dom:B0_zero.Op.t list -> B0_zero.Op.t list -> B0_zero.Op.t list)
      Cmdliner.Term.t

  val filter_cli : ?docs:string -> unit -> (B0_zero.Op.t -> bool)
      Cmdliner.Term.t

  val order_cli :
    ?docs:string -> unit ->
    (B0_zero.Op.t list -> B0_zero.Op.t list) Cmdliner.Term.t

  val s_selection_options : string
  (** [s_selection_options] is a section name for build operation selection
      options. *)

  val query_cli : ?docs:string -> unit -> query Cmdliner.Term.t
  (** [query_cli ~docs ()] is a command line interface to select
      build operations. [docs] is where the option are documented, defaults to
      {!Cmdliner.Manpage.s_options} *)

  val query_man : Cmdliner.Manpage.block list
  (** [query_man] is a manual fragment for {!query_cli}. *)
end

(** {!B0_memo} interaction. *)
module Memo : sig

  (** {1:feedback Memo feedback} *)

  val pp_leveled_feedback :
    ?sep:unit Fmt.t -> ?op_howto:B0_zero.Op.t Fmt.t -> show_op:Log.level ->
    show_ui:Log.level -> level:Log.level ->
    [B0_memo.feedback | B0_zero.Exec.feedback] Fmt.t
  (** [pp_leveled_feedback ~sep ~op_howto ~show_spawn_ui ~show_success ~level
      ppf] formats memo feedback on [ppf] followed by [sep] iff something
      is printed (defaults to {!B0_std.Fmt.flush_nl}).
      {ul
      {- {!B0_std.Log.Quiet} formats nothing}
      {- {!B0_std.Log.Debug} report all operations with
         {!B0_zero_conv.Op.pp_line}.}}
      {ul
      {- [show_ui] is the level at which any completed operation
         gets logged with {!B0_zero_conv.Op.pp_ui}.}
      {- [show_op] is the level at which any completed operation gets
         logged with {!B0_zero_conv.Op.pp_line_and_ui}}}
      The formatter [op_howto] should format a way to got more information
      about an operation, default to {!B0_std.Fmt.nop}. *)

  (** {1:dirs_files Directories and files} *)

  (** {2:b0_dir b0 directory} *)

  val b0_dir_env : string
  (** [b0_dir_env] is ["B0_DIR"]. *)

  val b0_dir_name : string
  (** [b0_dir_name] is ["_b0"] the default b0 directory name. *)

  val b0_dir :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Cmd.Env.info -> unit -> Fpath.t option Cmdliner.Term.t
  (** [b0_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 directory.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["b0-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_options}}
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

  val find_dir_with_b0_dir : start:Fpath.t -> Fpath.t option
  (** [find_dir_with_b0_dir ~start] finds the first directory starting
      with [start] that has a {!b0_dir_name} directory. [None] is
      returned if none could found or if [start] is relative. *)

  (** {2:cache_dir File cache directory} *)

  val cache_dir_env : string
  (** [b0_dir_env] is ["B0_CACHE_DIR"]. *)

  val cache_dir_name : string
  (** [cache_dir_name] is [".cache"] the default cache directory name
      in the [b0] directory. *)

  val cache_dir :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Cmd.Env.info -> unit -> Fpath.t option Cmdliner.Term.t
  (** [cache_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 cache directory.
      {ul
      {- [opts] are the cli options to specify it, default to
         [["cache-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_options}}
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
    ?env:Cmdliner.Cmd.Env.info -> unit -> Fpath.t option Cmdliner.Term.t
  (** [log_file ~doc_none ~docs ~doc ~env] is a cli interface for
      specifing a b0 log file.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["log-file"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_options}}
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
    ?env:Cmdliner.Cmd.Env.info -> unit -> int option Cmdliner.Term.t
  (** [jobs] is a cli interface for specifying the maximal number of
      commands to spawn concurrently.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["j";"jobs"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_options}}
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
    ?env:Cmdliner.Cmd.Env.info -> unit ->
    (module B0_hash.T) option Cmdliner.Term.t
  (** [hash_fun] is a cli interface for specfiying hash function
      used for caching.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["hash-fun"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!hash_fun_env}.}} *)

  val get_hash_fun : hash_fun:(module B0_hash.T) option -> (module B0_hash.T)
  (** [get_hash_fun ~hash_fun] determines a hash function. If [hash_fun]
      is [None] then {!B0_std.Hash} is used. *)

  (** {1:logs Logs} *)

  (** {!B0_memo.Log} *)
  module Log : sig

    (** {1:fmt Log formatters} *)

    val pp_stats : hashed_size:bool -> Op.query -> B0_memo_log.t Fmt.t
    (** [pp_stats sel] formats statistics stored in the log using
        [query] to select operations that are part of the statistics.
        If [hashed_size] the sum of the size of the files in {!file_hashes}
        is computed (this accesses the file system in a non-fatal way
        in case of errors). *)

    type format =
    [ `Hashed_files | `Op_hashes | `Ops | `Path | `Stats | `Root_hashed_files
    | `Trace_event | `Diagnosis ]
    (** The type for output format. *)

    val out :
      Format.formatter -> format -> B0_std_cli.output_details -> Op.query ->
      path:Fpath.t -> B0_memo_log.t -> unit
    (** [out] formats a log on the given formatter. [path] is used when
        [`Path] is requested. *)

    (** {1:cli Command line interface} *)

    val format_cli : ?docs:string -> unit -> format Cmdliner.Term.t
    (** [out_format_cli ~docs ()] are mutually exclusive options to specify
        alternate output formats. [docs] is the manual section in which
        options are documented, defaults to {!s_output_format_options} *)

    val s_output_format_options : Cmdliner.Manpage.section_name
    (** [s_output_format_options] is ["OUTPUT FORMAT OPTIONS"]. *)
  end
end
