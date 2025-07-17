(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line interface fragments and logic. *)

open B0_std

(** {!B0_hash} interaction.

    Ideally this would be in {!B0_std_cli} since this is where
    {!B0_hash} is. But since we vendor the former from [more].
    We have that here. *)
module Hash : sig

  val get_hash_fun : hash_fun:(module B0_hash.T) option -> (module B0_hash.T)
  (** [get_hash_fun ~hash_func] determines a hash function. If [hash_fun]
      is [None] then {!B0_std.Hash} is used. *)

  val hash_fun :
  ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
  ?env:Cmdliner.Cmd.Env.info -> unit ->
  (module B0_hash.T) option Cmdliner.Term.t
  (** [hash_func] is a cli interface for specfiying hash function used for
      caching.

      {ul
      {- [opts] are the cli options to specify it, defaults to [["hash-fun"]].}
      {- [docs] is where the option is documented, defaults to
       {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
       evaluates to [None].}
      {- [env] is a variable that can be used to override the default
       value, defaults to {!hash_fun_env}.}} *)

  val hash_fun_var : Cmdliner.Cmd.Env.info
  (** [hash_fun_var] is ["B0_HASH_FUN"]. *)
end

(** {!B0_zero.File_cache} interaction. *)
module File_cache : sig

  (** {1:high-level High-level commands implementations}

      These commands act on a cache directory. They avoid to create
      it via {!B0_zero.File_cache.make} if it doesn't exists and return
      [Ok false] in that case. *)

  type keyset = String.Set.t
  (** The type for sets of {!B0_zero.Filecache.key}. *)

  type key_kind = [ `Any | `Used | `Unused ]
  (** The type for key kinds. *)

  val keys_of_success_ops : ?init:keyset -> B0_zero.Op.t list -> keyset
  (** [keys_of_success_ops ~init ops] adds to [init] the non-nil hashes of
      the operations of [ops] that are {!B0_zero.Op.Success}. [init]
      defaults to {!String.Set.empty}. *)

  val delete :
    dir:Fpath.t -> used:keyset -> kind:key_kind ->
    [ `All | `Keys of B0_zero.File_cache.key list ] -> (bool, string) result
  (** [delete ~dir ~used ~kind keys] deletes [keys] in [dir] if an
      explicit key does not exist in [dir] or [unused] (depending on
      [kind]) a {!Log.warn} is issued. If [`All] and [`Any] is
      specified [dir] is deleted and recreated. *)

  val gc : dry_run:bool -> dir:Fpath.t -> used:keyset -> (bool, string) result
  (** [gc ~dry_run ~dir ~used] deletes keys that are not in [used].
      If [dry_run] is [true] outputs deltions on [stdout] rather than
      performing them. *)

  val keys :
    dir:Fpath.t -> used:keyset -> kind:key_kind -> (bool, string) result
  (** [keys ~dir ~used ~kind] lists the file cache keys on [stdout] using
      keyed againt [used] in the [`Used] and [`Unused] case. The argument is
      ignored on [`All]. *)

  val stats : dir:Fpath.t -> used:keyset -> (bool, string) result
  (** [status ~dir ~used] outputs statistics about the file cache on [stdout].
      [used] determines keys that are in use. *)

  val trim :
    dry_run:bool -> dir:Fpath.t -> used:keyset -> max_byte_size:int ->
    pct:int -> (bool, string) result
  (** [trim dir ~used ~max_byte_size ~pct] trims the cache using
      {!B0_zero.File_cache.trim_size}. [used] determines keys that
      are assumed to be used. If [dry_run] is [true] outputs deletions
      on [stdout] rather than performing them. *)

  (** {1:cli Cli fragments} *)

  (** {2:directory File cache directoy} *)

  val dir :
    ?opts:string list -> ?docs:Cmdliner.Manpage.section_name -> ?doc:string ->
    ?doc_absent:string -> ?env:Cmdliner.Cmd.Env.info -> unit ->
    Fpath.t option Cmdliner.Term.t
  (** [dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a file cache directory.
      {ul
      {- [opts] are the cli options to specify it, default to
         [["b0-cache-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_absent] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!dir_var}.}} *)

  val dir_var : Cmdliner.Cmd.Env.info
  (** [b0_dir_env] is ["B0_CACHE_DIR"]. *)

  val dirname : string
  (** [dirname] is [".cache"], a sugggested cache directory name. *)

  (** {2:ops File cache operations} *)

  val key_arg : B0_zero.File_cache.key Cmdliner.Arg.conv
  (** [key_arg] is an argument converter for cache keys. *)

  val keys_none_is_all :
    ?first:int -> unit ->
    [ `All | `Keys of B0_zero.File_cache.key list ] Cmdliner.Term.t
  (** [keys_none_is_all ~first ()] are the keys string at position [first]
      (defaults is [0]). If none is specified this is [`All]. *)

  val trim_cli :
    ?mb_opts:string list ->
    ?pct_opts:string list ->
    ?docs:Cmdliner.Manpage.section_name -> unit -> (int * int) Cmdliner.Term.t
  (** [trim_cli ~docs ()] are options to specify a maximal
      byte size and percentage to give to {!trim}. *)

  val key_kind_cli :
    ?docs:Cmdliner.Manpage.section_name -> unit -> key_kind Cmdliner.Term.t
  (** [key_kind_cli ()] are options to specify kinds of keys. *)

  val dry_run :
    ?docs:Cmdliner.Manpage.section_name -> unit -> bool Cmdliner.Term.t
  (** [dry_run ()] is a [--dry-run] option of {!trim} and {!gc}. *)

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

  val query_cli : ?docs:string -> unit -> query Cmdliner.Term.t
  (** [query_cli ~docs ()] is a command line interface to select
      build operations. [docs] is where the option are documented, defaults to
      {!Cmdliner.Manpage.s_options} *)

  val s_selection_options : Cmdliner.Manpage.section_name
  (** [s_selection_options] is a section name for build operation selection
      options. *)

  val query_man : Cmdliner.Manpage.block list
  (** [query_man] is a manual fragment for {!query_cli}. *)
end

(** {!B0_memo_log} interaction *)
module Log : sig

  (** {1:fmt Log formatters} *)

  val pp_stats :
    hashed_size:bool -> Op.query -> B0_memo_log.t Fmt.t
  (** [pp_stats sel] formats statistics stored in the log using
      [query] to select operations that are part of the statistics.
      If [hashed_size] the sum of the size of the files in {!file_hashes}
      is computed (this accesses the file system in a non-fatal way
      in case of errors). *)

  type format =
  [ `Hashed_files
  | `Op_hashes
  | `Ops
  | `Path
  | `Stats
  | `Root_hashed_files
  | `Trace_event
  | `Diagnosis ]
  (** The type for output format. *)

  val pp :
    ?sep:unit B0_std.Fmt.t -> format:format ->
    output_details:B0_std_cli.output_details -> query:Op.query ->
    path:Fpath.t -> unit -> B0_memo_log.t Fmt.t
  (** [pp ~format ~output_details quer ~path] formats a log as follows:
      {ul
      {- [format] specifies how the log is rendered.}
      {- [output_details] specifies how much details should be rendered.}
      {- [query] indicates which operations of the log must be formatted.}
      {- [path] is used when [`Path] is requested (XXX what does that mean ?)}
      {- [sep] is formatted at the end iff something is formated.
         Defaults to {!B0_std.Fmt.cut}.}}
  *)

  val format_cli : ?docs:string -> unit -> format Cmdliner.Term.t
  (** [out_format_cli ~docs ()] are mutually exclusive options to specify
      alternate output formats. [docs] is the manual section in which
      options are documented, defaults to {!s_output_format_options} *)

  val s_output_format_options : Cmdliner.Manpage.section_name
  (** [s_output_format_options] is ["OUTPUT FORMAT OPTIONS"]. *)

  (** {1:file Log file} *)

  val filename : string
  (** [filename] is ["_log"] the default log file name in the [b0]
      directory. *)

  val file :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_absent:string ->
    ?env:Cmdliner.Cmd.Env.info -> unit -> Fpath.t option Cmdliner.Term.t
  (** [file ~doc_none ~docs ~doc ~env] is a cli interface for
      specifing a memo log file.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["log-file"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_absent] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!cache_dir_env}.}} *)

  val file_var : Cmdliner.Cmd.Env.info
  (** [file_env] is ["B0_LOG_FILE"]. *)
end

(** {1:memo_feedback Memo feedback} *)

val pp_op_howto : B0_zero.Op.t Fmt.t
(** [pp_op_howto] formats instructions on how to get more information
    about an operation. This is ["b0-log --id %d"]. *)

val pp_leveled_feedback :
  ?sep:unit Fmt.t -> ?op_howto:B0_zero.Op.t Fmt.t ->
  output_op_level:B0_std.Log.level -> output_ui_level:B0_std.Log.level ->
  level:B0_std.Log.level ->
  [B0_memo.feedback | B0_zero.Exec.feedback] Fmt.t
(** [pp_leveled_feedback ~sep ~op_howto ~output_op_level ~output_ui_level ~level
     ppf] is formatter for memo feedback:
    {ul
    {- [level] indicates the wanted feedback level, this is typically
       the current {!B0_std.Log.level}. The following
       levels are treated specially (other arguments can be ignored):
       {ul
       {- {!B0_std.Log.Quiet} formats nothing.}
       {- {!B0_std.Log.Debug} report all completed operations with
          {!B0_zero_conv.Op.pp_line_and_ui}.}}}
    {- If [level >= output_op_level] any completed operation gets
       logged with {!B0_zero_conv.Op.pp_line_and_ui}}
    {- If [level >= output_ui_ui] any completed operation gets
       logged with {!B0_zero_conv.Op.pp_ui}}
    {- [sep] is formatted at the end iff something is formated.
       Defaults to {!B0_std.Fmt.cut}.}}

    The formatter [op_howto] should format a way to got more information
    about an operation, default to {!pp_op_howto}.

    {b TODO.} It's a bit unclear what "ui" is here. *)

(** {1:memo Memo parameters} *)

val trash_dirname : string
(** [trash_dirname] is [".trash"], a suggested trash directoy name. *)

(** {2:jobs Jobs} *)

val get_jobs : jobs:int option -> int
(** [get_jobs ~jobs] determines a maximal number of spawns. If jobs
    is [None] then {!B0_std.Os.Cpu.logical_count} is used. *)

val jobs :
  ?opts:string list ->  ?docs:string -> ?doc:string -> ?doc_none:string ->
  ?env:Cmdliner.Cmd.Env.info -> unit -> int option Cmdliner.Term.t
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
       value, defaults to {!jobs_var}.}} *)

val jobs_var : Cmdliner.Cmd.Env.info
(** [jobs_env] is ["B0_JOBS"]. *)
