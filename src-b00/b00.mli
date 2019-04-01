(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build kernel

    {b WARNING.} This is an unstable API use at your own risk.

    Concepts kept:
    {ul
    {- Treat build operations as pure functions that affect the file system
       and memoize their results.}
    {- Parallelize build operations by synchronizing on the files they
       read and writes.}
    {- Tight control over tool lookup and build environment.}}
    Concepts dropped and to be seen how we can recover them at a higher level:
    {ul
    {- Build aims for cross compilation (build/host OS distinction). This
       should be capturable as two separate {!Memo}s.}
    {- Build units (named sets of build operation with metadata)}
    {- Build operation synchronisation, only files for now, we had
       unit-level. Did introduce {!Memo.Fut} for odig.}
    {- Build configuration.}
    {- Build unit/config definition localisation (multiple B0.ml files)}
    {- Build directory structuring and forced clean builds}
    {- Build metadata, unit and file level, packages.}}
    {b TODO.}
    {ul
    {- The concept of response file in {!Tool} should likely appear at
       the {!Op.spawn} level aswell so that we can simply have the sequence
       of {!Op.t} value for a potential build log (otherwise {!Op.spawn_args}
       becomes the line with the response file which is not meanigfull)}
    {- Operations, b0 also had file deletion and copy maybe add them
       aswell. Copy can be expressed in terms of read/write but maybe
       better to have as a primitive for concurency. Delete doesn't
       fit well in the "file ready" model, but could be useful for
       concurrency.}
    {- Spawn redirection should maybe create the paths of the file
       they write to.}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:b00 B00} *)

open B0_std

(** File caches.

    A file cache maps a key to a metadata hunk and an ordered list of
    file {e contents} (filenames are irrelevant).

    File caches are used to capture the contents of files written by
    build operations and store their additional output in the
    metadata. This allows to recreate the effect of a build operation
    without having to rerun it.

    {b FIXME.} The following notions use the file system information
    and can be inaccurate. It's a bit unclear whether it's a good idea
    to rely on them. They are only used for cache {{!trim_size}trimming}.
    {ul
    {- The {e access time} of a key is the greatest access time to the
       file contents or metadata hunk it maps to.}
    {- A key is {e unused} if all its file contents are not referenced
       outside the cache. Key usage cannot be determined if the key file
       contents and their references do not live on the same file system
       device or if the file system does not support hard links, see
       {!need_copy}. {b FIXME.} Now that we allow keys to map to empty list
       of file contents, unused might not make much sense for trimming.
       Maybe refine the notion to {e unused content}.}} *)
module File_cache : sig

  (** {1:file_cache File caches} *)

  type feedback = [ `File_cache_need_copy of Fpath.t ]
  (** The type for file cache feedback. See {!create}. *)

  val pp_feedback : feedback Fmt.t
  (** [pp_feedback] formats file cache feedback. *)

  type key = string
  (** The type for keys. A key maps to a metadata hunk and an ordered
      list of file contents. The module treats keys as sequence of
      bytes however since they are used as file names they should
      satisfy the {!B0_std.Fpath.is_seg} predicate; this is not
      checked by the module. *)

  type t
  (** The type for file caches. *)

  val create : ?feedback:(feedback -> unit) -> Fpath.t -> (t, string) result
  (** [create ~feedback dir] is a file cache using directory [dir] for
      data storage. The full path to [dir] is created by the call if
      it doesn't exist. [feedback] is invoked {e once} if the cache
      switches to copying mode, see {!need_copy} for details (defaults
      is a nop). *)

  val dir : t -> Fpath.t
  (** [dir c] is [c]'s storage directory. *)

  val need_copy : t -> Fpath.t option
  (** [need_copy c] is [Some file] iff the cache switched to copying
      mode because of an operation that involved the file path [file],
      external to the cache. This can happen due to one of the
      following conditions:
      {ul
      {- The cache directory {!dir} and [file] live on different file
         system devices.}
      {- The underlying file system does not support hard links.}
      {- Too many hard links exist on a file.}}
      [file] is also given to the [notify_need_copy] callback provided
      on {!create} as soon as the condition is detected (and before
      the actual copy occurs). Note that once a file did copy, all the
      remaining transfers from or to the cache do copy aswell, which
      may be slow. *)

  (** {1:ops Cache operations}

      {b Note.} In general, whenever a cache operation modifies the
      file system and errors with [Error _] the resulting file system
      state is undefined. *)

  val mem : t -> key -> bool
  (** [mem c k] is [true] iff [k] is bound in [c]. *)

  val add : t -> key -> string -> Fpath.t list -> (bool, string) result
  (** [add c k m fs], binds the metadata [m] and the {e contents} of
      the ordered list of files [fs] to [k] in [c]. The function
      returns:
      {ul
      {- [Ok true] if the operation succeeds.}
      {- [Ok false] if a file of [fs] could not be accessed. In this
         case [k] is guaranteed to be unbound in [c].}
      {- [Error _] if an unexpected error occurs. In that case the
         resulting state of the cache for key [k] is undefined.}} *)

  val rem : t -> key -> (bool, string) result
  (** [rem c k] removes the binding of [k] in [c]. [Ok true] is
      returned if [k] was bound in [c] and [Ok false] otherwise. *)

  val find : t -> key -> ((Fpath.t * Fpath.t list) option, string) result
  (** [find c k] is [Some (mf, fs)] if [k] is bound in [c] with [mf]
      the file that holds the key metadata and [fs] the files that
      hold the file contents of the key in the order given on
      {!add}. The result is [None] if [k] is unbound in [c]. *)

  val revive :
    t -> key -> Fpath.t list -> ((string * Fpath.t list) option, string) result
  (** [revive c k fs] binds the file contents of key [k] to the inexistent
      file {e path}s (directories are created) of [fs]. The function returns:
      {ul
      {- [Ok (Some (m, existed)] in case of success, with [m] the
         metadata of the key and [existed] the files that already
         existed and were left untouched by the operation. Assuming no
         other process fiddles with [fs] all these file paths now
         exist (but those of [existed] might differ from the
         corresponding one in the cache).}
      {- [Ok None] if the length of [fs] does not match the
         sequence of files of [k] or if [k] is unbound in [c].
         In this case the file paths [fs] are left untouched.}
      {- [Error _] if an unexpected error occurs. In that case the resulting
         state of the file system for paths [fs] is undefined.}} *)

  val fold :
    t -> (key -> Fpath.t -> Fpath.t list -> 'a -> 'a) -> 'a ->
    ('a, string) result
  (** [fold c f acc] folds [f] over the contents of each key of [c]
      starting with [acc]. *)

  val keys : t -> (key list, string) result
  (** [keys c] are the keys of cache [c]. *)

  val is_unused : t -> key -> (bool, string) result
  (** [is_unused c k] is [true] iff [k] is bound in [c] and its
      content is being unused. {b Warning} For caches which are
      referenced across file system device or on file systems that do
      not support hard links this may return [true] even though the
      key is actually in use. *)

  val delete_unused : t -> (unit, string) result
  (** [delete_unused c] deletes {{!is_unused}unused} keys of [c]. *)

  val trim_size : t -> max_byte_size:int -> pct:int -> (unit, string) result
  (** [trim_size c max_byte_size ~pct] delete keys of [c] until they
      either weight at most [max_byte_size] or are [pct] of their
      current size; whichever is the smaller. The function deletes by
      order of increasing access time but unused keys are deleted
      first. *)

  (** {1:stats Cache statistics} *)

  (** Cache statistics. *)
  module Stats : sig

    (** {1:keys Key statistics} *)

    type keys
    (** The type for statistics about a set of keys *)

    val keys_count : keys -> int
    (** [keys_count s] is the number of keys in the set of keys. *)

    val keys_file_count : keys -> int
    (** [keys_file_count s] is the nubmer of files in the set of keys. *)

    val keys_byte_size : keys -> int
    (** [keys_count s] is the total byte size of the files in the set
        of keys. *)

    val keys_zero : keys
    (** [keys_zero] are zeros. *)

    val keys_sub : keys -> keys -> keys
    (** [keys_sub s0 s1] is he field-wise substraction [s0 - s1]. *)

    val pp_keys : keys Fmt.t
    (** [pp_keys] formats key statistics. *)

    val of_keys : t -> key list -> (keys, string) result
    (** [of_keys ks] are statistics for [ks]. *)

    (** {1:cache Cache statistics} *)

    type cache
    (** The type for cache statistics *)

    val all_keys : cache -> keys
    (** [all_keys s] are statistics about all keys in the cache. *)

    val unused_keys : cache -> keys
    (** [unused_keys s] are statistics about unused keys in the cache.
        {b Warning} Only relevant if {!is_unused} is. *)

    val pp : cache Fmt.t
    (** [pp] formats cache statistics. *)

    val of_cache : t -> (cache, string) result
    (** [of_cache c] are satistics of [c]. *)
  end
end

(** Build operations.

    This module only provides a type for specifying operations and the
    result of their execution. Execution and caching is respectively handled
    by the modules {!Exec} and {!Reviver}. *)
module Op : sig

  (** {1:op_status Operation status} *)

  type status =
  | Waiting  (** Waiting for execution. *)
  | Executed (** Executed successfully. *)
  | Failed   (** Executed unsuccessfully. *)
  | Aborted  (** Aborted due to prerequisite failure. *)
  (** The type for operation statuses. *)

  val pp_status : status Fmt.t
  (** [pp_status] formats execution status. *)

  (** {1:op Operations} *)

  type id = int
  (** The type for build operation identifiers. *)

  type t
  (** The type for build operations. *)

  type op = t
  (** Again. *)

  val id : t -> id
  (** [id o] is the identifier of operation [o]. *)

  val creation_time : t -> Time.span
  (** [creation_time o] is [o]'s creation time. *)

  val exec_start_time : t -> Time.span
  (** [exec_start_time o] is [o]'s execution start time. This is
      different from {!B0_std.Time.Span.zero} once the operation has
      been submitted for execution. *)

  val exec_end_time : t -> Time.span
  (** [exec_end_time o] is [o]'s execution end time. This is different
      from {!B0_std.Time.Span.zero} once the operation has been completed
      and collected. *)

  val exec_duration : t -> Time.span
  (** [exec_duration] is the difference between {!exec_end_time} and
      {!exec_start_time}. *)

  val exec_revived : t -> bool
  (** [exec_revived o] is [true] iff [o] was revived from a cache. *)

  val status : t -> status
  (** [status o] is [o] execution status. *)

  val reads : t -> Fpath.t list
  (** [reads o] are the file paths read by the operation. *)

  val writes : t -> Fpath.t list
  (** [writes o] are the file paths written by [o]. *)

  val did_not_write : t -> Fpath.t list
  (** [did_not_write o] compares {!writes} with the current state
      of the file system and reports those files that do not exist on it.  *)

  val hash : t -> Hash.t
  (** [hash o] is the operation's hash. This is {!Hash.nil} before the
      operation hash has been effectively computed and set via
      {!set_hash}. *)

  (** {1:op_kind Operations kinds} *)

  (** Tool spawns. *)
  module Spawn : sig

    (** {1:spawn Tool spawns} *)

    type stdo = [ `Ui | `File of Fpath.t | `Tee of Fpath.t ]
    (** The type for spawn standard outputs redirections.
        {ul
        {- [`Ui] redirects the output to the user interface of the build
           system (usually only shown in case of failure). Outputs are
           always first redirected to a file and read back by the program
           running the build on completion, this means that in the
           spawn program [isatty(3)] will be [false] on the fds.}
        {- [`File p] redirect the output to file path [p].}
        {- [`Tee p], is both [`Ui] and [File `f].}} *)

    type success_exits = int list
    (** The list of process exit codes that indicate success. If the
        list is empty this any exit code. *)

    type t
    (** The type for process spawn operations. *)

    val v :
      id:id -> Time.span -> reads:Fpath.t list -> writes:Fpath.t list ->
      env:Os.Env.assignments -> relevant_env:Os.Env.assignments ->
      cwd:Fpath.t -> stdin:Fpath.t option -> stdout:stdo ->
      stderr:stdo -> success_exits:success_exits ->
      Cmd.tool -> Cmd.t -> op
    (** [spawn] declares a spawn build operation, see the corresponding
        accessors in {!Spawn} for the semantics of the various fields. *)

    val get : op -> t
    (** [get o] is the spawn [o]. @raise Invalid_argument if [o] is
        not a spawn. *)

    val env : t -> Os.Env.assignments
    (** [env s] is the environment in which [s] runs. *)

    val relevant_env : t -> Os.Env.assignments
    (** [relevant_env s] are the assignements of [env s]
        that should be taken into account for caching. *)

    val cwd : t -> Fpath.t
    (** [cwd s] is the cwd with which [s] runs. *)

    val stdin : t -> Fpath.t option
    (** [stdin s] is the file [s] uses as stdin (if any). *)

    val stdout : t -> stdo
    (** [stdout s] is the redirection [s] uses for stdout. *)

    val stderr : t -> stdo
    (** [stderr s] is the redirection [s] uses for stderr. *)

    val success_exits : t -> success_exits
    (** [success_exits s] is the list of process exit codes [s] that
        indicate success. *)

    val tool : t -> Cmd.tool
    (** [tool t] is the spawned tool. Note that this has to be
        a full path at that point. *)

    val args : t -> Cmd.t
    (** [args s] are the spawned tool arguments. *)

    val stdo_ui : t -> (string, string) result option
    (** [stdo_ui sr] is the standard outputs redirection contents
        of [s] once it has executed (if any). *)

    val set_stdo_ui : t -> (string, string) result option -> unit
    (** [set_stdo_ui w ui] sets [w]'s standard output redirection contents
        to [ui]. *)

    val result : t -> (Os.Cmd.status, string) result
    (** [result s] is the spawn result of [s]. *)

    val set_result : t -> (Os.Cmd.status, string) result -> unit
    (** [set_result s e] the spawn result of [s] to [e]. *)

    val set_exec_status :
      op -> t -> Time.span -> (string, string) result option ->
      (Os.Cmd.status, string) result -> unit
    (** [set_exec_status o (get o) end_time stdo_ui result] sets the
        result of operation [o]. In particular this set the operation
        status according to [result]. *)

    (** {1:fmt Formatters} *)

    val pp_success_exits : int list Fmt.t
    (** [pp_success_exits] formats the success exits. *)

    val pp_cmd : t Fmt.t
    (** [pp_cmd] formats the command issued by the spawn. *)

    val pp_stdo_ui : elide:bool -> t Fmt.t
    (** [pp_stdo_ui] formats the standard output ui of the spawn. If
        [elide] is true elides long outputs. *)

    val pp_result : (Os.Cmd.status, string) result Fmt.t
    (** [pp_result] formats the command status of the spawn. *)

    val pp : t Fmt.t
    (** [pp] formats a spawn. *)
  end

  (** File reads. *)
  module Read : sig

    (** {1:read File reads} *)

    type t
    (** The type for file read operations. *)

    val v : id:id -> Time.span -> Fpath.t -> op
    (** [v] declares a file read operation, see the corresponding
        accessors in {!Read} for the semantics of the various fields. *)

    val get : op -> t
    (** [get_read o] is the read [o]. @raise Invalid_argument if [o]
        is not a read. *)

    val file : t -> Fpath.t
    (** [file r] is the file read by [r]. *)

    val result : t -> (string, string) result
    (** [result r] is the contents of the read file or an error. *)

    val set_result : t -> (string, string) result -> unit
    (** [set_result r res] sets the file read result of [r] to [res]. *)

    val set_exec_status :
      op -> t -> Time.span ->  (string, string) result -> unit
    (** [set_exec_status o (get o) end_time result] sets the result of
        operation [o]. In particular this set the operation status
        according to [result]. *)

    (** {1:formatting Formatters} *)

    val pp_result : (string, string) result Fmt.t
    (** [pp_result] formats the read result. *)

    val pp : t Fmt.t
    (** [pp] formats a read. *)
  end

  (** File writes. *)
  module Write : sig

    (** {1:write File writes} *)

    type t
    (** The type for file write operations. *)

    val v :
      id:id -> Time.span -> stamp:string -> reads:Fpath.t list ->
      mode:int -> write:Fpath.t -> (unit -> (string, string) result) -> op
    (** [write] declares a file write operations, see the corresponding
        accessors in {!Write} for the semantics of the various fields. *)

    val get : op -> t
    (** [geo o] is the write [o]. @raise Invalid_argument if [o] is
        not a write. *)

    val stamp : t -> string
    (** [stamp w] is the file write stamp used for caching. *)

    val mode : t -> int
    (** [int w] is the mode of the file written by [w]. *)

    val file : t -> Fpath.t
    (** [file w] is the file written by [w]. *)

    val data : t -> (unit -> (string, string) result)
    (** [data w ()] is the data to write. *)

    val result : t -> (unit, string) result
    (** [result w] is the result of the file write. *)

    val set_result : t -> (unit, string) result -> unit
    (** [set_result w res] sets the write results of [w] to [res]. *)

    val set_exec_status :
      op -> t -> Time.span -> (unit, string) result -> unit
    (** [set_exec_status o (get o) end_time result] sets the result of
        operation [o]. In particular this set the operation status
        according to [result]. *)

    (** {1:fmt Formatters} *)

    val pp_result : (unit, string) result Fmt.t
    (** [pp_result] formats a write result. *)

    val pp : t Fmt.t
    (** [pp] formats a write. *)
  end

  (** Directory creation. *)
  module Mkdir : sig

    (** {1:mkdir Directory creation} *)

    type t
    (** The type for directory creation operations. *)

    val v : id:id -> Time.span -> Fpath.t -> op
    (** [v] declares a directory creation operation, see the
        corresponding accessors for the semantics of the various
        fields. *)

    val get : op -> t
    (** [get o] is the mkdir [o]. @raise Invalid_argument if [o] is not
        a mkdir. *)

    val dir : t -> Fpath.t
    (** [dir mk] is the directory created by [mk]. *)

    val result : t -> (bool, string) result
    (** [result mk] is the result of the directory creation. *)

    val set_result : t -> (bool, string) result -> unit
    (** [set_result r res] sets the mkdir result of [r] to [res]. *)

    val set_exec_status :
      op -> t -> Time.span ->  (bool, string) result -> unit
    (** [set_exec_status o (get o) end_time result] sets the operation
        result of [o]. In particular this set the operation status
        according to [result]. *)

    (** {1:fmt Formatters} *)

    val pp_result : (bool, string) result Fmt.t
    (** [pp_result] formats directory creation results. *)

    val pp : t Fmt.t
    (** [pp] formats directory creations. *)
  end

  module Wait_files : sig
    val v : id:id -> Time.span -> Fpath.t list -> t
    (** [v] declares a wait files operation, these are stored in
        {!reads}. *)
  end

  type kind =
  | Spawn of Spawn.t
  | Read of Read.t
  | Write of Write.t
  | Mkdir of Mkdir.t
  | Wait_files (** *)
  (** The type for operation kinds. *)

  val kind_name : kind -> string
  (** [kind_name k] is a end user name for kind [k]. *)

  val kind : t -> kind
  (** [kind o] is [o]'s kind. *)

  val equal : t -> t -> bool
  (** [equal o0 o1] is [id o0 = id o1]. *)

  val compare : t -> t -> int
  (** [compare o0 o1] is [Pervasives.compare (id o0) (id o1)]. *)

  (** {1:fmt Formatters} *)

  val pp : t Fmt.t
  (** [pp] formats a build operation. *)

  val pp_short : t Fmt.t
  (** [pp_short] formats a build operation on a single line. *)

  val pp_did_not_write : (t * Fpath.t list) Fmt.t
  (** [pp_did_not_write] formats a build operation and the files
      it failed to write. *)

  val pp_spawn_status_fail : t Fmt.t
  (** [pp_spawn_status_fail] formats a spawn operation failure due to
      exit result. *)

  (** {1:upd Updating the build operation} *)

  val set_exec_start_time : t -> Time.span -> unit
  (** [exec_start_time o t] sets [o]'s execution start time to [t]. *)

  val set_exec_end_time : t -> Time.span -> unit
  (** [set_exec_end_time o t] sets [o]'s execution end time to [s]. *)

  val set_exec_revived : t -> bool -> unit
  (** [set_exec_revived o b] sets [o]'s cache revival status to [b]. *)

  val set_status : t -> status -> unit
  (** [set_status o s] sets the execution status to [s]. *)

  val set_reads : t -> Fpath.t list -> unit
  (** [set_reads t fs] sets the file paths read by [o] to [fs].
      Note that this resets the {!hash}. *)

  val set_writes : t -> Fpath.t list -> unit
  (** [set_writes t fs] sets the file paths written by [o] to [fs]. *)

  val set_hash : t -> Hash.t -> unit
  (** [set_hash o h] sets the operation hash to [h]. *)

  (** {1:set_map Operation sets and map} *)

  (** Operation sets *)
  module Set : Set.S with type elt := t

  (** Operation maps *)
  module Map : Map.S with type key := t
end

(** Build operation revivers.

    An operation reviver combines a {{!File_cache}file cache} and a
    {{!B0_std.Hash.T}hash function} to record and revive the effect of
    {{!Op}build operations}.

    {b Note.} Hashes performed on files by this module are
    {{!file_hashes}cached}. *)
module Reviver : sig

  (** {1:reviver Operation reviver} *)

  type t
  (** The type for build operation revivers. *)

  val create : Time.counter -> (module Hash.T) -> File_cache.t -> t
  (** [create clock hash_fun cache] is a reviver with
      {ul
      {- [clock] the clock used to {{!file_hash_dur}measure} file hashing
         time and {{!Op.set_exec_start_time}timestamp} revived operations.}
      {- [hash_fun] the hash function used to hash files and build operations.}
      {- [cache] the file cache used to record build operations.}} *)

  val clock : t -> Time.counter
  (** [clock r] is [r]'s clock. *)

  val hash_fun : t -> (module Hash.T)
  (** [hash_fun r] is [r]'s hash function. *)

  val file_cache : t -> File_cache.t
  (** [file_cache r] is [r]'s file cache. *)

  (** {1:hashing Hashing} *)

  val hash_op : t -> Op.t -> (Hash.t, string) result
  (** [hash_op r o] hashes the operation [o]. Errors if an input
      file of the build operation can't be hashed. *)

  val hash_file : t -> Fpath.t -> (Hash.t, string) result
  (** [hash_file r f] hashes file [f]. Note that file hashes
      are {{!file_hashes}cached} by [r]. *)

  val file_hashes : t -> Hash.t Fpath.Map.t
  (** [file_hashes r] is a map of the files that were hashed. *)

  val file_hash_dur : t -> Time.span
  (** [file_hash_dur r] is the time spent hashing files. *)

  (** {1:record_and_revive Recording and reviving operations} *)

  val record : t -> Op.t -> (bool, string) result
  (** [record r o] records operation [o] in the reviver [r]. This
      associates the [Op.writes o] of [o] to the key [Op.hash o]
      (i.e. this function assume [o] has gone through {!set_op_hash}
      before) and stores operation output information of [o] in the
      key's metadata hunk. The semantics of the result is like
      {!File_cache.add}; in particular in case of [Ok false] it means
      some file in the set of writes do not exist and is likely an
      error. *)

  val revive : t -> Op.t -> (Fpath.t list option, string) result
  (** [revive r o] tries to revive operation [o] from [r] using the key
      [Op.hash o] (i.e. this function assume [o] has gone through
      {!set_op_hash} before). In particular:
      {ol
      {- Recreates the files [Op.writes o]}
      {- Sets [o]'s execution information using the metadata hunk
         of the key. For example for spawn operations this also recovers the
         {{!Op.Spawn.exit}exit status} code and
         {{!Op.Spawn.stdo_ui}standard output redirection contents} into
         [o].}}
      The semantics of the result is like
      {!File_cache.revive}; in particular in case of [Ok None] the key
      nothing was revived.

      {b Warning.} In any case the fields {!Op.exec_start_time},
      {!Op.exec_end_time} of [o] get set. *)
end

(** Build operation guards.

    A guard ensure that a build operation is allowed to proceed.
    This means either that:
    {ul
    {- It is {e ready} and can be submitted for execution. This
       happens once all the files the operation {{!Op.reads}reads} are
       {{!set_file_ready}ready}: they exist and are up-to-date.}
    {- It is {e aborted}. This happens if a file it needs to
       read {{!set_file_never}never} becomes ready.}} *)
module Guard : sig

  (** {1:guards Guards} *)

  type feedback =
  [ `File_status_repeat of Fpath.t
  | `File_status_unstable of Fpath.t ]
  (** The type for guard feedbacks:
      {ul
      {- [`File_status_repeat f] indicates that the file status of [f]
         was set more than once.}
      {- [`File_status_unstable f] indicates that the file status of [f]
         was set more than once and in an inconsistent manner.}} *)

  type t
  (** The type for build operations guards. *)

  val create : ?feedback:(feedback -> unit) -> unit -> t
  (** [create ~feedback ()] is a new guard, using [feedback] to
      report inconsistencies (default is a nop.). *)

  val set_file_ready : t -> Fpath.t -> unit
  (** [set_file_ready g f] indicates to [g] that file [f] is ready,
      i.e. that it exists and is up-to-date. *)

  val set_file_never : t -> Fpath.t -> unit
  (** [set_file_never g f] indicate to [g] that file [f] will never
      become ready. *)

  val add : t -> Op.t -> unit
  (** [add g o] guards [o] in [g] until it is {{!allowed}allowed}. *)

  val allowed : t -> Op.t option
  (** [allowed g] is an operation that is either ready or aborted
      in [g] (if any). In the second case the {!Op.exec_status} is
      [Op.Aborted]. *)

  (** {1:stuck Stuck build analysis}

      The following functions are not efficient, only use for stuck
      build anaylsis or debugging. *)

  val guarded_ops : t -> Op.t list
  (** [guarded_opts g] is the list of operations that are not ready in [g]. *)

  val ready_files : t -> Fpath.Set.t
  (** [ready_files g] are the files that got ready in [g]. *)

  val never_files : t -> Fpath.Set.t
  (** [never_files g] are the files that never got ready in [g]. *)

  val undecided_files : t -> Fpath.Set.t
  (** [undecided_files g] are the files that are neither got ready nor
      never got ready in [g]. *)

  val root_undecided_files : t -> Fpath.Set.t
  (** [root_undecided_file g] is like {!undecided_files} but has only
      the files that are not written by a {!guarded_op} of [g]. If a
      build is stuck these are files that are not undecided as the result
      of a guarded operation. *)
end

(** Build operation executors.

    An executor is a parallel asynchronous work queue. It has no
    notion of synchronisation, any scheduled operation is randomly
    executed in parallel up to the executor's parallelizing limits. *)
module Exec : sig

  (** {1:execs Executors} *)

  type feedback = [ `Exec_submit of Os.Cmd.pid option * Op.t ]
  (** The type for executor feedbacks. *)

  val pp_feedback : feedback Fmt.t
  (** [pp_feedback] formats executor feedback. *)

  type t
  (** The type for executors. *)

  val create :
    ?clock:Time.counter -> ?rand:Random.State.t -> ?tmp_dir:Fpath.t ->
    ?feedback:(feedback -> unit) -> max_spawn:int -> unit -> t
  (** [create ~clock ~rand ~tmp_dir ~notify_submit ~max_spawn] with:
      {ul
      {- [max_spawn] the maximal number of processes spawn simultaneously.}
      {- [feedback] called with each {{!schedule}scheduled} operation
         when it gets submitted for execution. Default is a nop.}
      {- [tmp_dir] is a directory for temporary files,
          it must exist; defaults to {!B0_std.Os.Dir.default_tmp}[ ()].}
      {- [rand] random state used for internal queues; defaults to
         {!Random.State.make_self_init}.}
      {- [clock], the clock used to {{!Op.set_exec_start_time}timestamp}
         the operations; defaults to {!B0_std.Time.counter}[ ()].}} *)

  val clock : t -> Time.counter
  (** [clock e] is [e]'s clock. *)

  val tmp_dir : t -> Fpath.t
  (** [tmp_dir e] is [e]'s temporary directory. *)

  val max_spawn : t -> int
  (** [max_spawn e] is [e]'s maximal number of simultaneous process spawns. *)

  (** {1:schedcollect Scheduling and collecting operations} *)

  val schedule : t -> Op.t -> unit
  (** [schedule e o] schedules [o] for execution in [e]. Just before
      [o] is actually submitted for execution it is given to the
      [notify_submit] callback of [e] (see {!create}). *)

  val collect : t -> block:bool -> Op.t option
  (** [collect e ~block] removes from [e] an operation that has
      completed (if any). If [block] is [false] and no completed
      operation exists, the call returns immediately with [None]. If
      [block] is [true] and at least one incomplete operation exists
      in [e], the call blocks until an operation completes. If [block]
      is [true] and no operation exists in [e] [None] is returned. *)
end

(** {1:b01 B01} *)

(** Build environment.

    Build environments control tool lookup and the environment
    of tool spawns. *)
module Env : sig

  (** {1:lookup Tool lookup} *)

  type tool_lookup = Cmd.tool -> (Fpath.t, string) result
  (** The type for tool lookups. Given a command line tool
      {{!type:B0_std.Cmd.tool}specification} returns a file path to
      the tool executable or an error message mentioning the tool if
      it cannot be found. *)

  val env_tool_lookup :
    ?sep:string -> ?var:string -> Os.Env.t -> tool_lookup
  (** [env_tool_lookup ~sep ~var env] is a tool lookup that gets the
      value of the [var] variable in [env] treats it as a [sep]
      separated {{!B0_std.Fpath.list_of_search_path}search path} and
      uses the result to lookup with {!B0_std.Os.Cmd.must_find}.
      [var] defaults to [PATH] and [sep] to
      {!B0_std.Fpath.search_path_sep}. *)

  (** {1:env Environment} *)

  type t
  (** The type for build environments. *)

  val v : ?lookup:tool_lookup -> ?forced_env:Os.Env.t -> Os.Env.t -> t
  (** [v ~lookup ~forced_env env] is a build environment with:
      {ul
      {- [lookup] used to find build tools. Defaults to
         [env_tool_lookup env].}
      {- [forced_env] is environment forced on any tool despite
         what it declared to access, defaults to {!Os.Env.empty}}
      {- [env] the environment read by the tools' declared environment
         variables.}} *)

  val env : t -> Os.Env.t
  (** [env e] is [e]'s available spawn environment. *)

  val forced_env : t -> Os.Env.t
  (** [forced_env e] is [e]'s forced spawn environment. *)

  val tool : t -> Cmd.tool -> (Fpath.t, string) result
  (** [tool e t] looks up tool [t] in [e]. *)
end

(** Command line tools.

    A tool is specified either by name, to be looked up via an
    unspecified mecanism, or by a file path to an executable file. It
    also declares the environment variables it accesses in the process
    environment and whether and how it supports response files.

    Declared environment variables are split into relevant and
    shielded variables. A relevant variable is a variable whose
    value influences the tool's output. A shielded variable is a
    variable whose value does not influence the tool's output but is
    nonetheless essential to its operation. Shielded environment
    variables do not appear in the spawn environment signature which
    is used to memoize tool spawns. Variables specifying the location
    of {{!tmp_vars}temporary file directories} are good examples of
    shielded variables.

    {b Portability.} In order to maximize portability no [.exe]
    suffix should be added to executable names on Windows, the
    search procedure will add the suffix during the tool search
    procedure if absent. *)
module Tool : sig

  (** {1:env Environment variables} *)

  type env_vars = string list
  (** The type for lists of environment variable names. *)

  val tmp_vars : env_vars
  (** [tmp_vars] is [["TMPDIR"; "TEMP"; "TMP"]]. *)

  (** {1:resp Response files} *)

  type response_file
  (** The type for response file specification. *)

  val response_file_of :
    (Cmd.t -> string) -> (Fpath.t -> Cmd.t) -> response_file
  (** [response_file_of to_file cli] is a response file specification
      that uses [to_file cmd] to convert the command line [cmd] to a
      response file content and [cli f] a command line fragment to be
      given to the tool so that it treats file [f] as a response
      file. *)

  val args0 : response_file
  (** [args0] is response file support for tools that reads null byte
      ([0x00]) terminated arguments response files via an [-args0
      FILE] command line synopsis. *)

  (** {1:tools Tools} *)

  type t
  (** The type for tools. *)

  val v :
    ?response_file:response_file -> ?shielded_vars:env_vars ->
    ?vars:env_vars -> Cmd.tool -> t
  (** [v ~response_file ~shielded_vars ~vars cmd] is a tool specified
      by [cmd]. [vars] are the relevant variables accessed by the
      tool (defaults to [[]]). [shielded_vars] are the shielded
      variables accessed by the tool (defaults to {!tmp_vars}).
      [response_file] defines the reponse file support for the tool
      (if any). *)

  val by_name :
    ?response_file:response_file -> ?shielded_vars:env_vars ->
    ?vars:env_vars -> string -> t
  (** [by_name] is like {!v} but reference the tool directly via a name.

      @raise Invalid_argument if {!Fpath.is_seg} [name] is [false]. *)

  val name : t -> Cmd.tool
  (** [name t] is [t]'s tool name. If this is a relative file path
      with a single segment the tool is meant to be searched via an
      external mecanism. *)

  val vars : t -> env_vars
  (** [vars t] are the relevant environment variables accessed by [t]. *)

  val shielded_vars : t -> env_vars
  (** [shieled_vars t] are the shielded environment variables
      accessed by [t]. *)

  val response_file : t -> response_file option
  (** [response_file t] is [t]'s response file specification (if any). *)

  val read_env : t -> Os.Env.t -> Os.Env.t * Os.Env.t
  (** [read_env t env] is (all, relevant) with [all] the
      environment with the variables of [env] that are in [vars t]
      and [shielded_vars t] and [relevant] those of [vars t] only. *)
end

(** Build memoizer.

    A memoizer ties together and environment, an operation cache, a guard
    and an executor. *)
module Memo : sig

  (** {1:memo Memoizer} *)

  type feedback =
  [ `Fiber_exn of exn * Printexc.raw_backtrace
  | `Fiber_fail of string
  | `Miss_tool of Tool.t * string
  | `Op_cache_error of Op.t * string
  | `Op_complete of Op.t * [`Did_not_write of Fpath.t list] ]
  (** The type for memoizer feedback. *)

  val pp_feedback : feedback Fmt.t
  (** [pp_feedback ppf] formats feedback. *)

  type t
  (** The type for memoizers. This ties together an environment, a
      aguard, an operation cache and an executor. *)

  val create :
    ?clock:Time.counter ->
    ?cpu_clock:Time.cpu_counter ->
    feedback:(feedback -> unit) ->
    cwd:Fpath.t ->
    Env.t -> Guard.t -> Reviver.t -> Exec.t -> t

  val memo :
    ?hash_fun:(module B0_std.Hash.T) ->
    ?env:B0_std.Os.Env.t ->
    ?cwd:B0_std.Fpath.t ->
    ?cachedir:B0_std.Fpath.t ->
    ?max_spawn:int ->
    ?feedback:([feedback | File_cache.feedback | Exec.feedback] -> unit) ->
    unit -> (t, string) result
  (** [memo] is a simpler {!create}
      {ul
      {- [hash_fun] defaults to {!Op_cache.create}'s default.}
      {- [max_spawn] defaults to {!Exec.create}'s default.}
      {- [env] defaults to {!Os.Env.current}}
      {- [cwd] defaults to {!Os.Dir.cwd}}
      {- [cachedir] defaults to [Fpath.(cwd / "_b0" / "cache")]}
      {- [feedback] defaults formats feedback on stdout.}} *)

  val clock : t -> Time.counter
  (** [clock m] is [m]'s clock. *)

  val cpu_clock : t -> Time.cpu_counter
  (** [cpu_clock m] is [m]'s cpu clock. *)

  val env : t -> Env.t
  (** [env m] is [m]'s environment. *)

  val reviver : t -> Reviver.t
  (** [reviver m] is [m]'s reviver. *)

  val guard : t -> Guard.t
  (** [guard m] is [m]'s guard. *)

  val exec : t -> Exec.t
  (** [exec m] is [m]'s executors. *)

  val hash_fun : t -> (module B0_std.Hash.T)
  (** [hash_fun m] is [m]'s hash function. *)

  val stir : block:bool -> t -> unit
  (** [stir ~block m] runs the memoizer a bit. If [block] is [true]
      blocks until the memoizer is stuck with no operation to
      perform. *)

  val finish : t -> (unit, Fpath.Set.t) result
  (** [finish m] finishes the memoizer. This blocks until there are no
      operation to execute like {!stir} does. If no operations are
      left waiting this returns [Ok ()]. If there are remaining
      wiating operations it aborts them and returns [Error fs] with
      [fs] the files that never became ready and where not supposed to
      be written by the waiting operations. *)

  val ops : t -> Op.t list
  (** [ops m] is the list of operations that were submitted to the
      memoizer *)

  (** {1:fibers Fibers} *)

  type 'a fiber = ('a -> unit) -> unit
  (** The type for memoizer operation fibers. *)

  val fail : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [fail fmt ...] fails the fiber with the given error message. *)

  val fail_error : ('a, string) result -> 'a
  (** [fail_error] fails the fiber with the given error. *)

  (** {1:files Files and directories} *)

  val file_ready : t -> Fpath.t -> unit
  (** [ready m p] declares path [p] to be ready, that is exists and is
      up-to-date in [b]. This is typically used with source files
      and files external to the build (e.g. installed libraries). *)

  val wait_files : t -> Fpath.t list -> unit fiber
  (** [wait_files m files k] continues with [k ()] when [files]
      become ready. {b FIXME} Unclear whether
      we really want this though this is kind of a [reads] constraint
      for a pure OCaml operation, but then we got {!read}. *)

  val read : t -> Fpath.t -> string fiber
  (** [read m file k] reads the contents of file [file] as [s] when it
      becomes ready and continues with [k s]. *)

  val write :
    t -> ?stamp:string -> ?reads:Fpath.t list -> ?mode:int -> Fpath.t ->
    (unit -> (string, string) result) -> unit
  (** [write m ~reads file w] writes [file] with data [w ()] and mode
      [mode] (defaults to [0o644]) when [reads] are ready. [w]'s
      result must only depend on [reads] and [stamp] (defaults to
      [""]). *)

  val mkdir : t -> Fpath.t -> bool fiber
  (** [mkdir m dir k] creates directory [dir] and continues with [k created] at
      which point file [dir] is ready and [created] indicates if the
      directory was created by the operation. *)

  (** {1:spawn Memoizing tool spawns} *)

  type tool
  (** The type for memoized tools. *)

  type cmd
  (** The type for memoized tool invocations. *)

  val tool : t -> Tool.t -> (Cmd.t -> cmd)
  (** [tool m t] is tool [t] memoized. Use the resulting function
      to spawn the tool with the given arguments. *)

  val spawn :
    t -> ?reads:Fpath.t list -> ?writes:Fpath.t list -> ?env:Os.Env.t ->
    ?cwd:Fpath.t -> ?stdin:Fpath.t -> ?stdout:Op.Spawn.stdo ->
    ?stderr:Op.Spawn.stdo -> ?success_exits:Op.Spawn.success_exits ->
    ?k:(int -> unit) -> cmd -> unit
  (** [spawn m ~reads ~writes ~env ~cwd ~stdin ~stdout ~stderr
      ~success_exits cmd] spawns [cmd] once [reads] files are ready
      and makes files [writes] ready if the spawn succeeds and the
      file exists. The rest of the arguments are:
      {ul
      {- [stdin] reads input from the given file. If unspecified reads
         from the standard input of the program running the build.  {b
         Warning.} The file is not automatically added to [reads],
         this allows for example to use {!Os.File.null}.}
      {- [stdout] and [stderr], the redirections for the standard
         outputs of the command, see {!stdo}. {b Warning.} File redirections
         are not automatically added to [writes]; this allows for example
         to use {!Os.File.null}.}
      {- [success_exits] the exit codes that determine if the build operation
         is successful (defaults to [0], use [[]] to always succeed)}
      {- [env], environment variables added to the build environment.
         This overrides environment variables read by the tool in the
         build environment except for forced one. It also allows to
         specify environment that may not be mentioned by the running
         tool's {{!Tool.v}environment specification}.}
      {- [cwd] the current working directory. Default is {!cwd}. In
         general it's better to avoid using relative file paths and
         tweaking the [cwd]. Construct your paths using the absolute
         {{!dirs}directory functions} and make your invocations
         independent from the [cwd].}
      {- [k], if specified a fiber invoked once the spawn has succesfully
         executed with the exit code.}}
      {b TODO.} More expressive power could by added by:
      {ol
      {- Support to refine the read and write set after the operation
         returns.}}

      {b Note.} If the tool spawn acts on a sort of "main" file
      (e.g. a source file) it should be specified as the first element
      of [reads], this is interpreted specially by certain build
      tracer. *)

  (** {1:futs Future values} *)

  (** Future values. *)
  module Fut : sig

    (** {1:fut Future value setters} *)

    type 'a set
    (** The type for setting a future value of type ['a]. *)

    val set : 'a set -> 'a -> unit
    (** [set s v] sets the future value linked to [s] to the value [v].
        @raise Invalid_argument if the value was already set. *)

    (** {1:fut Future values} *)

    type memo = t
    (** See {!Memo.t} *)

    type 'a t
    (** The type for future values of type ['a]. *)

    val create : memo -> 'a t * 'a set
    (** [create memo] is [(f, s)] a future value [f] and a setter [s]
        for it. Fibers waiting on the future are scheduled by
        {!stir}ing [memo]. *)

    val value : 'a t -> 'a option
    (** [value f] is [f]'s value if set. *)

    val wait : 'a t -> 'a fiber
    (** [wait f k] waits for [f] to be set and continues with [k v]
        with [v] the value of the future. *)
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
