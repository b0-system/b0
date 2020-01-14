(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build kernel plumbing.

    These components when tied together appropriately are what makes
    a build {!B00.Memo}. *)

(** {1:b000 B000} *)

open B0_std

(** Delete file hierarchies.

    Deleting file hierarchies may be slow. This clears a given
    path by moving it to a trash directory which can be deleted
    at the end of the build asynchronously. *)
module Trash : sig

  type t
  (** The type for trashes. *)

  val create : Fpath.t -> t
  (** [create dir] is a trash using directory [dir] for data
      storage. [dir] may not exist. *)

  val dir : t -> Fpath.t
  (** [dir t] is the trash's directory (may not exist) *)

  val trash : t -> Fpath.t -> (unit, string) result
  (** [trash t p] trashes path [p] in [dir t]. If [p] does not exist
      this has no effect. Note that [p] needs to be on the same device
      as [dir t] and that the latter is created if needed. *)

  val delete : block:bool -> t -> (unit, string) result
  (** [delete ~block t] deletes [t]'s trash directory and its
      content. If [block] is [true] the operation is synchronous and
      blocks until the trash is effectively deleted; if [false] the
      operation is spawn as a separate process.

      {b Note.} On Windows [block:false] relies on [cmd.exe] being
      available. *)
end

(** File caches.

    A file cache maps a key to a metadata hunk and an ordered list of
    file {e contents} (filenames are irrelevant).

    Cache bindings are used to recreate the effect of build operations
    without having to rerun them. The key identifies the operation
    (usually via a hash), the ordered files are those written by the
    operation and the metadata has additional output information
    (e.g. exit codes, standard outputs, etc.).

    For some scenarios it may still be useful to store relative
    not only file contents but also relative file paths associated
    to them. Thus for certain keys these are stored along side
    the file contents.

    {b Note.} In general, whenever a cache operation modifies the
    file system and errors with [Error _] the resulting file system
    state is undefined. *)
module File_cache : sig

  (** {1:file_cache File caches} *)

  type key = string
  (** The type for keys. A key maps to a metadata hunk and an ordered
      list of file contents. The module treats keys as sequence of
      bytes however since they are used as file names they should
      satisfy the {!B0_std.Fpath.is_seg} predicate; this is not
      checked by the module. *)

  type t
  (** The type for file caches. *)

  val create : Fpath.t -> (t, string) result
  (** [create dir] is a file cache using directory [dir] for data
      storage. The full path to [dir] is created by the call if [dir]
      doesn't exist. *)

  val dir : t -> Fpath.t
  (** [dir c] is [c]'s storage directory. *)

  val keys : t -> (key list, string) result
  (** [keys c] are the keys of cache [c]. *)

  val key_stats : t -> key -> (int * int * float, string) result
  (** [key_stats c key] is statistical information about key [key].
      Namely the number of files (including the metadata and
      manifest), the key size in bytes and the access time of the key
      – this is the latest access time of one of its consituents the
      relevance of which depends on your file system. *)

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

  val manifest_add :
    t -> key -> string -> root:Fpath.t -> Fpath.t list -> (bool, string) result
  (** [manifest_add c k m ~root fs] is like {!add} except it also
      stores the file paths [fs] relativized with respect to
      [root]. This means the actual file paths need not to be provided
      to revive the operation see {!manifest_revive}. This errors
      if one of the files in [fs] is not prefixed by [root]. *)

  val rem : t -> key -> (bool, string) result
  (** [rem c k] removes the binding of [k] in [c]. [Ok true] is
      returned if [k] was bound in [c] and [Ok false] otherwise. *)

  val find : t -> key ->
    ((Fpath.t option * Fpath.t * Fpath.t list) option, string) result
  (** [find c k] is [Some (mf, m, fs)] if [k] is bound in [c] with
      [mf] the file that holds the file manifest (if any), [m] the
      file that holds the key metadata and [fs] the files that hold
      the file contents of the key in the order given on {!add} (or in
      the order of the manifest). The result is [None] if [k] is
      unbound in [c]. *)

  val revive :
    t -> key -> Fpath.t list -> (string option, string) result
  (** [revive c k fs] binds the file contents of key [k] to the file
      {e path}s [fs]. These file paths are overwritten if they exist
      and intermediate directories are created if needed (with
      permissions [0o755]). The function returns:
      {ul
      {- [Ok (Some m] in case of success, with [m] the metadata of the key.
         In this case the files [fs] are guaranteed to exist and to match
         those of the key files in the order given on {!add}.}
      {- [Ok None] if the length of [fs] does not match the
         sequence of files of [k] or if [k] is unbound in [c].
         In this case the file paths [fs] are left untouched.}
      {- [Error _] if an unexpected error occurs. In that case the
         resulting state of the file system for paths [fs] is
         undefined.}} *)

  val manifest_revive :
    t -> key -> root:Fpath.t -> ((Fpath.t list * string) option, string) result
  (** [manifest_revive] is like {!revive} however the file paths
      that are written to are determined by the cache's file manifest
      whose path are made absolute using [root] and returned in
      the result. [None] is returned if the key existed but
      had no manifest. *)

  val trim_size :
    ?is_unused:(key -> bool) -> t -> max_byte_size:int -> pct:int ->
    (unit, string) result
  (** [trim_size c ~is_unused max_byte_size ~pct] deletes keys of [c]
      until the cache either weights at most [max_byte_size] or is
      [pct] of its current size; whichever is the smaller. The
      function deletes by order of increasing key access time (see
      {!key_stats}) but unused keys determined by {!is_unused} are deleted
      first (defaults to [fun _ -> false]). *)
end

(** Build operations.

    This module provides a type for {e specifying} operations and
    their result. Operation execution and caching are respectively
    handled by the {!Exec} and {!Reviver} modules. *)
module Op : sig

  (** {1:op_status Operation status} *)

  type failure =
  | Exec of string option (** Execution failure with a potential error msg. *)
  | Missing_writes of Fpath.t list (** Write specification failure. *)
  | Missing_reads of Fpath.t list (** Read synchronisation failure. *)
  (** The type for operation failures. *)

  type status =
  | Aborted  (** Aborted due to prerequisite failure. *)
  | Done (** Executed successfully. *)
  | Failed  of failure (** Executed unsuccessfully. *)
  | Waiting  (** Waiting for execution. *)
  (** The type for operation statuses. *)

  (** {1:op Operations} *)

  type id = int
  (** The type for build operation identifiers. *)

  type group = string
  (** The type for build operation groups. *)

  type t
  (** The type for build operations. *)

  type op = t
  (** Again. *)

  (** File copy. *)
  module Copy : sig

    (** {1:copy File copy} *)

    type t
    (** The type for file copies. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> mode:int -> linenum:int option -> src:Fpath.t ->
      Fpath.t -> op
    (** [v] declares a file copy operation, see the corresponding
        accessors for the semantics of various arguments. *)

    val v : src:Fpath.t -> dst:Fpath.t -> mode:int -> linenum:int option -> t
    (** [v] constructs a bare copy operation. *)

    val get : op -> t
    (** [get o] is the copy operation [o]. Raises {!Invalid_argument} if [o]
        is not a copy. *)

    val src : t -> Fpath.t
    (** [src c] is the file read for the copy. *)

    val dst : t -> Fpath.t
    (** [dst c] is the written by the copy. *)

    val mode : t -> int
    (** [mode c] is the mode of the file written by the copy. *)

    val linenum : t -> int option
    (** [linenum c] is the linumber directive to write at the begining
        of the destination file (if any). *)
  end

  (** Path deletion. *)
  module Delete : sig

    (** {1:del Path deletion} *)

    type t
    (** The type for path deletion operations. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> Fpath.t -> op
    (** [v_op] declares a path deletion operation, see the corresponding
        accessors for the semantics of the various arguments. *)

    val v : path:Fpath.t -> t
    (** [v] constructs a bare deletion operation. *)

    val get : op -> t
    (** [get o] is the delete operation [o]. Raises {!Invalid_argument} if [o]
        is not a delete. *)

    val path : t -> Fpath.t
    (** [path d] is the path to delete. *)
  end

  (** Directory creation. *)
  module Mkdir : sig

    (** {1:mkdir Directory creation} *)

    type t
    (** The type for directory creation operations. *)

    val v_op :
      id:id -> group:group -> mode:int -> created:Time.span ->
      ?post_exec:(op -> unit) -> ?k:(op -> unit) -> Fpath.t -> op
    (** [v_op] declares a directory creation operation, see the
        corresponding accessors for the semantics of the various
        arguments. *)

    val v : dir:Fpath.t -> mode:int -> t
    (** [v] constructs a bare mkdir operation. *)

    val get : op -> t
    (** [get o] is the mkdir [o]. Raises {!Invalid_argument} if [o] is not
        a mkdir. *)

    val dir : t -> Fpath.t
    (** [dir mk] is the directory created by [mk]. *)

    val mode : t -> int
    (** [mode mk] are the permissions of the directory created by [mk]. *)
  end

  (** End-user notifications. *)
  module Notify : sig

    (** {1:notifications Notification} *)

    type t
    (** The type for notification. *)

    type kind = [ `End | `Fail | `Info | `Start | `Warn ]
    (** The type for kinds of notifications. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> kind -> string -> op
    (** [v_op] declares a notification operation see the corresponding
        accessors in {!Notify} for the semantics of the various
        arguments. *)

    val v : kind:kind -> msg:string -> t
    (** [v] constructs a notification operation. *)

    val get : op -> t
    (** [get o] is the notification [o]. Raise {!Invalid_argument} if [o]
        is not a notification. *)

    val kind : t -> kind
    (** [kind] is the kind of notification. *)

    val msg : t -> string
    (** [msg] is the message. *)
  end

  (** File reads. *)
  module Read : sig

    (** {1:read File reads} *)

    type t
    (** The type for file read operations. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> Fpath.t -> op
    (** [v_op] declares a file read operation, see the corresponding
        accessors in {!Read} for the semantics of the various
        arguments. *)

    val v : file:Fpath.t -> data:string -> t
    (** [v] constructs a bare read operation. *)

    val get : op -> t
    (** [get o] is the read [o]. Raise {!Invalid_argument} if [o]
        is not a read. *)

    val file : t -> Fpath.t
    (** [file r] is the file read by [r]. *)

    val data : t -> string
    (** [data r] is the read data. *)

    val set_data : t -> string -> unit
    (** [set_data r d] sets the read data to [d]. *)

    val discard_data : t -> unit
    (** [discard_data ()] discards read data. *)
  end

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

    val v_op :
      id:id -> group:group -> created:Time.span -> reads:Fpath.t list ->
      writes:Fpath.t list -> ?writes_manifest_root:Fpath.t ->
      ?post_exec:(op -> unit) -> ?k:(op -> unit) ->
      stamp:string -> env:Os.Env.assignments ->
      stamped_env:Os.Env.assignments -> cwd:Fpath.t ->
      stdin:Fpath.t option -> stdout:stdo -> stderr:stdo ->
      success_exits:success_exits -> Cmd.tool -> Cmd.t -> op
    (** [v_op] declares a spawn build operation, see the corresponding
        accessors in {!Spawn} for the semantics of the various arguments. *)

    val v :
      env:Os.Env.assignments -> stamped_env:Os.Env.assignments ->
      cwd:Fpath.t -> stdin:Fpath.t option -> stdout:stdo ->
      stderr:stdo -> success_exits:success_exits -> Cmd.tool ->
      Cmd.t -> stamp:string -> stdo_ui:(string, string) result option ->
      exit:Os.Cmd.status option -> t
    (** [v] constructs a bare spawn operation. *)

    val get : op -> t
    (** [get o] is the spawn [o]. Raises {!Invalid_argument} if [o] is
        not a spawn. *)

    val env : t -> Os.Env.assignments
    (** [env s] is the environment in which [s] runs. *)

    val stamped_env : t -> Os.Env.assignments
    (** [stamped_env s] are the assignements of [env s] that may
        influence the tool outputs. *)

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

    val stamp : t -> string
    (** [stamp s] is a stamp that is used for caching. Two spans
        operations all otherwise equal but differing only in their
        stamp must not cache to the same key. *)

    val set_stamp : t -> string -> unit
    (** [set_stamp s st] sets the stamp of [s] to [st]. See {!stamp}. *)

    val stdo_ui : t -> (string, string) result option
    (** [stdo_ui sr] is the standard outputs redirection contents
        of [s] once it has executed (if any). *)

    val set_stdo_ui : t -> (string, string) result option -> unit
    (** [set_stdo_ui w ui] sets [w]'s standard output redirection contents
        to [ui]. *)

    val exit : t -> Os.Cmd.status option
    (** [exit s] is the spawn exit status of [s]. *)

    val set_exit : t -> Os.Cmd.status option -> unit
    (** [set_exit s e] the spawn exit status of [s] to [e]. *)

    val exit_to_status : t -> status
    (** [exit_to_status s] assumes [s] has been executed and
        determines an operation status according to {!exit}
        and {!success_exits}. *)
  end

  (** Waiting on files.

      FIXME now that we have Futs maybe we can maybe get rid of this. *)
  module Wait_files : sig
    type t
    (** The type for wait files operations. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> Fpath.t list -> op
    (** [v] declares a wait files operation, these are stored in
          {!reads}. *)

    val v : unit -> t
    (** [v] constructs a bare wait files operation. *)
  end

  (** File writes. *)
  module Write : sig

    (** {1:write File writes} *)

    type t
    (** The type for file write operations. *)

    val v_op :
      id:id -> group:group -> created:Time.span -> ?post_exec:(op -> unit) ->
      ?k:(op -> unit) -> stamp:string -> reads:Fpath.t list -> mode:int ->
      write:Fpath.t -> (unit -> (string, string) result) -> op
    (** [write] declares a file write operations, see the corresponding
        accessors in {!Write} for the semantics of the various arguments. *)

    val v :
      stamp:string -> mode:int -> file:Fpath.t ->
      data:(unit -> (string, string) result) -> t
    (** [v] constructs a bare write operation. *)

    val get : op -> t
    (** [geo o] is the write [o]. Raises {!Invalid_argument} if [o] is
        not a write. *)

    val stamp : t -> string
    (** [stamp w] is the file write stamp used for caching. *)

    val mode : t -> int
    (** [int w] is the mode of the file written by [w]. *)

    val file : t -> Fpath.t
    (** [file w] is the file written by [w]. *)

    val data : t -> (string, string) result
    (** [data w] invokes and discards the write data function. If the
        write data function raises this is turned into an [Error _]. *)

    val discard_data : t -> unit
    (** [discard_data w] discards the write data function. *)
  end

  type kind =
  | Copy of Copy.t
  | Delete of Delete.t
  | Mkdir of Mkdir.t
  | Notify of Notify.t
  | Read of Read.t
  | Spawn of Spawn.t
  | Wait_files of Wait_files.t
  | Write of Write.t (** *)
  (** The type for operation kinds. *)

  val kind_name : kind -> string
  (** [kind_name k] is an end user name for kind [k]. *)

  val v :
    id -> group:group -> time_created:Time.span -> time_started:Time.span ->
    duration:Time.span -> revived:bool -> status:status ->
    reads:Fpath.t list -> writes:Fpath.t list ->
    writes_manifest_root:Fpath.t option -> hash:Hash.t ->
    ?post_exec:(op -> unit) -> ?k:(op -> unit) -> kind -> t
  (** [v] constructs an operation. See the corresponding accessors
      for the semantics of various arguments. *)

  val kind : t -> kind
  (** [kind o] is [o]'s kind. *)

  val equal : t -> t -> bool
  (** [equal o0 o1] is [id o0 = id o1]. *)

  val compare : t -> t -> int
  (** [compare o0 o1] is [compare (id o0) (id o1)]. *)

  val id : t -> id
  (** [id o] is the identifier of operation [o]. *)

  val group : t -> string
  (** [group o] is the group of [o]. *)

  val time_created : t -> Time.span
  (** [time_created o] is [o]'s creation time. *)

  val time_started : t -> Time.span
  (** [time_started o] is [o]'s execution start time. This is
      different from {!B0_std.Time.Span.max} once the operation has
      been submitted for execution. *)

  val time_ended : t -> Time.span
  (** [exec_end_time o] is [o]'s execution end time. This is different
      from {!B0_std.Time.Span.max} once the operation has been completed
      and collected. *)

  val waited : t -> Time.span
  (** [waited] is [o]'s waiting time between creation and execution. *)

  val duration : t -> Time.span
  (** [duration] is [o]'s execution duration time. *)

  val revived : t -> bool
  (** [revived o] is [true] iff [o] was revived from a cache. Only
      relevant if {!hash} is not {!Hash.nil}. *)

  val status : t -> status
  (** [status o] is [o] execution status. *)

  val reads : t -> Fpath.t list
  (** [reads o] are the file paths read by the operation. *)

  val writes : t -> Fpath.t list
  (** [writes o] are the file paths written by [o]. *)

  val writes_manifest_root : t -> Fpath.t option
  (** [writes_manifest_root o] if [Some root], the operation is cached
      using a manifest key. This means the {!writes} made relative to
      [root] are stored along-side the cache key. *)

  val hash : t -> Hash.t
  (** [hash o] is the operation's hash. This is {!Hash.nil} before the
      operation hash has been effectively computed and set via
      {!set_hash}. This remains {!Hash.nil} for operations that are
      not revivable. *)

  (** {1:upd Updating the build operation} *)

  val invoke_k : t -> unit
  (** [exec_k o ()] invokes and discards [o]'s continuation.
      Note that this does {b not} protect against the continuation
      raising. *)

  val discard_k : t -> unit
  (** [discard o] discards [o]'s continuation. *)

  val invoke_post_exec : t -> unit
  (** [exec_post_exec o] invokes and discards [o]'s post execution
      hook. This hook called is right after the operation execution
      and, if applicable, {b before} reviver recording. It is always
      called even if the operation fails or is revived (use {!status}
      and {!revived} to check these conditions). Note that if the hook
      unexpectedly raises this turns [o] in to a failure. *)

  val discard_post_exec : t -> unit
  (** [discard_post_exec o] discards [o]'s post execution hook. *)

  val abort : t -> unit
  (** [abort o] sets the status of [o] to {!Op.Aborted} and discards
      the operation closures (including kind specific ones). *)

  val set_time_started : t -> Time.span -> unit
  (** [set_time_started o t] sets [o]'s execution start time to [t]. *)

  val set_time_ended : t -> Time.span -> unit
  (** [set_time_ended o t] sets [o]'s execution end time to [s]. *)

  val set_revived : t -> bool -> unit
  (** [set_revived o b] sets [o]'s cache revival status to [b]. *)

  val set_status : t -> status -> unit
  (** [set_status o s] sets the execution status to [s]. *)

  val set_status_from_result : t -> ('a, string) result -> unit
  (** [set_status_from_result o r] sets status of operation [o]
      to [Executed] if [r] is [Ok _] and [Failed (Exec e)] if [r]
      is [Error e]. *)

  val set_reads : t -> Fpath.t list -> unit
  (** [set_reads t fs] sets the file paths read by [o] to [fs].
      Note that this resets the {!hash}. *)

  val set_writes : t -> Fpath.t list -> unit
  (** [set_writes t fs] sets the file paths written by [o] to [fs]. *)

  val set_writes_manifest_root : t -> Fpath.t option -> unit
  (** [set_writes_manifest_root t r] sets the writes manifest root
      to [r]. *)

  val set_hash : t -> Hash.t -> unit
  (** [set_hash o h] sets the operation hash to [h]. *)

  (** {1:set_map Operation sets and map} *)

  (** Operation sets *)
  module Set : Set.S with type elt = t

  (** Operation maps *)
  module Map : Map.S with type key = t

  (** {1:analyze Analyzing operations} *)

  val did_not_write : t -> Fpath.t list
  (** [did_not_write o] compares {!writes} with the current state
      of the file system and reports those files that do not exist.  *)

  val cannot_read : t -> Fpath.t list
  (** [cannot_read o] compares {!reads} with the current state
      of the file system and reports those files that cannot be read. *)

  val unready_reads : ready_roots:Fpath.Set.t -> t list -> Fpath.Set.t
  (** [unready_reads os] are the file read by [os] that are not written
      by those and not in [ready_roots]. *)

  val read_write_maps : t list -> Set.t Fpath.Map.t * Set.t Fpath.Map.t
  (** [read_write_maps ops] is [reads, writes] with [reads] mapping
      file paths to operations that reads them and [writes] mapping
      file paths to operations that write them. *)

  val write_map : t list -> Set.t Fpath.Map.t
  (** [write_map os] is [snd (read_write_maps os)]. If one of the
      operation sets in the map is not a singleton the operations
      should likely not be run toghether. *)

  val find_read_write_cycle : t list -> t list option
  (** [find_read_write_cycle os] is [Some cs] if there exists a
      read/write cycle among the operations [os]. This means each each
      element of [cs] writes a file read by its successor in the list
      with the successor of the last element being the first. *)

  type aggregate_error =
  | Failures (** Some operations failed. *)
  | Cycle of t list (** Dependency cycle. *)
  | Never_became_ready of Fpath.Set.t (** Some files never became ready. *)
  (** The type for errors related to a {e list} of operations. This is:
      {ul
      {- [Failures], if there is one or more operations in the list that
         {!Failed} (and hence for a {!B00.Memo} also if a fiber failed,
         see {{!B00.Memo.fiber}here}).}
      {- [Cycle ops], if there is a set of {!Waiting} operations
         in the list whose individual reads and writes leads to a dependency
         cycle. See also {!find_read_write_cycle}.}
      {- [Never_became_ready fs], with [fs] files that are in
         the reads of {!Waiting} operations but are written
         by no operation from the list and were not made ready.}}
      Note that formally more than one of these conditions can be true
      at the same time. But [Never_became_ready] is only reported once
      the first two kind of errors have been ruled out. The reason is
      that those files that never became ready may be created by
      continuations of the failed or cyclic operations and reporting
      them would not lead the user to focus on the right cause. *)

  val find_aggregate_error :
    ready_roots:Fpath.Set.t -> t list -> (unit, aggregate_error) result
  (** [find_aggregate_error ~ready_roots os] finds an aggregate error among
      the list of operation [os], assuming files [ready_roots] were made
      ready. This is [Ok ()] if all operations [os] {!Executed}. *)
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

  val hash_string : t -> string -> Hash.t
  (** [hash_string r s] hashes [s] using [r]'s {!hash_fun}. *)

  val hash_file : t -> Fpath.t -> (Hash.t, string) result
  (** [hash_file r f] hashes file [f] using [r]'s {!hash_fun}. Note
      that file hashes are {{!file_hashes}cached} by [r]. *)

  val hash_op : t -> Op.t -> (Hash.t, string) result
  (** [hash_op r o] hashes the operation [o]. Errors if an input
      file of the build operation can't be hashed, this is most
      likely because an input file does not exist. *)

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

  val revive : t -> Op.t -> (bool, string) result
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
      If [Ok true] is returned the operation was revived. If [Ok false]
      is returned then nothing was revived and the file system is
      kept unchanged. Nothing is guaranteed in case of [Error _].

      {b Warning.} The fields {!Op.exec_start_time},
      {!Op.exec_end_time} of [o] get set regardless of the result.
      This may be overwritten later by {!Exec}. *)
end

(** Build operation guards.

    A guard ensure that a build operation is allowed to proceed.
    This means either that:
    {ul
    {- It is {e ready} and can be submitted for execution. This
       happens once all the files the operation {{!Op.reads}reads} are
       {{!set_file_ready}ready}: they exist and are up-to-date.}
    {- It is {e aborted}. This happens if a file it needs to
       read {{!set_file_never}never} becomes ready.}}

    {b Note.} This module does not access the file system it trusts
    clients that call {!set_file_ready} not to lie about its existence
    on the file system. *)
module Guard : sig

  (** {1:guards Guards} *)

  type t
  (** The type for build operations guards. *)

  val create : unit -> t
  (** [create ()] is a new guard. *)

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
      in [g] (if any). In the second case the {!Op.status} is
      {!Op.Aborted}. *)
end

(** Build operation executors.

    An executor is a parallel asynchronous work queue. It has no
    notion of synchronisation, any scheduled operation is randomly
    executed in parallel up to the executor's parallelizing limits. *)
module Exec : sig

  (** {1:execs Executors} *)

  type feedback = [ `Exec_start of Os.Cmd.pid option * Op.t ]
  (** The type for executor feedbacks. Indicates the given
      operation starts executig with, for spawn operations,
      their operating system process identifier. *)

  type t
  (** The type for executors. *)

  val create :
    ?clock:Time.counter -> ?rand:Random.State.t -> ?tmp_dir:Fpath.t ->
    ?feedback:(feedback -> unit) -> trash:Trash.t -> jobs:int -> unit -> t
  (** [create ~clock ~rand ~tmp_dir ~feedback ~trash ~jobs] with:
      {ul
      {- [clock], the clock used to timestamp build operations;
         defaults to {!B0_std.Time.counter}[ ()].}
      {- [rand] random state used for internal queues; defaults to
         {!Random.State.make_self_init}.}
      {- [tmp_dir] is a directory for temporary files, it must exist;
          defaults to {!B0_std.Os.Dir.default_tmp}[ ()].}
      {- [feedback] a function called with each {{!schedule}scheduled}
         operation when it starts executing. Default is a nop.}
      {- [trash], the trash used to execute {!Op.Delete} build
         operations.}
      {- [jobs] the maximal number of processes spawn simultaneously.}}
 *)

  val clock : t -> Time.counter
  (** [clock e] is [e]'s clock. *)

  val tmp_dir : t -> Fpath.t
  (** [tmp_dir e] is [e]'s temporary directory. *)

  val trash : t -> Trash.t
  (** [trash e] is [e]'s trash. *)

  val jobs : t -> int
  (** [jobs e] is [e]'s maximal number of simultaneous process spawns. *)

  (** {1:schedcollect Scheduling and collecting operations} *)

  val schedule : t -> Op.t -> unit
  (** [schedule e o] schedules [o] for execution in [e]. When [o]
      starts executing it is given to the [feedback] callback of [e]
      (see {!create}). *)

  val collect : t -> block:bool -> Op.t option
  (** [collect e ~block] removes from [e] an operation that has
      completed (if any). If [block] is [false] and no completed
      operation exists, the call returns immediately with [None]. If
      [block] is [true] and at least one incomplete operation exists
      in [e], the call blocks until an operation completes. If [block]
      is [true] and no operation exists in [e] [None] is returned. *)
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
