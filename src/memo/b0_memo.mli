(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build memoizer *)

open B0_std

(** {1:memo Memoizer} *)

type t
(** The type for memoizers. Provides a context to issue
    memoizable operations. *)

(** {1:spawns Tool spawns}

   {b TODO} explain better how this all works and try to simplify.  If
   the path given to [Tool.t] is not made of a single path segment it
   is not search in the environmet and it is the duty of the client to
   ensure it gets ready at some point. Either by a direct call to
   {!file_ready} or by another file write. *)

(** Command line tools.

    A tool is specified either by name, to be looked up via an
    unspecified mecanism, or by a file path to an executable file. It
    declares the environment variables it accesses in the process
    environment and whether and how it supports response files.

    By default declared environment variables are assumed to influence
    the tool's output and are part of the stamp used to memoize tool
    spawns. If an environment variable is accessed by the tool but
    does not influence its output it should be declared as
    unstamped. Variables specifying the location of
    {{!Tool.tmp_vars}temporary file directories} are good examples of
    unstamped variables.

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

  val make :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> Cmd.tool -> t
  (** [make ~response_file ~unstamped_vars ~vars cmd] is a tool specified
      by [cmd]. [vars] are the stamped variables accessed by the
      tool (defaults to [[]]). [unstamped_vars] are the unstamped
      variables accessed by the tool (defaults to {!tmp_vars}).
      [response_file] defines the reponse file support for the tool
      (if any). *)

  val by_name :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> string -> t
  (** [by_name] is like {!make} but reference the tool directly via a name.

      Raises [Invalid_argument] if {!B0_std.Fpath.is_seg} [name] is [false]. *)

  val name : t -> Cmd.tool
  (** [name t] is [t]'s tool name. If this is a relative file path
      with a single segment the tool is meant to be searched via an
      external mecanism. *)

  val vars : t -> env_vars
  (** [vars t] are the stamped environment variables accessed by [t]. *)

  val unstamped_vars : t -> env_vars
  (** [unstamped_vars t] are the unstamped environment variables
      accessed by [t]. *)

  val response_file : t -> response_file option
  (** [response_file t] is [t]'s response file specification (if any). *)

  val read_env :
    forced_env_vars:env_vars -> t -> Os.Env.t -> Os.Env.t * Os.Env.t
  (** [read_env t env] is [(all, stamped)] such that:
      {ul
      {- [all] has the variables of [env] that are in [vars t]
         and [unstamped_vars t]}
      {- [unstamped] has the variables of [env] that are in
         [stamped t]  only.}} *)
end

(** Memo environment lookups. *)
module Env : sig

  val find : empty_is_none:bool -> Os.Env.var_name -> t -> string option
  (** [find var m] looks up [var] in [m]'s {!env}.
      This {!B0_memo.fail}s the memo in case of error. *)

  val find' : empty_is_none:bool ->
    (Os.Env.var_name -> ('a, string) result) -> Os.Env.var_name -> t ->
    'a option
  (** [find' m parse var] looks up [var] in [m]'s {!env} and
      parses it with [parse]. This {!B0_memo.fail}s the memo in case of
      error. *)

  val mem : Os.Env.var_name -> t -> bool
  (** [mem var m] is [true] if [var] is defined in [m]'s {!env},
      even if empty. *)
end

type tool_lookup = t -> Cmd.tool -> (Fpath.t, string) result Fut.t
(** The type for tool lookups. Given a command line tool
    {{!type:B0_std.Cmd.tool}specification} returns a file path to
    the tool executable or an error message mentioning the tool if
    it cannot be found. *)

val tool_lookup_of_os_env :
  ?sep:string -> ?var:string -> Os.Env.t -> tool_lookup
(** [env_tool_lookup ~sep ~var env] is a tool lookup that gets the
    value of the [var] variable in [env] treats it as a [sep]
    separated {{!B0_std.Fpath.list_of_search_path}search path} and
    uses the result to lookup with {!B0_std.Os.Cmd.get} with
    the memo's {!win_exe}. [var] defaults to [PATH] and [sep] to
    {!B0_std.Fpath.search_path_sep}. *)

type cmd
(** The type for memoized tool invocations. *)

type tool
(** The type for memoized tools. *)

val tool : t -> Tool.t -> (Cmd.t -> cmd)
(** [tool m t] is tool [t] memoized. Use the resulting function
    to spawn the tool with the given arguments. *)

val tool_opt : t -> Tool.t -> (Cmd.t -> cmd) option Fut.t
(** [tool_opt m t] is like {!val:tool}, except [None] is returned
    if the tool cannot be found. y*)

val spawn :
  t -> ?stamp:string -> ?reads:Fpath.t list -> ?writes:Fpath.t list ->
  ?env:Os.Env.t -> ?cwd:Fpath.t -> ?stdin:Fpath.t ->
  ?stdout:B0_zero.Op.Spawn.stdo -> ?stderr:B0_zero.Op.Spawn.stdo ->
  ?success_exits:B0_zero.Op.Spawn.success_exits ->
  ?post_exec:(B0_zero.Op.t -> unit) ->
  ?k:(B0_zero.Op.t -> int -> unit) -> cmd -> unit
(** [spawn m ~reads ~writes ~env ~cwd ~stdin ~stdout ~stderr
    ~success_exits cmd] spawns [cmd] once [reads] files are ready
    and makes files [writes] ready if the spawn succeeds and the
    file exists. The rest of the arguments are:
    {ul
    {- [stdin] reads input from the given file. If unspecified reads
       from the standard input of the program running the build.  {b
       Warning.} The file is not automatically added to [reads],
       this allows for example to use {!B0_std.Fpath.null}.}
    {- [stdout] and [stderr], the redirections for the standard
       outputs of the command, see {!B0_zero.Op.Spawn.stdo}. Path to files are
       created if needed. {b Warning.} File redirections
       are not automatically added to [writes]; this allows for example
       to use {!B0_std.Fpath.null}.}
    {- [success_exits] the exit codes that determine if the build operation
       is successful (defaults to [0], use [[]] to always succeed)}
    {- [env], environment variables added to the build environment.
       This overrides environment variables read by the tool in the
       build environment except for forced one. It also allows to
       specify enovironment that may not be mentioned by the running
       tool's {{!Tool.make}environment specification}.}
    {- [cwd] the current working directory. Default is the memo's [cwd]. In
       general it's better to avoid using relative file paths and
       tweaking the [cwd]. Construct make your paths absolute
       and invocations independent from the [cwd].}
    {- [post_exec], if specified is called with the build operation
       after it has been executed or revived. If it was executed
       this is called before the operation gets recorded. It can
       be used to define the [reads] and [writes] of the operation
       if they are difficult to find out before hand. {b Do not}
       access [m] in that function.}
    {- [k], if specified a function invoked once the spawn has succesfully
       executed with the operation and the exit code.}
    {- [stamp] is used for caching if two spawns diff only in their
       stamp they will cache to different keys. This can be used to
       memoize tool whose outputs may not entirely depend on the environment,
       the cli stamp and the the content of read files.}}

    {b Note.} If the tool spawn acts on a sort of "main" file
    (e.g. a source file) it should be specified as the first element
    of [reads], this is interpreted specially by certain build
    tracer. *)

val spawn' :
  t -> ?stamp:string -> ?reads:Fpath.t list -> writes_root:Fpath.t ->
  ?writes:(B0_zero.Op.t -> Fpath.t list) -> ?env:Os.Env.t -> ?cwd:Fpath.t ->
  ?stdin:Fpath.t -> ?stdout:B0_zero.Op.Spawn.stdo ->
  ?stderr:B0_zero.Op.Spawn.stdo ->
  ?success_exits:B0_zero.Op.Spawn.success_exits ->
  ?k:(B0_zero.Op.t -> int -> unit) -> cmd -> unit
(** [spawn'] is like {!val-spawn} except the actual file paths written
    by the spawn need not be determined before the spawn. Only the
    root directory of writes need to be specified via
    [writes_root]. After the spawn executes the writes can be
    determined via the [writes] function, the returned paths must be
    absolute and be prefixed by [writes_root] (defaults to recursively
    list all the files rooted in [writes_root]). *)

(** {1:proc Procedures and failing them} *)

val run_proc : t -> (unit -> unit Fut.t) -> unit
(** [run_proc m proc] calls [proc ()] and handles any {!fail}ure. This
    also catches non-asynchronous uncaught exceptions and turns them
    into [`Fail] notification operations. *)

val fail : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail m fmt ...] fails the procedure via a {!notify} operation. *)

val fail_if_error : t -> ('a, string) result -> 'a
(** [fail_if_error m r] is [v] if [r] is [Ok v] and [fail m "%s" e] if
    [r] is [Error _]. *)

(** {1:files Files and directories} *)

val ready_file : t -> Fpath.t -> unit
(** [read_file m p] declares path [p] to be ready, that is exists and is
    up-to-date in [b]. This is typically used with source files
    and files external to the build like installed libraries. *)

val ready_files : t -> Fpath.t list -> unit
(** [ready_files m ps] is [List.iter (ready_files m) ps]. *)

val read : t -> Fpath.t -> string Fut.t
(** [read m file k] is a future that determines with the contents
    [s] of file [file] when it becomes ready in [m]. *)

val write :
  t -> ?stamp:string -> ?reads:Fpath.t list -> ?mode:int ->
  Fpath.t -> (unit -> (string, string) result) -> unit
(** [write m ~reads file w] writes [file] with data [w ()] and mode
    [mode] (defaults to [0o644]) when [reads] are ready. [w]'s
    result must only depend on [reads], [mode] and [stamp] (defaults to
    [""]) and should not perform other side effects on the file system. *)

val copy :
  t -> ?mode:int -> ?linenum:int -> Fpath.t -> dst:Fpath.t -> unit
(** [copy m ~mode ?linenum src ~dst] copies file [src] to [dst] with
    mode [mode] (defaults to [0o644]) when [src] is ready. If [linenum]
    is specified, the following line number directive is prependend
    in [dst] to the contents of [src]:
{[
#line $(linenum) "$(src)"
]} *)

val copy_to_dir :
  t -> ?mode:int -> ?linenum:int ->
  ?src_root:Fpath.t -> Fpath.t -> dir:Fpath.t -> Fpath.t
(** [copy_to_dir src dir] is [copy src ~dst] with [dst] as
    [Fpath.reroot ~src_root ~dst_root:dst src] and [src_root] defaulting
    to [Fpath.parent src]. The function returns the destination file. *)

val mkdir : t -> ?mode:int -> Fpath.t -> unit Fut.t
(** [mkdir m dir p] is a future that determines with [()] when the
    directory path [p] has been created with mode [mode] (defaults
    to [0o755]). The behaviour with respect to file permission
    of intermediate path segments matches {!B0_std.Os.Dir.create}. *)

val delete : t -> Fpath.t -> unit Fut.t
(** [delete m p] is a future that determines with [()] when path [p]
    is deleted (trashed in fact) and free to reuse. *)

val wait_files : t -> Fpath.t list -> unit Fut.t
(** [wait_files m files] is a future that deterines with [()]
    when all [files] are ready in [m]. {b FIXME} Unclear whether
    we really want this, but somehow it's part of the primitives.  *)

(** {1:marks Activity marks}

    Activity marks are just identifiers used for UI purposes to
    watermark the activity – notably build operations – occuring in
    the memo. *)

val mark : t -> string
(** [mark m] is [m]'s mark. *)

val with_mark : t -> string -> t
(** [mark m mark] is [m] but operations performed on [m] are marked by
    [mark]. *)

(** {1:feedback Feedback}

    {b XXX} This needs a bit of reviewing. *)

type notify_kind = [ `Fail | `Warn | `Start | `End | `Info ]

val notify :
  ?k:(unit -> unit) -> t ->  notify_kind ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [notify m kind msg] is a notification [msg] of kind [kind]. Note that
    a [`Fail] notification will entail an an {!has_failures} on the memo,
    see also {!fail} and {!fail_if_error}. *)

val notify_if_error : t -> notify_kind -> use:'a -> ('a, string) result -> 'a
(** [notify_if_error m kind ~use r] is [v] if [r] is [Ok v]. If [r]
    is [Error e], a notification of kind [kind] is added to [m]
    and [use] is returned. Note that a [`Fail] notification will entail
    an {!has_failures} on the memo, see also {!fail} and {!fail_if_error}. *)

(** {1:creating Creating} *)

type feedback = [ `Op_complete of B0_zero.Op.t ]
(** The type for memoizer feedback. *)

val make_zero :
  ?clock:Os.Mtime.counter -> ?cpu_clock:Os.Cpu.Time.counter ->
  feedback:(feedback -> unit) -> cwd:Fpath.t ->
  ?win_exe:bool -> ?tool_lookup:tool_lookup -> ?env:Os.Env.t ->
  ?forced_env_vars:Tool.env_vars ->
  B0_zero.Guard.t -> B0_zero.Reviver.t -> B0_zero.Exec.t -> (t, string) result

val make :
  ?hash_fun:(module Hash.T) -> ?win_exe:bool -> ?tool_lookup:tool_lookup ->
  ?env:Os.Env.t -> ?forced_env_vars:Tool.env_vars -> ?cwd:Fpath.t ->
  ?jobs:int -> ?feedback:([feedback | B0_zero.Exec.feedback] -> unit) ->
  cache_dir:Fpath.t -> trash_dir:Fpath.t -> unit -> (t, string) result
(** [make] is a simpler {!make_zero}
    {ul
    {- [hash_fun] defaults to {!B0_std.Hash.Xxh3_64}.}
    {- [jobs] defaults to {!B0_std.Os.Cpu.logical_count}.}
    {- [env] defaults to {!B0_std.Os.Env.current}}
    {- [cwd] defaults to {!val:B0_std.Os.Dir.cwd}}
    {- [cache_dir] is the cache directory.}
    {- [trash_dir] is the trash directory.}
    {- [feedback] defaults to a nop.}
    {- [forced_env_vars], defaults to [[]].}} *)

val with_feedback : t -> (feedback -> unit) -> t
(** [with_feedback m feedback] is [m] with feedback replaced by [feedback]. *)

(** {1:low Low-level operations} *)

val delete_trash : block:bool -> t -> (unit, string) result
(** [delete_trash ~block m] is {!B0_zero.Trash.delete}[ ~block (trash m)]. *)

val hash_string : t -> string -> Hash.t
(** [hash_string m s] is {!B0_zero.Reviver.hash_string}[ (reviver m) s]. *)

val hash_file : t -> Fpath.t -> (Hash.t, string) result
(** [hash_file m f] is {!B0_zero.Reviver.hash_file}[ (reviver m) f].
    Note that these file hashes operations are memoized. *)

val stir : block:bool -> t -> unit
(** [stir ~block m] runs the memoizer a bit. If [block] is [true]
    blocks until the memoizer is stuck with no operation to execute. *)

val status : t -> (unit, B0_zero.Op.aggregate_error) result
(** [status m] looks for aggregate errors in [m] in [ops m], see
    {!B0_zero.Op.aggregate_error} for details.

    Usually called after a blocking {!stir} to check everything
    executed as expected. The function itself has no effect more
    operations can be on [m] afterwards. If you are only interested in
    checking if a failure occured in the memo {!has_failures} is
    faster. *)

val timestamp : t -> Mtime.Span.t
(** [timestamp m] gets a {!clock} time stamp. *)

(** {1:props Properties} *)

val clock : t -> Os.Mtime.counter
(** [clock m] is [m]'s clock. *)

val cpu_clock : t -> Os.Cpu.Time.counter
(** [cpu_clock m] is [m]'s cpu clock. *)

val env : t -> Os.Env.t
(** [env m] is the environment of [m]. The environment read by the tools'
    declared environment variables. *)

val exec : t -> B0_zero.Exec.t
(** [exec m] is [m]'s executors. *)

val forced_env_vars : t -> Tool.env_vars
(** [forced_env_vars m] are the forced environment variables of [m].
    These variables are put in the stamped environment of any tool
    despite what it declares to access. *)

val guard : t -> B0_zero.Guard.t
(** [guard m] is [m]'s guard. *)

val has_failures : t -> bool
(** [has_failures m] is [true] iff at least one operation has failed. *)

val ops : t -> B0_zero.Op.t list
(** [ops m] is the list of operations that were submitted to the
    memoizer *)

val reviver : t -> B0_zero.Reviver.t
(** [reviver m] is [m]'s reviver. *)

val tool_lookup : t -> tool_lookup
(** [tool_lookup m] is [m]'s tool lookup function. *)

val trash : t -> B0_zero.Trash.t
(** [trash m] is [m]'s trash. *)

val win_exe : t -> bool
(** [win_exe m] is [true] if we spawn windows executables. This
    affects tool lookups. Defaults to {!Sys.win32}. *)
