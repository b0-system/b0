(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build kernel *)

(** {1:b00 B00} *)

open B00_std

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

  val v :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> Cmd.tool -> t
  (** [v ~response_file ~unstamped_vars ~vars cmd] is a tool specified
      by [cmd]. [vars] are the stamped variables accessed by the
      tool (defaults to [[]]). [unstamped_vars] are the unstamped
      variables accessed by the tool (defaults to {!tmp_vars}).
      [response_file] defines the reponse file support for the tool
      (if any). *)

  val by_name :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> string -> t
  (** [by_name] is like {!v} but reference the tool directly via a name.

      @raise Invalid_argument if {!B00_std.Fpath.is_seg} [name] is [false]. *)

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

  val read_env : t -> Os.Env.t -> Os.Env.t * Os.Env.t
  (** [read_env t env] is (all, stamped) with [all] the
      environment with the variables of [env] that are in [vars t]
      and [unstamped_vars t] and [stamped] those of [vars t] only. *)
end

(** Build environment.

    Build environments specify the environment of tool spawns.

    {b TODO} Now that tool lookup moved to Memo,
    is it still worth sense to have that separate ? *)
module Env : sig

  (** {1:env Environment} *)

  type t
  (** The type for build environments. *)

  val v : ?forced_env:Os.Env.t -> Os.Env.t -> t
  (** [v ~lookup ~forced_env env] is a build environment with:
      {ul
      {- [forced_env] is environment forced on any tool despite
         what it declared to access, defaults to {!B00_std.Os.Env.empty}}
      {- [env] the environment read by the tools' declared environment
         variables.}} *)

  val env : t -> Os.Env.t
  (** [env e] is [e]'s available spawn environment. *)

  val forced_env : t -> Os.Env.t
  (** [forced_env e] is [e]'s forced spawn environment. *)
end

(** Build memoizer.

    A memoizer ties together and environment, an operation cache, a guard
    and an executor. *)
module Memo : sig

  type t
  (** The type for memoizers. This ties together an environment, a
      guard, an operation cache and an executor. *)

  (** {1:lookup Tool lookup} *)

  type tool_lookup = t -> Cmd.tool -> (Fpath.t, string) result Fut.t
  (** The type for tool lookups. Given a command line tool
      {{!type:B00_std.Cmd.tool}specification} returns a file path to
      the tool executable or an error message mentioning the tool if
      it cannot be found. *)

  val tool_lookup_of_os_env :
    ?sep:string -> ?var:string -> Os.Env.t -> tool_lookup
  (** [env_tool_lookup ~sep ~var env] is a tool lookup that gets the
      value of the [var] variable in [env] treats it as a [sep]
      separated {{!B00_std.Fpath.list_of_search_path}search path} and
      uses the result to lookup with {!B00_std.Os.Cmd.get} with
      the memo's {!win_exe}. [var] defaults to [PATH] and [sep] to
      {!B00_std.Fpath.search_path_sep}. *)

  (** {1:memo Memoizer} *)

  type feedback =
  [ `Miss_tool of Tool.t * string
  | `Op_complete of B000.Op.t ]
  (** The type for memoizer feedback. FIXME remove `Miss_tool now
      that we have notify operations. *)

  val create :
    ?clock:Os.Mtime.counter -> ?cpu_clock:Os.Cpu.Time.counter ->
    feedback:(feedback -> unit) -> cwd:Fpath.t ->
    ?win_exe:bool -> ?tool_lookup:tool_lookup -> Env.t -> B000.Guard.t ->
    B000.Reviver.t -> B000.Exec.t -> t

  val memo :
    ?hash_fun:(module Hash.T) -> ?win_exe:bool -> ?tool_lookup:tool_lookup ->
    ?env:Os.Env.t -> ?cwd:Fpath.t -> ?cache_dir:Fpath.t ->
    ?trash_dir:Fpath.t -> ?jobs:int ->
    ?feedback:([feedback | B000.Exec.feedback] -> unit) -> unit ->
    (t, string) result
  (** [memo] is a simpler {!create}
      {ul
      {- [hash_fun] defaults to {!B00_std.Hash.Xxh_64}.}
      {- [jobs] defaults to {!B00_std.Os.Cpu.logical_count}.}
      {- [env] defaults to {!B00_std.Os.Env.current}}
      {- [cwd] defaults to {!val:B00_std.Os.Dir.cwd}}
      {- [cache_dir] defaults to [Fpath.(cwd / "_b0" / ".cache")]}
      {- [trash_dir] defaults to [Fpath.(cwd / "_b0" / ".trash")]}
      {- [feedback] defaults to a nop.}} *)

  val clock : t -> Os.Mtime.counter
  (** [clock m] is [m]'s clock. *)

  val cpu_clock : t -> Os.Cpu.Time.counter
  (** [cpu_clock m] is [m]'s cpu clock. *)

  val win_exe : t -> bool
  (** [win_exe m] is [true] if we spawn windows executables. This
      affects tool lookups. Defaults to {!Sys.win32}. *)

  val tool_lookup : t -> tool_lookup
  (** [tool_lookup m] is [m]'s tool lookup function. *)

  val env : t -> Env.t
  (** [env m] is [m]'s environment. *)

  val reviver : t -> B000.Reviver.t
  (** [reviver m] is [m]'s reviver. *)

  val guard : t -> B000.Guard.t
  (** [guard m] is [m]'s guard. *)

  val exec : t -> B000.Exec.t
  (** [exec m] is [m]'s executors. *)

  val trash : t -> B000.Trash.t
  (** [trash m] is [m]'s trash. *)

  val has_failures : t -> bool
  (** [has_failures m] is [true] iff at least one operation has failed. *)

  val hash_string : t -> string -> Hash.t
  (** [hash_string m s] is {!B000.Reviver.hash_string}[ (reviver m) s]. *)

  val hash_file : t -> Fpath.t -> (Hash.t, string) result
  (** [hash_file m f] is {!B000.Reviver.hash_file}[ (reviver m) f].
      Note that these file hashes operations are memoized. *)

  val stir : block:bool -> t -> unit
  (** [stir ~block m] runs the memoizer a bit. If [block] is [true]
      blocks until the memoizer is stuck with no operation to execute. *)

  val status : t -> (unit, B000.Op.aggregate_error) result
  (** [status m] looks for aggregate errors in [m] in [ops m], see
      {!B000.Op.aggregate_error} for details.

      Usually called after a blocking {!stir} to check everything
      executed as expected. The function itself has no effect more
      operations can be on [m] afterwards. If you are only interested
      in checking if a failure occured in the memo {!has_failures} is
      faster. *)

  val delete_trash : block:bool -> t -> (unit, string) result
  (** [delete_trash ~block m] is {!B000.Trash.delete}[ ~block (trash m)]. *)

  val ops : t -> B000.Op.t list
  (** [ops m] is the list of operations that were submitted to the
      memoizer *)

  (** {1:marks Activity marks}

      Activity marks are just identifiers used for UI purposes to
      watermark the activity – notably build operations – occuring in
      the memo. *)

  val mark : t -> string
  (** [mark m] is [m]'s mark. *)

  val with_mark : t -> string -> t
  (** [mark m mark] is [m] but operations performed on [m] are marked by
      [mark]. *)

  (** {2:proc Procedures} *)

  val run_proc : t -> (unit -> unit Fut.t) -> unit
  (** [run m proc] calls [proc ()] and handles any {!fail}ure. This
      also catches non-asynchronous uncaught exceptions and turns them
      into [`Fail] notification operations. *)

  val fail : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [fail m fmt ...] fails the procedure via a {!notify} operation. *)

  val fail_if_error : t -> ('a, string) result -> 'a
  (** [fail_if_error m r] is [v] if [r] is [Ok v] and [fail m "%s" e] if
      [r] is [Error _]. *)

  (** {1:feedback Feedback}

      {b XXX} This needs a bit of reviewing. *)

  val notify :
    ?k:(unit -> unit) -> t -> [ `Fail | `Warn | `Start | `End | `Info ] ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [notify m kind msg] is a notification [msg] of kind [kind]. Note that
      a [`Fail] notification will entail an an {!has_failures} on the memo,
      see also {!fail} and {!fail_if_error}. *)

  val notify_if_error :
    t -> [ `Fail | `Warn | `Start | `End | `Info ] -> use:'a ->
    ('a, string) result -> 'a
  (** [notify_if_error m kind ~use r] is [v] if [r] is [Ok v]. If [r]
      is [Error e], a notification of kind [kind] is added to [m]
      and [use] is returned. Note that a [`Fail] notification will entail
      an {!has_failures} on the memo, see also {!fail} and {!fail_if_error}. *)

  (** {1:files Files and directories} *)

  val file_ready : t -> Fpath.t -> unit
  (** [ready m p] declares path [p] to be ready, that is exists and is
      up-to-date in [b]. This is typically used with source files
      and files external to the build (e.g. installed libraries). *)

  val read : t -> Fpath.t -> string Fut.t
  (** [read m file k] is a future that determines with the contents
      [s] of file [file] when it becomes ready in  [m]. *)

  val write :
    t -> ?stamp:string -> ?reads:Fpath.t list -> ?mode:int ->
    Fpath.t -> (unit -> (string, string) result) -> unit
  (** [write m ~reads file w] writes [file] with data [w ()] and mode
      [mode] (defaults to [0o644]) when [reads] are ready. [w]'s
      result must only depend on [reads] and [stamp] (defaults to
      [""]). *)

  val copy :
    t -> ?mode:int -> ?linenum:int -> src:Fpath.t -> Fpath.t -> unit
  (** [copy m ~mode ?linenum ~src dst] copies file [src] to [dst] with
      mode [mode] (defaults to [0o644]) when [src] is ready. If [linenum]
      is specified, the following line number directive is prependend
      in [dst] to the contents of [src]:
{[
#line $(linenum) "$(src)"
]} *)

  val mkdir : t -> ?mode:int -> Fpath.t -> unit Fut.t
  (** [mkdir m dir p] is a future that determines with [()] when the
      directory path [p] has been created with mode [mode] (defaults
      to [0o755]). The behaviour with respect to file permission
      of intermediate path segments matches {!B00_std.Os.Dir.create}. *)

  val delete : t -> Fpath.t -> unit Fut.t
  (** [delete m p] is a future that determines with [()] when path [p]
      is deleted (trashed in fact) and free to reuse. *)

  val wait_files : t -> Fpath.t list -> unit Fut.t
  (** [wait_files m files] is a future that deterines with [()]
      when all [files] are ready in [m]. {b FIXME} Unclear whether
      we really want this. *)

  (** {1:spawn Memoizing tool spawns}

      {b TODO.} Can't we simplify the cmd/tool/tool_lookup dance ?. *)

  type cmd
  (** The type for memoized tool invocations. *)

  type tool
  (** The type for memoized tools. *)

  val tool : t -> Tool.t -> (Cmd.t -> cmd)
  (** [tool m t] is tool [t] memoized. Use the resulting function
      to spawn the tool with the given arguments.

      {b TODO} explain better how this all works. If the path given
      to [Tool.t] is not made of a single path segment it is not
      search in the environmet and it is the duty of the client
      to ensure it gets ready at some point. Either by a direct
      call to {!file_ready} or by another file write. *)

  val tool_opt : t -> Tool.t -> (Cmd.t -> cmd) option Fut.t
  (** [tool_opt m t] is like {!val:tool}, except [None] is returned
      if the tool cannot be found. y*)

  val spawn :
    t -> ?stamp:string -> ?reads:Fpath.t list -> ?writes:Fpath.t list ->
    ?env:Os.Env.t -> ?cwd:Fpath.t -> ?stdin:Fpath.t ->
    ?stdout:B000.Op.Spawn.stdo -> ?stderr:B000.Op.Spawn.stdo ->
    ?success_exits:B000.Op.Spawn.success_exits ->
    ?post_exec:(B000.Op.t -> unit) ->
    ?k:(int -> unit) -> cmd -> unit
  (** [spawn m ~reads ~writes ~env ~cwd ~stdin ~stdout ~stderr
      ~success_exits cmd] spawns [cmd] once [reads] files are ready
      and makes files [writes] ready if the spawn succeeds and the
      file exists. The rest of the arguments are:
      {ul
      {- [stdin] reads input from the given file. If unspecified reads
         from the standard input of the program running the build.  {b
         Warning.} The file is not automatically added to [reads],
         this allows for example to use {!B00_std.Fpath.null}.}
      {- [stdout] and [stderr], the redirections for the standard
         outputs of the command, see {!B000.Op.Spawn.stdo}. Path to files are
         created if needed. {b Warning.} File redirections
         are not automatically added to [writes]; this allows for example
         to use {!B00_std.Fpath.null}.}
      {- [success_exits] the exit codes that determine if the build operation
         is successful (defaults to [0], use [[]] to always succeed)}
      {- [env], environment variables added to the build environment.
         This overrides environment variables read by the tool in the
         build environment except for forced one. It also allows to
         specify environment that may not be mentioned by the running
         tool's {{!Tool.v}environment specification}.}
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
         executed with the exit code.}
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
    ?writes:(B000.Op.t -> Fpath.t list) ->
    ?env:Os.Env.t -> ?cwd:Fpath.t -> ?stdin:Fpath.t ->
    ?stdout:B000.Op.Spawn.stdo -> ?stderr:B000.Op.Spawn.stdo ->
    ?success_exits:B000.Op.Spawn.success_exits ->
    ?k:(int -> unit) -> cmd -> unit
  (** [spawn'] is like {!val-spawn} except the actual file paths
      written by the spawn need not be determined before the
      spawn. Only the root directory of writes need to be specified
      via [writes_root].  After the spawn executes the writes can be
      determined via the [writes] function, the returned paths must be
      absolute and be prefixed by [writes_root] (defaults to
      recursively list all the files rootet in [writes_root]). *)
end

(** Lazy immutable stores.

    These stores provide access to immutable, lazily determined, typed
    key-value bindings.

    The value of a key in a store is defined either:
    {ul
    {- Explicitly when the store is {{!Store.create}created}.}
    {- Lazily on the first key {{!Store.get}access} via a key determination
       function
       specified at {{!Store.val-key}key creation time}.}}
    Once determined the value of a key in the store never changes.

    {b XXX.} Maybe move that at the B0 level. *)
module Store : sig

  (** {1:stores Stores} *)

  type 'a key
  (** The type for keys binding values of type ['a]. *)

  type binding = B : 'a key * 'a -> binding (** *)
  (** The type for store bindings. A key and its value. *)

  type t
  (** The type for stores. *)

  val create : Memo.t -> dir:Fpath.t -> binding list -> t
  (** [create memo ~dir bs] is a store with predefined bindings [bs].
      If a key is mentioned more than once in [bs] the last binding
      takes over. The store uses [memo] to determine other keys as
      {{!get}needed}.  [dir] is a scratch directory used by key determination
      functions to write memoized file outputs. *)

  val memo : t -> Memo.t
  (** [memo s] is [s]'s memo as given on {!create}. *)

  val dir : t -> Fpath.t
  (** [dir s] is the scratch directory of [s]. Key determination functions
      using this directory to write files should do so using nice file name
      prefixes (e.g. lowercased module or lib names) to avoid name
      clashes. *)

  val key : ?mark:string -> (t -> Memo.t -> 'a Fut.t) -> 'a key
  (** [key ~mark det] is a new key whose value is determined on
      {{!get}access} by the future:
{[
det s (Memo.with_mark mark (Store.memo s))
]}
      [mark] defaults to [""]. *)

  val get : t -> 'a key -> 'a Fut.t
  (** [get s k] is a future that dermines with the value of [k] in
      [s]. *)

(**/**)
  val set : t -> 'a key -> 'a -> unit
  (** [set s k v] sets value [k] to [v] in [s]. {b Warning.} In general
      this should not be used but it may be useful to initialize the
      store. In particular this will raise [Invalid_argument] if [k] is
      already set in [s]. *)
(**/**)
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
