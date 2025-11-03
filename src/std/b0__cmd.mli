(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines.

    Command line values specify the command line arguments given to
    tool spawns. Depending on the context this may represent either
    only tool arguments or the full command specification with the
    tool to spawn as the first argument.

    See {{!Cmd.examples}examples}.

    @canonical B0_std.Cmd *)

(** {1:cl Command lines} *)

type t
(** The type for command lines.

    A command line is a list of command line arguments. The first
    argument usually denotes the {{!section-tool}tool or executable}
    to invoke. *)

val is_empty : t -> bool
(** [is_empty cmd] is [true] iff [cmd] is an empty list of arguments. *)

val empty : t
(** [empty] is an empty list of arguments. *)

val arg : string -> t
(** [arg a] is the atomic argument [a]. *)

val append : t -> t -> t
(** [append cmd1 cmd2] appends arguments [cmd2] to [cmd1]. *)

(** {1:derived Derived combinators} *)

val ( % ) : t -> string -> t
(** [cmd % a] is [append cmd (arg a)]. *)

val ( %% ) : t -> t -> t
(** [cmd1 %% cmd2] is [append cmd1 cmd2]. *)

val if' : bool -> t -> t
(** [if' cond cmd] is [cmd] if [cond] is [true] and {!empty} otherwise. *)

val if_some : t option -> t
(** [if_some o] is [cmd] if [o] is [Some cmd] and {!empty} otherwise. *)

val int : int -> t
(** [int i] is [arg (string_of_int i)]. *)

val float : float -> t
(** [float f] is [arg (float_of_int f)]. *)

val path : B0__fpath.t -> t
(** [path p] is [arg (Fpath.to_string p)]. *)

val list : ?slip:string -> string list -> t
(** [list ?slip l] is a command line from the list of arguments [l].
    If [slip] is specified it is added on the command line before
    each element of [l]. *)

val paths : ?slip:string -> B0__fpath.t list -> t
(** [paths ?slip ps] is {!of_list}[ ?slip Fpath.to_string ps]. *)

val of_list : ?slip:string -> ('a -> string) -> 'a list -> t
(** [of_list ?slip conv l] is {!list}[ ?slip (List.map conv l)]. *)

(** {1:tool Tools}

    Tools are the first argument of commands. *)

type tool = B0__fpath.t
(** The type for command line tools.

    A command line tool is represented by a file path according to
    the POSIX convention for [exec(3)]:

    {ul
    {- If it is made of a single segment, for example [Fpath.v "ocaml"], it
       represents a program name to be looked up via a search procedure;
       for example in the [PATH] environment variable.}
    {- If it is a file path with multiple segments (POSIX would say if they
       contain a slash character) the program is the file itself.}}

    {b Note.} For portability one should not use the [.exe] suffix on
    Windows on tools. This should be handled transparently by
    {!type-tool_search} procedures. *)

val tool : string -> t
(** [tool t] is [arg t], used for reading clarity. *)

val find_tool : t -> tool option
(** [find_tool cmd] is [cmd]'s first argument. This is [None] if the
    command is {!empty} or if the first element can't be parsed to a
    {!type-tool}. *)

val get_tool : t -> (tool, string) result
(** [get_tool] is like {!val-find_tool} but returns an english [Error msg] on
    [None]. *)

val set_tool : tool -> t -> t
(** [set_tool t cmd] replaces [cmd]'s first element with [t]. This
    is [path t] if [cmd] is {!empty}. *)

(** {2:tool_search Tool search} *)

type tool_search = t -> (t, string) result
(** The type for tool search functions.

    These are functions that resolve and {{!set_tool}set} the
    {!get_tool} argument of commands to a concrete program executable.
    Or return an error message if the tool cannot be resolved.
    See {!B0_std.Os.Cmd.section-tool_search}
    for implementations. *)

(** {1:preds Predicates} *)

val is_singleton : t -> bool
(** [is_singleton l] is [true] iff [l] has a single argument. *)

(** {1:converting Converting} *)

val fold :
  arg:(string -> 'a) -> unstamp:('a -> 'a) -> append:('a -> 'a -> 'a) ->
  empty:'a -> t -> 'a
(** [fold ~arg ~unstamp ~append ~empty l] folds over [l]'s structure. *)

val iter_enc :
  arg:('a -> string -> unit) ->
  unstamp:('a -> unit) ->
  append:('a -> unit) ->
  empty:('a -> unit) -> 'a -> t -> unit

val to_list : t -> string list
(** [to_list l] converts [l] to a list of strings. *)

val to_string : t -> string
(** [to_string l] converts [l] to a string that can be passed
    to the
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/system.html}
    [command(3)]} POSIX system call. *)

val of_string : string -> (t, string) result
(** [of_string s] tokenizes [s] into a command line. The tokens
    are recognized according to the [token] production of the following
    grammar which should be mostly be compatible with POSIX shell
    tokenization.
{v
white   ::= ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r'
squot   ::= '\''
dquot   ::= '\"'
bslash  ::= '\\'
tokens  ::= white+ tokens | token tokens | ϵ
token   ::= ([^squot dquot white] | squoted | dquoted) token | ϵ
squoted ::= squot [^squot]* squot
dquoted ::= dquot (qchar | [^dquot])* dquot
qchar   ::= bslash (bslash | dquot | '$' | '`' | '\n')
v}
    [qchar] are substitued by the byte they escape except for ['\n']
    which removes the backslash and newline from the byte stream.
    [squoted] and [dquoted] represent the bytes they enclose. *)

val pp_arg : string B0__fmt.t
(** [pp_arg] formats an argument [a]. The string is quoted with
    {!Filename.quote} iff it contains one of the characters mentioned
    in {{:https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_02}this
    section} of POSIX or if it is the empty string. *)

val pp : t B0__fmt.t
(** [pp] is an unspecified formatter for commands. *)

val pp_shell : t B0__fmt.t
(** [pp_shell] formats a command as a multiline shell command
    that can be cut and pasted. {b Note.} Currently this may
    overflow your boxes. *)

val pp_dump : t B0__fmt.t
(** [pp_dump] formats raw data for debugging. *)

(** {1:stamps Stamps}

    Stamps are not useful unless you are interested in memoizing
    tool invocations. A command stamp represents the part of
    the command line that influences a tool's output. By default
    arguments are part of the stamp however they can be selectively
    {!Cmd.unstamp}ed to remove them from the stamp.

    Unstamped arguments have no special semantics. As far as the
    command line is concerned they simply indicate that the argument
    value itself does not influence the outputs of the tool.
    Unstamped arguments do not appear in the command line
    {{!Cmd.to_list_and_stamp}stamp} which can be used as a key to
    memoize tool spawns.

    A typical example of unstamped arguments are file paths to inputs:
    it's often the file contents not the actual file path that
    determines the tool output; beware though that some tool use both
    the file path contents and the actual file path in their outputs
    (typically compilers which track source locations). See
    {{!examples}examples}. *)

val unstamp : t -> t
(** [unstamp cmd] indicates that arguments [cmd] do not influence the
    tool's invocation outputs. These arguments are omitted from
    the command line's {{!to_list_and_stamp}stamp}, see {!section-stamps}
    for more details and {{!examples}examples}. *)

val to_stamp : t -> string list
(** [to_stamp l] is the sequence of stamped arguments. *)

val to_list_and_stamp : t -> string list * string list
(** [to_list_and_stamp l] is a [l] as a list of strings tuppled with
    its stamp: the sequence of stamped arguments. *)

(** {1:examples Examples}

{[
let ls p = Cmd.(tool "ls" % "-a" % path p)

let tar archive dir =
  Cmd.(tool "tar" % "-cvf" %% unstamp (path archive) %% path dir)

let opam cmd = Cmd.(tool "opam" % cmd)
let opam_install pkgs = Cmd.(opam "install" %% list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(tool "ocamlc" % "-c" % if' debug (arg "-g") %% path file)

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(if' profile (arg "-p")) in
  let debug = Cmd.(if' debug (arg "-g")) in
  let incs = Cmd.(unstamp (paths ~slip:"-I" incs)) in
  Cmd.(tool "ocamlopt" % "-c" %% debug %% profile %% incs %%
       unstamp (path file))
]} *)
