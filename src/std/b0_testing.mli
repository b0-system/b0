(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unit, random and snapshot testing for OCaml.

    {{!example}This example} is a blueprint for a test executable. See
    also the {{!page-testing}b0 testing manual} to integrate your
    tests in your [B0.ml] file and run them with [b0 test].

    This module is designed to be opened.

    {b Warning.} As it stands the module is not thread safe. It will
    be made thread safe in the future. *)

open B0_std

(** Testing structure and assertions.

    See an {{!example}example}. *)
module Test : sig

  (** {1:locs Source locations} *)

  type loc = string * int * int * int
  (** The type for test source locations. This is the type of
      {!Stdlib.__POS_OF__}.

      It is an optional argument of most combinators to keep track of precise
      locations of failures. The idea is that the argument is simply used
      with {!Stdlib.__POS__}. For example:
      {[Test.string s0 s1 ~__POS__]} *)

  (** {1:log Logging} *)

  type 'a log = ?__POS__:loc -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** The type for log functions. If [__POS__] is specified starts by
      logging the position on a line before logging the format. *)

  val log : 'a log
  (** [log fmt …] logs a message formatted by [fmt]. *)

  val log_pass : 'a log
  (** [log_pass fmt …] is like {!log} but formatted for passing.
      This does not increment passed assertions, use {!pass} to do so. *)

  val log_fail : 'a log
  (** [log_fail fmt …] is like {!log} but formatted for failures. This
      does not increment failed assertions, use {!fail} to register a
      failure. *)

  val log_start : 'a log
  (** [log_start] is like {!log} but does not finish the line. {!log_end}
      does. *)

  val log_finish : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log_finish] ends a line started with {!log_start}. *)

  val log_raw : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log_raw fmt …] outputs to the test log. *)

  (** {1:main_tests Main & tests} *)

  val main : ?doc:string -> ?name:string -> (unit -> unit) -> int
  (** [main f] executes [f ()], logs the resulting testing status and
      returns [0] if all tests passed and [1] otherwise to be given to
      {!Stdlib.exit}. Usually [f] calls functions that call {!test}.
      {ul
      {- [doc] is a synopsis for the test executable.}
      {- [name] is a name for the test executable.}}
      See an {{!example}example}. *)

  val main' :
    ?man:Cmdliner.Manpage.block list ->
    ?doc:string -> ?name:string -> 'a Cmdliner.Term.t -> ('a -> unit) -> int
  (** [main'] is like {!main} but allows to define your own additional
      command line. *)

  val set_main_exit : (unit -> int) -> unit
  (** [set_main_exit f] will disable the final report performed
      by {!main} and invoke [f] instead. {b TODO.} A bit ad-hoc. *)

  (** Test arguments. *)
  module Arg : sig
    type 'a t
    (** The type for arguments of type ['a]. *)

    type value
    (** The type for argument values. *)

    val make : unit -> 'a t
    (** [make ()] is a new test argument. *)

    val value : 'a t -> 'a -> value
    (** [value arg v] is the argument [arg]  value [v]. *)
  end

  val test : ?long:bool -> string -> (unit -> unit) -> (unit -> unit)
  (** [test name f] is a test that when run logs ["Test %s" name] and
      executes [f ()]. The test fails if any {{!flow}failure} is
      reported in [f] or if [f] raises an unexpected exception. If
      [long] is true, this is a long test an only run when explicitely
      requested.  See {{!example}examples}.  *)

  val test' : 'a Arg.t -> ?long:bool -> string -> ('a -> unit) -> ('a -> unit)
  (** [test' arg] is like {!test} but takes an argument. *)

  val autorun : ?args:Arg.value list -> unit -> unit
  (** [autorun ()] calls all the tests defined by {!test} and
      {!test'}, possibly filtered by the command line. For the latter
      an argument value should be provided in [args] (defaults to
      [[]]). *)

  (** {1:flow Stop, pass, skip and fail}

      A {!test} is usually made of many assertions. If an assertion fails
      the test fails. *)

  val stop : unit -> 'a
  (** [stop ()] stops the current {{!block}block}, {{!test}test}, or
      {!main}. Note that this does not increment failed assertions,
      use {!fail} or {!failstop} for that. *)

  val skip : 'a log
  (** [skip ()] skips the current {{!block}block}, {{!test}test}, or
      {!main}. *)

  val pass : unit -> unit
  (** [pass ()] increments the number of successfull assertions.  *)

  val fail : 'a log
  (** [fail fmt …] increments the number of failed assertions and logs
      [fmt] with {!log_fail}. The test or block continues to execute, use
      {!stop} or {!failstop} to stop the test. *)

  val failstop : ?__POS__:loc -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [failf fmt …] is like {!fail} but also {!stop}s the test. *)

  (** {2:blocks Blocks and loops}

      Blocks and loops can be used as larger sub units of {!test}
      which you can {!stop} and {!failstop} without stopping the test
      itself. *)

  val block :
    ?fail:(?__POS__:loc -> int -> assertions:int -> unit) -> ?__POS__:loc ->
    (unit -> unit) -> unit
  (** [block ~fail f] runs [f ()], if that results in [n > 0] failed
      assertions, [fail n ~assertions] is called afterwards with
      [assertoins] the total number of assertions that were performed.
      If [f] peforms a {!failstop} the block is stopped but not the
      test. It is possible to fail stop the test by stoppping in
      [fail].

      {b TODO.} Should we simply make blocks nested {!test}s ? *)

  val range :
    ?kind:string -> first:int -> last:int -> ?__POS__:loc -> (int -> unit) ->
    unit
  (** [range ~first ~last f] calls [f n] with [n] ranging over the
      range \[[first];[last]\]. If a failure occurs for [n],
      logs ["%s in range [%d;%d] failed on %d" kind first last n].
      If [f] performs a {!failstop} the loop is stopped but not the test.
      [kind] defaults to ["Test"]. *)

  (** {1:testers Testers} *)

  (** The type for testable values. *)
  module type T = sig
    type t
    (** The tested type. *)

    val equal : t -> t -> bool
    (** [equal] asserts the equality of two values. *)

    val pp : t Fmt.t
    (** [pp] formats values for inspection. *)
  end

  (** Testers.

      A tester simply packs an equality function and a formatter
      to print values.

      {b Important.} We indicate when the printers respect the OCaml syntax.
      Only these testers are suitable for use with {!Test.snap}. *)
  module T : sig

    (** {1:testers Testers} *)

    type 'a t = (module T with type t = 'a)
    (** The type for testing values of type ['a]. *)

    val make : ?equal:('a -> 'a -> bool) -> ?pp:'a Fmt.t -> unit -> 'a t
    (** [make ~equal ~pp ()] is a tester using [equal] to assert
        values and [pp] to inspect them. Note the following points:
        {ol
        {- If your module [M] implement {!T}  it can directly be used as an
           equality tester value as [(module M)], you don't need to call
           this function.}
        {- If you want to use [M] with {!Test.snap} note that it
           expects [M.pp] to output valid OCaml syntax. If that is not
           the case you can redefine it with {!with_tester}.}
        {- By convention the first argument of [equal] is the value to
           test and the second value is the reference value. This is
           somtimes used by the combinators.}} *)

    val equal : 'a t -> 'a -> 'a -> bool
    (** [equal t] is the equality function of [t]. *)

    val pp : 'a t -> 'a Fmt.t
    (** [pp t] is the formatting function of [t]. *)

    val with' : ?equal:('a -> 'a -> bool) -> ?pp:'a Fmt.t -> 'a t -> 'a t
    (** [with' t] is [t] with those arguments specified
        replaced. *)

    (** {1:predef Predefined testers} *)

    (** {2:generic Generic} *)

    val true' : 'a t
    (** [true'] equates all values and prints them with {!Test.Fmt.anon}. *)

    val false' : 'a t
    (** [false'] distinguishes all values and prints them with
        {!Test.Fmt.anon}. *)

    val any : 'a t
    (** [any] uses {!Stdlib.compare} for testing equality
        (works {!Float.nan} values) and prints them with {!Test.Fmt.anon}. *)

    (** {2:exn Exceptions} *)

    val exn : exn t
    (** [exn] uses {!Stdlib.compare} for testing equality and prints
        with {!B0_std.Fmt.exn}. *)

    val invalid_arg : exn t
    (** [invalid_arg] has an equality function that returns [true] iff
        the first argument is of the form [Invalid_argument _]. *)

    val failure : exn t
    (** [failure] has an equality function that returns [true] iff
        the first argument is of the form [Failure _]. *)

    (** {2:base Base types} *)

    val unit : unit t
    (** [unit] tests units and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.unit}. *)

    val bool : bool t
    (** [bool] tests booleans and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.bool}. *)

    val int : int t
    (** [int] tests integers and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.int}. *)

    val int32 : int32 t
    (** [int32] tests 32-bit integers and prints them as OCaml syntax
        with {!B0_std.Fmt.Lit.int32}. *)

    val uint32 : int32 t
    (** [uint32] tests unsigned 32-bit integers and prints them with
        {!B0_std.Fmt.Lit.uint32} *)

    val int64 : int64 t
    (** [int64] tests 64-bit integers and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.int64}. *)

    val uint64 : int64 t
    (** [uint64] tests unsigned 64-bit integers and prints them as OCaml
        syntax with {!B0_std.Fmt.Lit.uint64}. *)

    val nativeint : nativeint t
    (** [nativeint] tests native integers and prints them as OCaml syntax
        with {!Fmt.Lit.nativeint}. *)

    val nativeuint : nativeint t
    (** [nativeuint] tests unsigned native integers and prints them
        as OCaml syntax with {!B0_std.Fmt.Lit.nativeuint}. *)

    val float : float t
    (** [float] tests float with {!Float.equal} so it can be used
        to assert [nan] values and prints as OCaml syntax
        them with {!B0_std.Fmt.Lit.floatl}.

        {b Warning.} Be aware of the limitations of testing floats
        for strict binary equality. *)

    val hex_float : float t
    (** [hex_float] is like {!float} but uses the OCaml syntax
        {!B0_std.Fmt.Lit.hex_float} for printing. *)

    (** {2:strings Characters and strings} *)

    val char : char t
    (** [char] tests characters (bytes) and prints them as OCaml syntax
        with {!B0_std.Fmt.Lit.char}. *)

    val ascii_string : string t
    (** [ascii_string] tests strings and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.ascii_string}. *)

    val string : string t
    (** [string] tests strings and prints them as OCaml syntax with
        {!B0_std.Fmt.Lit.val-string} *)

    val binary_string : string t
    (** [binary_string] tests strings and prints them with as OCaml syntax
        with {!B0_std.Fmt.Lit.binary_string}. *)

    val styled_string : string t
    (** [styled_string] tests strings and prints them with
        with {!B0_std.Fmt.styled_text_string}. *)

    val lines : string t
    (** [lines] tests strings and prints them by lines using
        {!B0_std.Fmt.lines}. *)

    val bytes : bytes t
    (** [bytes] tests bytes and prints them with
        {!B0_std.Fmt.bytes}. *)

    (** {2:parametric Parametric types} *)

    val option : 'a t -> 'a option t
    (** [option v] tests options of values tested with [v] and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.option}. *)

    val either : left:'a t -> right:'b t -> ('a, 'b) Either.t t
    (** [either ~left ~right] tests [Either.t] values and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.either}. *)

    val result : ok:'a t -> ('a, string) result t
    (** [result ok] tests result values using [ok] for
        [Ok] and a {!string} for [Error] and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.result}. *)

    val result' : ok:'a t -> error:'e t -> ('a, 'e) result t
    (** [result ok error] tests result values using the given
        equalities for the values of each cas and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.result}. *)

    val list : 'a t -> 'a list t
    (** [list elt] tests list of elements tested with [elt] and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.list}. *)

    val array : 'a t -> 'a array t
    (** [array elt] test array of elements tested with [elt] and prints
        them as OCaml syntax with {!B0_std.Fmt.Lit.array}. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair fst snd] tests pairs with [fst] and [snd] and prints them
        as OCaml syntax with {!B0_std.Fmt.Lit.pair}. *)

    val t2 : 'a t -> 'b t -> ('a * 'b) t
    (** [t2] is {!pair}. *)

    val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
    (** [t3] tests triplets and prints them as OCaml syntax with
        {!B0_std.Fmt.List.t3}. *)

    val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
    (** [t4] tests quadruplets and prints them as OCaml syntax with
        {!B0_std.Fmt.List.t4}. *)

    val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
    (** [t5] tests quintuplets and prints them as OCaml syntax with
        {!B0_std.Fmt.List.t5}. *)

    val t6 :
      'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
      ('a * 'b * 'c * 'd * 'e * 'f) t
    (** [t6] tests sextuplets and prints them as OCaml syntax with
        {!B0_std.Fmt.List.t6}. *)
  end

  (** Reporting value differences.

      In general the generic differs will do but depending
      on your application domain you may want to customize how
      differences on value mismatch are reported. For that devise
      a differ with {!make} and pass it to assertion functions. *)
  module Diff : sig

    (** {1:diffs Differs} *)

    type 'a t
    (** The type for differs of value of type ['a]. *)

    val make : ('a T.t -> fnd:'a -> exp:'a -> unit Fmt.t) -> 'a t
    (** [make render] reports diff by calling [render].
        When called [fnd] and [exp] are expected to be different as per
        tester equality, no need to retest them. *)

    val pp : ?diff:'a t -> 'a T.t -> fnd:'a -> exp:'a -> unit Fmt.t
    (** [pp ?diff t ~fnd ~exp] formats with [diff] the differences between
        [fnd] and [exp] which are expected to be different as per [t]'s
        equality. [diff] defaults to {!default}. *)

    (** {1:predef Predefined} *)

    val dumb : 'a t
    (** [dumb] is a dumb differ, it pretty prints the values according
        to tester and separates them by a [<>]. *)

    val of_cmd : Cmd.t -> 'a t
    (** [of_cmd cmd] is a differ from command [cmd] which must
        expect to receive [exp] and [fnd] as text files as the two
        first positional arguments in that order. The text files have rendering
        of the values with the test files to which a final newline is
        appended. *)

    val git : 'a t
    (** [git] diffs by pretty printing the value according to the
        value tester and diffing the results by shelling out to [git
        diff]. For now this is the default differ. *)

    val git_diff : fnd:string -> exp:string -> (string, string) result
    (** [git_diff ~fnd ~exp] provides access to the underlyling primitive
        used by {!git} to diff textual content [fnd] and [exp]. Note that
        [fnd] and [exp] are appended a newline before diffing them. *)

    (** {1:run_state Run state} *)

    val default : unit -> 'a t
    (** [default ()] is the default differ for the test run. *)
  end

  (** Patching text and files.

      This is used by snapshot testing, see
      {!Snapshot.substitutions}. Normally you don't need to
      use that. *)
  module Patch : sig
    type subst =
      { first : int; (** First byte of range to replace. *)
        last : int; (** Last byte of range to replace. *)
        subst : string (** The data substituted in the range. *) }
    (** The type for text substitutions. *)

    type t
    (** The type for text file patches. *)

    val make : src:string -> t
    (** [make ~src] is an empty patch on source text [src]. *)

    val is_empty : t -> bool
    (** [is_empty p] is [true] iff [p] has no substitutions. *)

    val src : t -> string
    (** [src p] is the source text to patch. *)

    val substs : t -> subst list
    (** [substs p] are the substitutions to perform on [p]. *)

    val add_subst : t -> subst -> t
    (** [add_subst p s] adds substitution [s] to patch [p]. *)

    val apply : t -> string
    (** [apply p] is the text resulting from applying the substition
        of [p] to [src p]. *)

    (** {1:files Patching files} *)

    val get : Fpath.t -> t option
    (** [get file] is the patch for file [file]. This is [None]
        only if [file] did not exist, in which case an error messages
        has been logged. The first time you lookup [file] you get
        an empty patch. *)

    val update : Fpath.t -> t -> unit
    (** [update file p] updates the patch of file [file] to [p]. *)

    val write_files : unit -> unit
    (** [write_files ()] writes the patched files. {b Note.}
        if you are using {!Test.main}, this is done automatically. *)

    (** {1:run_state Run state} *)

    val src_root : unit -> Fpath.t option
    (** [src_root ()] if present it prefixed to {!Test.loc} path
        with {!Fpath.append} for looking up files. *)
  end

  (** {1:assertions Assertions} *)

  type 'a eq = ?diff:'a Diff.t -> ?__POS__:loc -> 'a -> 'a -> unit
  (** The type for functions aasserting equality. [diff] indicates
      how to report differences on mismatches. *)

  val eq : 'a T.t -> 'a eq
  (** [eq t fnd exp] assert [fnd] and [exp] are equal using tester [t].

      {b Convention.} When testing values we assume that the second
      value is the expected argument and the first one the value
      against which it is checked. *)

  val neq : 'a T.t -> 'a eq
  (** [neq t fnd exp] assert [fnd] and [exp] are not equal using
      tester [t]. *)

  val holds : ?msg:string -> ?__POS__:loc -> bool -> unit
  (** [holds b] asserts that [b] is [true]. [msg] is logged if the
      assertion fails. *)

  (** {2:test_exns Exceptions} *)

  val noraise : ?__POS__:loc -> (unit -> 'a) -> 'a
  (** [noraise f] tests that [f ()] does not raise an exception
      and fail stops if it does except if this is a
      {!B0_std.Os.exn_don't_catch} exception. *)

  val catch :
    ?ret:'a T.t -> ?__POS__:loc -> (unit -> 'a) -> (exn -> unit) -> unit
  (** [catch f k] calls [f ()] and gives the exception it raises to
      [k] or fails otherwise. [ret] is used to print an offending
      returned value. {!B0_std.Os.exn_don't_catch} exceptions are not
      catched.  *)

  val exn : exn eq
  (** [ex ] is [eq ]{!T.exn}. *)

  val raises :
    ?ret:'a T.t -> ?exn:exn T.t -> ?diff:exn Diff.t ->
    ?__POS__:loc -> exn -> (unit -> 'a) -> unit
  (** [raises exn f] tests that [f ()] raises exception [exn].
      {!B0_std.Os.exn_don't_catch} exceptions are not catched.  *)

  val invalid_arg :
    ?ret:'a T.t -> ?diff:exn Diff.t -> ?__POS__:loc -> (unit -> 'a) -> unit
  (** [invalid_arg f] tests that [f ()] raises [Invalid_argument _].
      {b Note.} It is usually better to use {!Snap.raise} as it allows
      to keep track of the error message. *)

  val failure :
    ?ret:'a T.t -> ?diff:exn Diff.t -> ?__POS__:loc -> (unit -> 'a) -> unit
  (** [failure f] tests that [f ()] raises [Failure _].
      {b Note.} It is usually better to use {!Snap.raise} as it allows
      to keep track of the error message. *)

  (** {2:test_base_types Base types} *)

  val unit : unit eq
  (** [unit] is [eq ]{!T.unit}. *)

  val bool : bool eq
  (** [bool] is [eq ]{!T.bool}. *)

  val int : int eq
  (** [int] is [eq ]{!T.int}. *)

  val int32 : int32 eq
  (** [int32] is [eq ]{!T.int32}. *)

  val uint32 : int32 eq
  (** [uint32] is [eq ]{!T.uint32}. *)

  val int64 : int64 eq
  (** [int64] is [eq ]{!T.int64}. *)

  val uint64 : int64 eq
  (** [uint64] is [eq ]{!T.uint64}. *)

  val nativeint : nativeint eq
  (** [nativeint] is [eq ]{!T.nativeint}. *)

  val nativeuint : nativeint eq
  (** [nativeuint] is [eq ]{!T.nativeuint}. *)

  val float : float eq
  (** [float] is [eq ]{!T.float}. *)

  (** {2:chars_and_strings Character and strings} *)

  val char : char eq
  (** [char] is [eq ]{!T.char}. *)

  val string : string eq
  (** [string] is [eq ]{!T.string}. Assumes UTF-8 encoded strings. *)

  val lines : string eq
  (** [lines] is [eq ]{!T.lines}. Assumes UTF-8 encoded strings. *)

  val binary_string : string eq
  (** [binary_string] is [eq ]{!T.binary_string}. Assumes arbitrary binary
      strings. *)

  val styled_string : string eq
  (** [styled_string] is [eq ]{!T.styled_string}. Assumes strings with ANSI
      escape sequences. *)

  val bytes : bytes eq
  (** [string] is [eq ]{!T.bytes}. *)

  (** {2:parametric Parametric types} *)

  val any : 'a eq
  (** [any] uses {!T.any}. *)

  val option : 'a T.t -> 'a option eq
  (** [option some] is [eq (]{!T.option}[ some)]. *)

  val either : left:'a T.t -> right:'b T.t -> ('a, 'b) Either.t eq
  (** [either ~left ~right] is [eq (]{!T.either}[ ~left ~right)]. *)

  val result : ok:'a T.t -> ('a, string) result eq
  (** [result ~ok] is [eq (]{!T.result}[ ~ok)]. *)

  val result' : ok:'a T.t -> error:'e T.t -> ('a, 'e) result eq
  (** [result' ~ok ~error] is [eq (]{!T.result'}[ ~ok ~error)]. *)

  val list : 'a T.t -> 'a list eq
  (** [list elt] is [eq (]{!T.list}[ elt)]. *)

  val array : 'a T.t -> 'a array eq
  (** [array elt] is [eq (]{!T.array}[ elt)]. *)

  val pair : 'a T.t -> 'b T.t -> ('a * 'b) eq
  (** [pair fst snd] is [eq (]{!T.pair}[ fst snd)]. *)

  val t2 : 'a T.t -> 'b T.t -> ('a * 'b) eq
  (** [t2] is {!pair}. *)

  val t3 : 'a T.t -> 'b T.t -> 'c T.t -> ('a * 'b * 'c) eq
  (** [t3] is {!eq} for triplets. *)

  val t4 : 'a T.t -> 'b T.t -> 'c T.t -> 'd T.t -> ('a * 'b * 'c * 'd) eq
  (** [t4] is {!eq} for quadruplets. *)

  val t5 :
    'a T.t -> 'b T.t -> 'c T.t -> 'd T.t -> 'e T.t ->
    ('a * 'b * 'c * 'd * 'e) eq
  (** [t5] is {!eq} for quintuplets. *)

  val t6 :
    'a T.t -> 'b T.t -> 'c T.t -> 'd T.t -> 'e T.t -> 'f T.t ->
    ('a * 'b * 'c * 'd * 'e * 'f) eq
  (** [t6] is {!eq} for sextuplets. *)

  (** {1:random Randomized testing} *)

  (** Randomized testing. *)
  module Rand : sig

    (** {1:run_state Run state} *)

    val state : unit -> Random.State.t
    (** [state ()] returns a random state. This is self seeded unless
        the [SEED] environment variable is set in which case an
        integer. *)
  end

  (** {1:snapshot Snapshot testing} *)

  (** Snapshot tests. *)
  module Snapshot : sig

    (** {1:snapshots Snaphots} *)

    type 'a t = loc * 'a
    (** The type for expected snapshots of type 'a.

        {b Warning.} Do not rely on this being a structural type use
        {!loc} and {!value} to deconstruct it. *)

    val loc : 'a t -> loc
    (** [loc s] is the location of the expected snapshot. *)

    val value : 'a t -> 'a
    (** [value s] is the value of the expected snapshot. *)

    (** {1:substitutions Substitutions} *)

    type 'a subst =
      'a T.t -> 'a t -> by:'a -> src:string -> Patch.subst
    (** The type for snapshot substitution functions, returns a substitution
        to perform in [src]. Raise [Failure] if you can't. *)

    val generic_subst : 'a subst
    (** [generic_subst] is a generic snapshot substution function.
        It uses the given tester's pretty printer to print the new
        snapshot which must result in valid OCaml syntax. *)

    val string_subst : string subst
    (** [string_subst] is a special snapshot substitution function for
        [string] literals. It looks in the context to preserve quoted
        literal strings. *)

    (** {1:run_state Test run state} *)

    val correct : unit -> bool
    (** [correct ()] is [true] if expected snapshot mismatches must
        be updated to the snapshot computed during the run. *)

    val force_correct : unit -> bool
    (** [force_correct ()] is [true] if all expected snapshots must be
        be updated to the snapshots computed during the run,
        regardless of their correctness. *)
  end

  type 'a snap = ?diff:'a Diff.t -> 'a -> 'a Snapshot.t -> unit
  (** The type for snapshot functions. *)

  val snap : ?subst:'a Snapshot.subst -> 'a T.t -> 'a snap
  (** [snap t fnd @@ __POS_OF__ exp] compares snapshot [fnd] to [exp].
      [subst] is used to correct snapshots, it defaults to
      {!Snapshot.generic_subst} which depends on [t]'s pretty printer
      formatting valid OCaml code.

       See {!B0_testing.Snap} for preapplied combinators. *)

  (** {1:low Low-level modules}

      These modules are not needed for basic [B0_testing] usage. *)

  (** {2:cli Command line arguments} *)

  (** Command line. *)
  module Cli : sig
    val s_common_options : string
    (** [s_common_options] is the manpage section name where common
        options are listed. *)

    val setup : unit Cmdliner.Term.t
    (** [setup] has a command line to setup {!B0_testing}. If
        you use {!main} more information can be obtained by running
        your tool with [--help]. But in a nutshell:
        {ul
        {- [--seed] and [SEED] to specify the seed.}
        {- [--correct] and [CORRECT] to indicate snapshot correction.}
        {- [--force-correct] to force expected snapshots to update.}
        {- [--diff-cmd] to set the external command used for diffing}
        {- [--src-root] to resolve relative source file paths}
        {- [-i NAME] and [-x NAME] to select tests}} *)
  end

  (** {2:fmt Formatters} *)

  (** Formatters for test runners and tested values. *)
  module Fmt : sig
    val padding : string
    val fail_color : Fmt.style list
    val skip_color : Fmt.style list
    val test : unit Fmt.t
    val fail : unit Fmt.t
    val pass : unit Fmt.t
    val skip : unit Fmt.t
    val passed : unit Fmt.t
    val failed : unit Fmt.t
    val skipped : unit Fmt.t
    val dur : Mtime.Span.t Fmt.t
    val fail_count : int Fmt.t
    val count : int Fmt.t
    val fail_count_ratio : (int * int) Fmt.t
    val loc : loc Fmt.t
    val anon : 'a Fmt.t
  end
end

(** Snapshots.

    These combinators can be used to snapshot values.  They are based
    on the {!Test.val-snap} primitive.

    While we wait for {{:https://github.com/ocaml/RFCs/pull/52}implicit
    source location} support to be integrated in
    the compiler they are expected to be used as follows:
    {[
    Snap.bool (Bool.not true) @@ __POS_OF__ false;
    Snap.string ("a" ^ "b") @@ __POS_OF__
      "ab";
    ]} *)
module Snap : sig

  (** Repeated here for convenience, e.g. [Snap.(list T.int)].  *)
  module T = Test.T

  (** {1:exn Exceptions} *)

  val exn : exn Test.snap
  (** [exn] is {!Test.val-snap}[ ]{!Test.module-T.snap}. *)

  val raise :
    ?ret:'a Test.T.t -> ?exn:exn Test.T.t -> ?diff:exn Test.Diff.t ->
    ?__POS__:Test.loc -> (unit -> 'a) -> exn Test.Snapshot.t -> unit
  (** [raise f] snapshots the exception raised by [f] by printing
      it using [exn] (defaults to {!T.exn}). The combinators
      fails if no exception is raised by [f]. *)

  (** {1:base Base types} *)

  val unit : unit Test.snap
  (** [unit] is {!Test.val-snap}[ ]{!Test.module-T.unit}. *)

  val bool : bool Test.snap
  (** [bool] is {!Test.val-snap}[ ]{!Test.module-T.bool}. *)

  val int : int Test.snap
  (** [int] is {!Test.val-snap}[ ]{!Test.module-T.int}. {b Warning.} Depending
      on their magnitude integer snapshots may not be portable across 64-bit
      and 32-bit platforms. *)

  val int32 : int32 Test.snap
  (** [int32] is {!Test.val-snap}[ ]{!Test.module-T.int32} *)

  val uint32 : int32 Test.snap
  (** [uint32] is {!Test.val-snap}[ ]{!Test.module-T.uint32} *)

  val int64 : int64 Test.snap
  (** [int64] is {!Test.val-snap}[ ]{!Test.module-T.int64} *)

  val uint64 : int64 Test.snap
  (** [uint64] is {!Test.val-snap}[ ]{!Test.module-T.uint64} *)

  val nativeint : nativeint Test.snap
  (** [nativeint] is {!Test.val-snap}[ ]{!Test.module-T.nativeint} *)

  val nativeuint : nativeint Test.snap
  (** [nativeuint] is {!Test.val-snap}[ ]{!Test.module-T.nativeuint} *)

  val float : float Test.snap
  (** [float] is {!Test.val-snap}[ ]{!Test.module-T.float}. N.B. not
      exactly. *)

  val hex_float : float Test.snap
  (** [hex_float] is {!Test.val-snap}[ ]{!Test.module-T.hex_float}
      N.B. not exactly. *)

  (** {2:chars_and_strings Character and strings}

      {b Note.} The string snapshoters have a special
      substitution function that respects quoted literals.
  *)

  val char : char Test.snap
  (** [char] is {!Test.val-snap}[ ]{!Test.module-T.char}. *)

  val ascii_string : string Test.snap
  val string : string Test.snap

  val line : string Test.snap
  (** like {!string} but on a single line. *)

  val lines : string Test.snap
  (** [lines] uses {!Test.module-T.lines} to produce the diffs. *)

  (*
  val binary_string : string eq
  (** [binary_string fnd exp] asserts that [fnd] and [exp] are equal assuming
      that [fnd] and [exp] is binary data. *)

  val styled_string : string eq
  (** [styled_string fnd exp] asserts that [fnd] and [exp] are equal assuming
      that [fnd] and [exp] have ANSI escape sequences. *)

  val bytes : bytes eq
  (** [string fnd exp] asserts that [fnd] and [exp] are equal. *)
*)

  (** {1:parametric Parametric types} *)

  val option : 'a Test.T.t -> 'a option Test.snap
  (** [option t] snapshots [t] option. *)

  val either : 'a Test.T.t -> 'b Test.T.t -> ('a, 'b) Either.t Test.snap
  (** [result left right] is
      {!Test.val-snap}[ (]{!Test.module-T.either}[ left right)] *)

  val result : 'a Test.T.t -> ('a, string) result Test.snap
  (** [result ok] is {!Test.val-snap}[ (]{!Test.module-T.result}[ ok)] *)

  val result' : 'a Test.T.t -> 'e Test.T.t -> ('a, 'e) result Test.snap
  (** [result ok error] is
      {!Test.val-snap}[ (]{!Test.module-T.result}[ ok error)] *)

  val list : 'a Test.T.t -> 'a list Test.snap
  (** [list elt] is {!Test.val-snap}[ (]{!Test.module-T.list}[ elt)] *)

  val array : 'a Test.T.t -> 'a array Test.snap
  (** [array elt] is {!Test.val-snap}[ (]{!Test.module-T.array}[ elt)] *)

  val pair : 'a Test.T.t -> 'b Test.T.t -> ('a * 'b) Test.snap
  (** [pair fst snd] is {!Test.val-snap}[ (]{!Test.module-T.pair}[ fst snd)] *)

  val t2 : 'a Test.T.t -> 'b Test.T.t -> ('a * 'b) Test.snap
  (** [t2] is {!pair}. *)

  val t3 : 'a Test.T.t -> 'b Test.T.t -> 'c Test.T.t -> ('a * 'b * 'c) Test.snap
  (** [t3 v0 v1 v2] is {!Test.val-snap} for triplets. *)

  val t4 :
    'a Test.T.t -> 'b Test.T.t -> 'c Test.T.t -> 'd Test.T.t ->
    ('a * 'b * 'c * 'd) Test.snap
  (** [t4] is {!Test.val-snap} for quadruplets. *)

  val t5 :
    'a Test.T.t -> 'b Test.T.t -> 'c Test.T.t -> 'd Test.T.t -> 'e Test.T.t ->
    ('a * 'b * 'c * 'd * 'e) Test.snap
  (** [t5] is {!Test.val-snap} for quintuplets. *)

  val t6 :
    'a Test.T.t -> 'b Test.T.t -> 'c Test.T.t -> 'd Test.T.t -> 'e Test.T.t ->
    'f Test.T.t -> ('a * 'b * 'c * 'd * 'e * 'f) Test.snap
  (** [t6] is {!Test.val-snap} for sextuplets. *)

  (** {1:exec Command executions} *)

  val stdout :
    ?__POS__:Test.loc ->
    ?diff:string Test.Diff.t -> ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
    ?stdin:Os.Cmd.stdi -> ?stderr:[`Stdo of Os.Cmd.stdo | `Out] ->
    trim:bool -> Cmd.t -> string Test.Snapshot.t -> unit
  (** [stdout cmd] snaphosts the standard output of the execution (see
      {!Os.run_status_out}). The function {!Test.fails} if there's
      any sort of error. The [status] is ignored (FIXME do something
      more sensitive we could assert it)). *)
end

val ( !! ) : ?loc:Test.loc -> 'a -> 'a Test.Snapshot.t
(** This doesn't work for now, but is what we would like to use
    in the future. Use {!Stdlib.__POS_OF__} for now see {!Snap}. *)

(** {1:example Example}

    This is a typical test executable. Note that tests
    are [let] bound. This mans that you can load them in the toplevel
    to invoke them manually.

{[
let test_string_get =
  Test.test "String.get" @@ fun () ->
  Test.char (String.get "a" 0) 'a' ~__POS__;
  Snap.char (String.get "ab" 1) @@ __POS_OF__ 'b';
  Snap.raise (fun () -> String.get "" 0) @@ __POS_OF__
    (Invalid_argument("index out of bounds"));
  ()

let test_list_map =
  Test.test "List.map" @@ fun () ->
  Test.(list T.int) (List.map succ [1; 2; 3]) [2; 3; 4] ~__POS__;
  Snap.(list T.int) (List.map succ [1; 2; 3]) @@ __POS_OF__
    [2; 3; 4];
  ()

let test_randomized =
  Test.test "Randomized" @@ fun () ->
  let rstate = Test.Rand.state () in
  let n = Random.State.int rstate 10 in
  let a = Array.init (n + 1) Fun.id in
  let asum = (n * (n + 1)) / 2 in
  Test.int (Array.fold_left ( + ) 0 a) asum ~__POS__;
  ()

let test_snapshots =
  Test.test "Snapshots" @@ fun () ->
  Snap.string (String.concat "," ["a";"b";"c"]) @@ __POS_OF__
    "a,b,c";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
]}

  Note that if you are in a testing hurry or migrating you can always
  simply use OCaml's [assert] in the test functions: you just won't
  get any rendering on assertion failures and tests stop at the first
  assertion failure.

  {b TODO.} Show how to use arguments.
*)
