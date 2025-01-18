(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Simple testing tools. *)

open B0_std

(** Testing structure and combinators.

    See an {{!example}example}.

    {b Warning.} Currently the testing combinators are not thread-safe.
    They will be made thread-safe when OCaml 5.0 is required. *)
module Test : sig

  (** {1:src Source positions} *)

  type pos = string * int * int * int
  (** The type for test source positions. This is the type of {!Stdlib.__POS__}.

      It is an optional argument of most combinators to short-circuit
      the best effort failure location tracking performed via backtraces
      which may be unreliable in certain scenarios (e.g. closures). The idea
      is that the argument is simply used with {!Stdlib.__POS__}. For
      example:
      {[
        Test.string ~__POS__ s0 s1;
      ]}
      Note that at the moment not all functions make use of the argument. *)

  (** {1:fmt Formatters} *)

  (** Formatters for test runners and tested values. *)
  module Fmt : sig
    val padding : string
    val test : unit Fmt.t
    val fail : unit Fmt.t
    val pass : unit Fmt.t
    val skip : unit Fmt.t
    val passed : unit Fmt.t
    val failed : unit Fmt.t
    val skipped : unit Fmt.t
    val dur : Mtime.Span.t Fmt.t
    val count : int Fmt.t
    val count_ratio : (int * int) Fmt.t
    val pos : pos Fmt.t
    val hex_string : string Fmt.t
    val list : 'a Fmt.t -> 'a list Fmt.t
    val option : 'a Fmt.t -> 'a option Fmt.t
    val result : ok:'a Fmt.t -> error:'e Fmt.t -> ('a, 'e) result Fmt.t
  end

  (** {1:structure Test structure} *)

  val main : ?__POS__:pos -> (unit -> unit) -> int
  (** [main f] executes [f ()], logs the resulting testing status and
      returns [0] if all tests passed and [1] otherwise for use with
      {!Stdlib.exit}. Usually [f] calls functions that call {!test}.
      See the {{!example}example}. *)

  val test : ?__POS__:pos -> string -> (unit -> unit) -> unit
  (** [test name f] logs ["Test %s" name] and executes [f ()].
      The test fails if any {{!checks}failure} is reported in [f] or
      if [f] raises an unexpected exception.  See the
      {{!example}example}.  *)

  val stop : ?__POS__:pos -> unit -> 'a
  (** [stop] stops the current {{!loops}loop}, {{!test}test}, or {!main}. Note
      that this does not increment failed checks, use {!fail} or {!failstop}
      for that. *)

  (** {1:checks Passing and failing checks}

      A {!test} is usually made of many checks. If a check fails the test
      fails. *)

  val pass : ?__POS__:pos -> unit -> unit
  (** [pass ()] increments the number of successfull checks.  *)

  val fail : ?__POS__:pos -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [failf fmt…] increments the number of failed checks. The test continues
      to execute, use {!stop} and {!failstop} to stop the test. *)

  val failstop : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [failf fmt…] is like {!fail} but {!stop}s the test. *)

  (** {1:loging Logging} *)

  val log : ?__POS__:pos -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log fmt …] logs a message formatted by [fmt]. *)

  val log_fail :
    ?__POS__:pos -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log_fail fmt …] is like {!log} but formatted for failures. This
      does not increment failed checks, use {!fail} to register a failure. *)

  (** {1:combinators Test combinators} *)

  (** {2:loops Loops and blocks} *)

  val range :
    ?kind:string -> first:int -> last:int -> ?__POS__:pos -> (int -> unit) ->
    unit
  (** [range ~first ~last f] calls [f n] with [n] ranging over the
      range \[[first];[last]\]. If a failure occurs for [n],
      logs ["%s in range [%d;%d] failed on %d" kind first last n].
      If [f] performs a {!failstop} the loop is stopped but not the test.
      [kind] defaults to ["Test"]. *)

  val block :
    ?fail:(?__POS__:pos -> int -> checks:int -> unit) -> ?__POS__:pos ->
    (unit -> unit) -> unit
  (** [block ~fail f] runs [f ()], if that results in [n > 0]
      failed checks, [fail n ~checks] is called afterwards with
      [checks] the total number of checks that were performed.  If [f]
      peforms a {!failstop} the block is stopped but not the test. It
      is possible to fail stop the test by stoppping in [fail]. *)

  (** {2:test_assert Assertions} *)

  val holds : ?msg:string -> ?__POS__:pos -> bool -> unit
  (** [holds b] asserts that [b] is [true]. [msg] is logged if the
      assertion fails. *)

  (** {2:test_exceptions Exceptions} *)

  val raises : exn -> ?__POS__:pos -> (unit -> 'a) -> unit
  (** [raises exn f] tests that [f ()] raises exception [exn]. *)

  val raises' : (exn -> bool) -> ?__POS__:pos -> (unit -> 'a) -> unit
  (** [raises is_exn f] tests that [f ()] raises an exception [exn]
      that satisfies [is_exn exn]. *)

  val invalid_arg : ?__POS__:pos -> (unit -> 'a) -> unit
  (** [invalid_arg f] tests that [f ()] raises [Invalid_argument _]. *)

  val failure : ?__POS__:pos -> (unit -> 'a) -> unit
  (** [failure f] tests that [f ()] raises [Failure _]. *)

  (** {2:test_values Values} *)

  (** Equality testers. *)
  module Eq : sig

    (** The type for types with equality and formatters. *)
    module type T = sig
      type t
      val equal : t -> t -> bool
      val pp : Format.formatter -> t -> unit
    end

    type 'a t = (module T with type t = 'a)
    (** The type for testing equality of values of type ['a]. *)

    val make :
      ?equal:('a -> 'a -> bool) -> ?pp:(Format.formatter -> 'a -> unit) ->
      unit -> 'a t
    (** [make ~equal ~pp ()] is an equality tester.

        {b Note.} If your module [M] implement {!T} (and most often
        should) it can directly be used as an equality tester as
        [(module M)], you don't need to call this function. *)

    val true' : 'a t
    (** [true'] equates all values. *)

    val false' : 'a t
    (** [false'] negates all values. *)

    val any : 'a t
    (** [any] uses {!Stdlib.compare} for testing values for equality
        (works on [nan] values). *)

    val unit : unit t
    (** [unit] tests units. *)

    val bool : bool t
    (** [bool] tests booleans. *)

    val char : char t
    (** [char] tests characters (bytes). *)

    val int : int t
    (** [int] tests integers. *)

    val int32 : int32 t
    (** [int32] tests 32-bit integers. *)

    val uint32 : int32 t
    (** [uint32] tests unsigned 32-bit integers. *)

    val int64 : int64 t
    (** [int64] tests 64-bit integers. *)

    val uint64 : int64 t
    (** [uint64] tests 64-bit integers. *)

    val nativeint : nativeint t
    (** [nativeint] tests a native integer. *)

    val float : float t
    (** [float] tests floats. {b Warning} This uses {!Float.equal}
        see {!Test.float}. *)

    val string : string t
    (** [string] tests textual strings. *)

    val binary_string : string t
    (** [binary_string] tests binary string. *)

    val styled_string : string t
    (** [styled_string] tests textual string styled with ANSI escape
        sequences. *)

    val bytes : bytes t
    (** [bytes] tests bytes. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair fst snd] tests pairs with [fst] and [snd]. *)

    val list : 'a t -> 'a list t
    (** [list elt] tests list of elements tested with [elt]. *)

    val array : 'a t -> 'a array t
    (** [array elt] test array of elements tested with [elt]. *)

    val option : 'a t -> 'a option t
    (** [option v] tests options of values tested with [v]. *)

    val result : ok:'a t -> error:'e t -> ('a, 'e) result t
    (** [result ~ok ~error] tests result values using the given
        equalities for the values of each case. *)
  end

  val eq : 'a Eq.t -> ?__POS__:pos -> 'a -> 'a -> unit
  (** [eq t v0 v1] assert [v0] and [v1] are equal are equal
      using equality tester [t]. *)

  val neq : 'a Eq.t -> ?__POS__:pos -> 'a -> 'a -> unit
  (** [neq t v0 v1] assert [v0] and [v1] are not equal using
      equality tester [t]. *)

  (** {2:test_stdlib Stdlib values} *)

  val bool : ?__POS__:pos -> bool -> bool -> unit
  (** [bool b0 b1] asserts that [b0] and [b1] are equal. *)

  val char : ?__POS__:pos -> char -> char -> unit
  (** [char c0 c1] asserta that [c0] and [c1] are equal. *)

  val int : ?__POS__:pos -> int -> int -> unit
  (** [int i0 i1] asserts that [i0] and [i1] are equal. *)

  val int32 : ?__POS__:pos -> int32 -> int32 -> unit
  (** [int32 i0 i1] asserts that [i0] and [i1] are equal. *)

  val uint32 : ?__POS__:pos -> int32 -> int32 -> unit
  (** [uint32 i0 i1] asserts that [i0] and [i1] are equal. *)

  val int64 : ?__POS__:pos -> int64 -> int64 -> unit
  (** [int64 i0 i1] asserts that [i0] and [i1] are equal. *)

  val uint64 : ?__POS__:pos -> int64 -> int64 -> unit
  (** [uint64 i0 i1] asserts that [i0] and [i1] are equal. *)

  val nativeint : ?__POS__:pos -> nativeint -> nativeint -> unit
  (** [nativeint i0 i1] asserts that [i0] and [i1] are equal. *)

  val float : ?__POS__:pos -> float -> float -> unit
  (** [float f0 f1] asserts that [f0] and [f1] are equal. This uses
      {!Float.equal} so it can be used to assert [NaN] values, also
      you should be aware of the limitations that are involved in
      testing floats for equality. *)

  val string : ?__POS__:pos -> string -> string -> unit
  (** [string s0 s1] asserts that [s0] and [s1] are equal. *)

  val binary_string : ?__POS__:pos -> string -> string -> unit
  (** [binary_string s0 s1] asserts that [s0] and [s1] are equal assuming
      that [s0] and [s1] is binary data. *)

  val styled_string : ?__POS__:pos -> string -> string -> unit
  (** [styled_string s0 s1] asserts that [s0] and [s1] are equal assuming
      that [s0] and [s1] have ANSI escape sequences. *)

  val bytes : ?__POS__:pos -> bytes -> bytes -> unit
  (** [string s0 s1] asserts that [s0] and [s1] are equal. *)

  val pair :
    ?fst:'a Eq.t -> ?snd:'b Eq.t -> ?__POS__:pos -> ('a * 'b) -> ('a * 'b) ->
    unit
  (** [pair ~fst ~snd p0 p1] asserts pairs [p0] and [p1] are equal using
      [fst] and [snd] to test components (defaults to {!Eq.any}). *)

  val list : ?elt:'a Eq.t -> ?__POS__:pos -> 'a list -> 'a list -> unit
  (** [list ~elt l0 l1] assert lists [l0] and [l1] are equal using [elt] to
      test elements (default to {!Eq.any}). *)

  val array : ?elt:'a Eq.t -> ?__POS__:pos -> 'a array -> 'a array -> unit
  (** [array ~elt a0 a1] assert arrays [a0] and [a1] are equal
      using [elt] to test elements (defaults to {!Eq.any}). *)

  (** {3:options Options} *)

  val get_some : ?__POS__:pos -> 'a option -> 'a
  (** [get_some] gets the value of [Some _] and {!fail}s and {!stop}s
      if that is not possible. *)

  val is_some : ?__POS__:pos -> 'a option -> unit
  (** [is_some v] asserts that [v] is [Some _]. *)

  val option : ?some:'a Eq.t -> ?__POS__:pos -> 'a option -> 'a option -> unit
  (** [option ~some:(module M) v0 v1] asserts options [v0] and [v1]
      are equal according to [M]'s equality (otherwise {!Stdlib.( = )}
      is used). *)

  (** {3:results Results} *)

  val get_ok : ?__POS__:pos -> ('a, string) result -> 'a
  (** [get_ok] gets the value of [Ok _] and {!failstop}s if that is not
      possible. Calls {!pass} on success. *)

  val get_ok' : ?error:'e Eq.t -> ?__POS__:pos -> ('a, 'e) result -> unit
  (** [get_ok'] is like {!get_ok} but uses [error] to render the error. *)

  val is_ok : ?__POS__:pos -> ('a, string) result -> unit
  (** [is_ok v] asserts [v] is (Ok _) value. *)

  val is_ok' : ?error:'e Eq.t -> ?__POS__:pos -> ('a, 'e) result -> unit
  (** [is_ok'] is like {!is_ok} but uses [error] to render the error. *)

  val result :
    ?ok:'a Eq.t -> ?__POS__:pos ->
    ('a, string) result -> ('a, string) result -> unit
  (** [result ~ok r0 r1] asserts results [r0] and [r1] are
      equal according to [ok] (defaults to {!Eq.any}). *)

  val result' :
    ?ok:'a Eq.t -> ?error:'e Eq.t -> ?__POS__:pos ->
    ('a, 'e) result -> ('a, 'e) result -> unit
  (** [result ~ok ~error r0 r1] asserts results [r0] and [r1] are
      equal according to [ok] and [error] (both default to {!El.eq} *)

  (** {1:random Randomized testing} *)

  (** Randomized testing. *)
  module Rand : sig
    val state : unit -> Random.State.t
    (** [state ()] returns a random state. This is self seeded
        unless the [TEST_SEED] environment variable is set in which case
        an integer. *)
  end

  (** Command line *)
  module Cli : sig
    val parse :  ?opts:(Arg.key * Arg.spec * Arg.doc) list -> unit -> unit
    (** [parse ()] parses and setups testing state. This automatically
        adds an option for seeding {!Rand}. *)
  end
end

(** {1:example Example}

    This is a typical test executable. The pattern here
    ensures that you can, case arising, load it in the toplevel
    and invoke the various tests manually.
{[
open B0_testing

let test_string_get () =
  Test.test "String.get" @@ fun () ->
  Test.char (String.get "a" 0) 'a' ~__POS__;
  Test.invalid_arg @@ (fun () -> String.get "" 0) ~__POS__;
  ()

let test_string_append () =
  Test.test "String.append" @@ fun () ->
  Test.string (String.cat "a" "b") "ab" ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  test_string_get ();
  test_string_append ();
  ()

let () = if !Sys.interactive then () else exit (main ())
]}

  Note that if you are in a testing hurry or migrating you can always
  simply use OCaml's [assert] in the test functions: you just won't
  get any rendering on assertion failures and tests stop at the first
  assertion failure. *)
