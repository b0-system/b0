(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Simple testing tools. *)

open B0_std

(** Testing structure and combinators. *)
module Test : sig

  (** {1:testing Testing structure} *)

  val test : string -> (unit -> unit) -> unit
  (** [test name f] runs the test in [f]. The test fails if
      it raises an unexpected exception. [name] is logged before
      [f] is executed. This is typically called this way:
      {[
let mytest () =
  Test.test "Something" @@ fun () ->
  assert (1 = 1); …
  ()
]}
  *)

  val main : (unit -> unit) -> int
  (** [main f] executes [f ()] and reports the testing status.
      The returned integer is the number of failing tests which
      should be given to [exit].

      [f] typically calls function that call {!test}. Your typical main
      should look like this::
{[
let main () =
  Test.main () @@ fun () ->
  my_test ()
  …

let () = if !Sys.interactive then exit (main ())
]}
   This structure ensures you can load and run its components
   in the toplevel, e.g. via [b0 -- .ocaml.ocaml] *)
  (** {1:tests Testing combinators} *)

  (** {1:combinators Test combinators} *)

  val repeat :
    fail:(int -> 'b, Format.formatter, unit, unit) format4 ->
    int -> (int -> 'a) -> unit
    (** [repeat ~fail n f] calls [f] with [n] to [1] stopping
        if it fails and logging [log fail n]. *)

  val invalid_arg : (unit -> 'a) -> unit
  (** [invalid_arg f] tests that [f ()] raises [Invalid_argument].  *)

  val failure : (unit -> 'a) -> unit
  (** [failure f] tests that [f ()] raises [Failure]. *)

  val raises : (exn -> bool) -> (unit -> 'a) -> unit
  (** [raises is_exn f] tests that [f ()] raises an exception [exn]
      that satisfies [is_exn exn]. *)

  (** {1:testing_log Testing logging} *)

  val log : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log fmt …] logs a message formatted by [fmt] *)

  val log_fail : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log_fail fmt …] is like {!log} but for failures. *)

end


(** Formatters for test runners. *)
module Test_fmt : sig
  val pp_test : unit Fmt.t
  val pp_fail : unit Fmt.t
  val pp_pass : unit Fmt.t
  val pp_passed : unit Fmt.t
  val pp_failed : unit Fmt.t
  val pp_dur : Mtime.Span.t Fmt.t
  val pp_count : int Fmt.t
end

(** {1:example Example}
{[
open B0_testing

let test_string_get () =
  Test.test "String.get" @@ fun () ->
  Test.invalid_arg @@ (fun () -> String.get "" 1));
  assert (String.get "a" 1 = 'a');
  ()

let test_string_append () =
  Test.test "String.append" @@ fun () ->
  assert (String.append "a" "b" = "ab");
  ()

let main () =
  Test.main @@ fun () ->
  test_string_append ();
  ()

let () = if !Sys.interactive then exit (main ())
]}
*)
