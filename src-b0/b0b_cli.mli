(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Cli tools for b0. *)

open B0

(** {1:dym Did you mean ?} *)

val did_you_mean :
  ?pre:string -> ?post:string -> log:Log.level -> kind:string ->
  string * string list -> unit
(** [level] default to [Log.Error], [pre] to "Unknown", [post] to "". *)

(** {1:exits Exits} *)

type exit =
  [ `Ok | `Unknown_name | `Undefined_key | `No_outcome | `Discover_error
  | `Some_error | `Cli_error | `No_default_variant | `No_variant | `Hook_error
  | `No_b0_dir | `Code of int ]

val to_exit_code : exit result -> int result
val join_exit : (exit, exit) Pervasives.result -> exit result
val exits : Cmdliner.Term.exit_info list

(** {1:args Arguments} *)

val err_too_many_args : string list -> unit
val err_missing_arg : kind:string -> unit

val synopsis_cmd_with_action : Cmdliner.Manpage.block
val action_arg : (string * 'a) list -> 'a Cmdliner.Term.t
val action_arg_with_default :
  default:'a -> (string * 'a) list -> 'a Cmdliner.Term.t

val action_pos_args : string list Cmdliner.Term.t

val no_pager : bool Cmdliner.Term.t

(** {1:b0_dir B0 directory} *)

val b0_dir_must_exist :
  log:Log.level -> B0_driver.B0_dir.t -> (unit, exit) Pervasives.result

(** {1:variants Variants} *)

val get_default_variant_name :
  log:Log.level -> [`Effective | `Stored] -> B0_driver.B0_dir.t ->
  (string, exit) Pervasives.result

val find_variant_name :
  cli:string option -> B0_driver.B0_dir.t -> string option

val find_variant_scheme_name :
  cli:string option -> B0_driver.B0_dir.t -> string option

val get_variant_scheme :
  log:Log.level -> string option ->
  (Variant.Scheme.t, exit) Pervasives.result

val variant_suggest_name :
  dir:Fpath.t -> Variant.Scheme.t -> string result

val need_variant_name :
  log:Log.level -> string option -> (string, exit) Pervasives.result

val find_variant :
  log:Log.level -> dir:Fpath.t -> string option ->
  Variant.load option result

val get_variant :
  log:Log.level -> cli:string option -> b0_dir:B0_driver.B0_dir.t ->
  (Variant.load, exit) Pervasives.result

val load_variant :
  log:Log.level -> dir:Fpath.t -> string ->
  (Variant.load, exit) Pervasives.result

val variant_create :
  log:Log.level -> b0_dir:B0_driver.B0_dir.t ->
  name:string option -> scheme:Variant.Scheme.t -> preset:bool ->
  make_default:bool -> (Variant.t, exit) Pervasives.result

val variant_load_conf :
  log:Log.level -> Variant.t -> Conf.t result

val variant_load_outcome : Variant.t -> Outcome.t option result
val variant_get_outcome :
  log:Log.level -> Variant.t -> (Outcome.t, exit) Pervasives.result

val variant_load_last_conf : Variant.t -> Conf.t option result
val variant_save_conf :
  ?exit:exit -> log:Log.level -> B0.Variant.t -> B0.Conf.t -> exit

(** {1:cache Cache directory} *)

val get_cache_dir : b0_dir:Fpath.t -> cache_dir:Fpath.t option -> Fpath.t

(** {1:gen Generic operations on defined values. *)

val find_named_values :
  log:Log.level -> kind:string -> list:(unit -> 'a list) ->
  get_or_suggest:(string -> ('a, string list) Pervasives.result) ->
  string list -> 'a list * [> `Ok | `Unknown_name ]

val show_value :
  short:'a Fmt.t -> normal:'a Fmt.t -> long:'a Fmt.t ->
  B0_driver.Cli.out_fmt -> 'a -> unit

val list_values_action :
  find:(string list -> 'b list * exit) ->
  show:(B0_driver.Cli.out_fmt -> 'b -> unit) -> B0_driver.Cli.out_fmt ->
  string list -> (exit, 'e) Pervasives.result

val value_info_action :
  list_action:(B0_driver.Cli.out_fmt -> string list ->
               (exit, 'e) Pervasives.result) ->
  B0_driver.Cli.out_fmt -> string list -> (exit, 'e) Pervasives.result

(** Showing things *)

val show_key :
  ?ext:Conf.Key.t Fmt.t -> B0_driver.Cli.out_fmt -> Conf.Key.t -> unit

val show_variant :
  ?ext:Variant.t Fmt.t -> B0_driver.Cli.out_fmt -> Variant.t -> unit

(** Editing and paging files *)

val edit_files : Fpath.t list -> int result
val find_pager : don't:bool -> Fpath.t option result

(** Units *)

val units : doc:string -> string list Cmdliner.Term.t
val show_unit : B0_driver.Cli.out_fmt -> Unit.t -> unit
val find_units :
  log:Log.level -> string list -> Unit.t list * [> `Ok | `Unknown_name ]

val units_of_pkgs : B0.Pkg.t list -> B0.Unit.t list

(** Packages *)

val pkgs : doc:string -> string list Cmdliner.Term.t
val show_pkg : B0_driver.Cli.out_fmt -> Pkg.t -> unit
val find_pkgs :
  log:Log.level -> string list -> Pkg.t list * [> `Ok | `Unknown_name ]

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
