(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(** Software version numbers. *)

(** {1:version Versions} *)

type t = int * int * int * string option
(** The type for versions. Major, minor, patch and additional info. *)

(** {1:incr Incrementing} *)

val next_major : ?info:string -> t -> t
(** [next_major ?info (maj, _, _, _)] is [(maj+1, 0, 0, info)]. *)

val next_minor : ?info:string -> t -> t
(** [next_major ?info (maj, min, _, _)] is [(maj, min + 1, 0, None)]. *)

val next_patch : ?info:string -> t -> t
(** [next_patch (maj, min, patch, _)] is [(maj, min, patch + 1, info)]. *)

val with_info : string option -> t -> t
(** [with_info (maj, min, patch, _) info] is [(maj, min, patch, info)]. *)

(** {1:converting Converting} *)

val of_string : string -> t option
(** [of_string] parses version strings of the form:
    {[
      [v|V]major.minor[.patchlevel][(+|~)additional-info]
    ]}
    into [(major, minor, patch, (+|~)additional_info)] tuples. If no
    [patchlevel] is found [0] is used. *)

val to_string : t -> string
(** [to_string v] is ["major.minor.patchlevel[(+|~)additional-info]"] *)

val string_drop_initial_v : string -> string
(** [string_drop_initial_v s] drops a leading ['v'] or ['V'] from [s]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a version. *)

val pp_string : string Fmt.t
(** [pp_string] formats a version string. *)
