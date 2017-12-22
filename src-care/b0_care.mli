(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Generic convenience descriptions.

    These descriptions are completely generic. Some of them are used
    to guide and alter the behaviour of the [b0] and [d0] tools. *)

open B0

(** {1:fmeta File path metadata}

    {b TODO} Move that to a Fpath submodule. *)

val exe : bool Fpath.Meta.key
(** [exe] indicates the file is executable. This is used by the [b0]
    [run] command to identify runnable candidates. *)

type install =
  [ `Bin | `Doc | `Etc | `Lib | `Lib_root | `Libexec | `Libexec_root
  | `Man | `Misc | `Sbin | `Share | `Share_root | `Stublibs
  | `Other of Fpath.t ] * Fpath.t option
(** The type for installs. A destination and an optional relative file
    path in the destination. If unspecified the file is installed at
    the root with the same file name of the source. *)

val install : install Fpath.Meta.key
(** [install] indicates the file is installable at the given install
    specification.  *)

val dist : bool Fpath.Meta.key
(** [dist] indicates that a built file generated in the hierarchy
    rooted at {!B0.build.src_dir} should be kept for distribution. *)

(** {1:umeta Unit metadata} *)

(** Unit metadata. *)
module Unit : sig

  (** {1:outcome Outcome tags} *)

  val exe : bool Unit.Meta.key
  (** [exe] is for units with executable outcomes. *)

  val lib : bool Unit.Meta.key
  (** [lib] is for units with library outcomes. *)

  val test : bool Unit.Meta.key
  (** [test] is for units with testing outcomes. *)

  val bench : bool Unit.Meta.key
  (** [bench] is for units with benchmarking outcomes. *)

  val doc : bool Unit.Meta.key
  (** [doc] is for units with documentation outcomes. *)

  val build : bool Unit.Meta.key
  (** [build] is for units with outcomes used by the build
      system itself. *)

  val dev : bool Unit.Meta.key
  (** [dev] is for units with outcomes used during development. *)
end

(** {1:osinfo Operating system information} *)

(** Build and host operating system information. *)
module OS : sig

  (** {1:osinfo Operating system information} *)

  val name : string Conf.key
  (** [name] is the operating system name. Determine using the
      following steps (in order):
      {ol
      {- If [uname] can be found in the environment. Determined
         by normalizing the output of [uname -s]. In particular ["darwin"]
         is normalized to ["macos"]}
      {- If the [COMSPEC] environment variable is defined, then ["windows"]
         is used.}
      {- ["unknown"] otherwise.}} *)

  val arch : string Conf.key
  (** [arch] is the architecture of the operating system.
      FIXME this should depend on [name].
      If {!Sys.os_type} is:
      {ul
      {- ["Win32"]. Determined by normalizing
       the ["PROCESSOR_ARCHITECTURE"] and ["PROCESSOR_ARCHITEW6432"]
       environment variables.}
      {- Otherwise. Determined by normalizing the output of [uname -m] or
         ["unknown"].}}
      Normalization simplifies various specific architecture to more
      general identifiers like ["arm32"], ["arm64"], ["ppc32"],
      ["ppc64"], ["x86_32"] and ["x86_64"]. *)

  val arch_bits : int Conf.key
  (** [arch_bits] is number of bits in a word on the architecture of
      the operating system, derived from {!arch}. Usually either [32]
      or [64]. *)

  val distribution : string Conf.key
  (** [distribution] is the operating system distribution or
      a package manager. If {!name} is
      {ul
      {- ["macos"] and either [brew] or [ports] are found in [PATH] then
       this is respectively ["homebrew"] or ["macport"], the former
       takes over if both are present.}
      {- ["linux"]. An Android heuristic is applied, otherwise looks up
       the [ID] field of
       {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available.}
      {- Otherwise. The value of {!name}}} *)

  val version : string Conf.key
  (** [version] is the operating system version string.
      If {!name} is
      {ul
      {- ["macos"]. Determined by the output of [sw_vers -productVersion].}
      {- ["linux"]. An Android heuristic is applied, otherwise looks
       the [VERSION_ID] field of
       {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available.}
      {- ["win32"], ["cygwin"]. Determined by parsing the output of
        [wmic os get Version]. The map of this version number to actual
        Windows operating systems is
        {{:https://msdn.microsoft.com/en-us/library/windows/desktop/ms724832.aspx}here}.}
      {- ["freebsd"]. The output of [uname -U].}
      {- Otherwise. The output of [uname -r] or ["unknown"].}} *)

  val family : string Conf.key
  (** [family] is the operating system family. This lumps the following
      {!name} names under a common identifier:
      {ul
      {- The various BSDs as ["bsd"]}
      {- ["win32"], ["cygwin"] as ["windows"]}
      {- ["linux"], looks up the first value of [ID_LIKE] in
          {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available. If not uses {!distribution}.}
      {- Otherwise. The value of {!name}}} *)

  val exe_ext : Fpath.ext Conf.key
  (** [exe_ext] is the file extension used for executable file formats
      on the operating system. Derived from {!name}. *)
end


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
