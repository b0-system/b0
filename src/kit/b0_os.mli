(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Operating system and machine information. *)

(** {1:id OS identification} *)

val name : string B0_store.key
(** [name] is the operating system name. Determined using the
    following steps, in order:
    {ol
    {- If the [uname] tool is available in the memo environment, [name] is the
       lowercased token returned by [uname -s], transformed as follows:
     {ul
     {- ["darwin"] is mapped on ["macos"]}
     {- Otherwise left as is. {b XXX.} Normalization is likely unsufficient.}}}
    {- If the [COMSPEC] environment variable is defined in the
       memo environment, ["windows"].}
    {- Otherwise, ["unknown"].}} *)

val version : string B0_store.key
(** [version] is the operating system version string. This depends on
    the value of {!name}:
    {ul
    {- ["freebsd"]. The output of [uname -U].}
    {- ["linux"]. An Android heuristic is applied, otherwise looks
       the [VERSION_ID] field of
       {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available, otherwise ["unknown"]}
    {- ["macos"]. The output of [sw_vers -productVersion].}
    {- ["windows"]. The parsed output of [cmd.exe /c ver]. The Windows
       operating system to which this version string maps can be found
       {{:https://docs.microsoft.com/en-gb/windows/win32/sysinfo/operating-system-version}
       here.}}
    {- Otherwise if the [uname] tool is available in the memo environment
       the result of [uname -r] and otherwise ["unknown"].}} *)

val distribution : string B0_store.key
(** [distribution] is the operating system distribution or
    a package manager. This depends on the value of {!name}:
    {ul
    {- ["linux"]. Looks up
       the [ID] field of
       {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available, if not found ["linux"]}
    {- ["macos"]. If either the [brew] or [port] tool is found in the memo
       environment then this is respectively ["homebrew"] or ["macports"].
       The former takes over if both are present. If none are found ["macos"].}
    {- Otherwise. The value of {!name}}} *)

val family : string B0_store.key
(** [family] is the operating system family. This lumps the following
    {!name} names under a common identifier:
    {ul
    {- The various BSDs as ["bsd"]}
    {- ["windows"], ["cygwin"] as ["windows"]}
    {- ["linux"], looks up the first value of [ID_LIKE] in
          {{:https://www.freedesktop.org/software/systemd/man/os-release.html}
       os-release} if available. If not uses {!distribution}.}
    {- Otherwise. The value of {!name}}} *)

(** {1:exe_ext Executable file extension} *)

val exe_ext : B0_std.Fpath.ext B0_store.key
(** [exe_ext] is operating system specific file extension for
    executable files. This is:
    {ul
    {- [".exe"] if {!name} is ["windows"].}
    {- [""] otherwise.}} *)

(** {1:machine_arch Machine architecture}

    As reported by the operating system. *)

val arch : string B0_store.key
(** [arch] is the architecture of the operating system, see also
    {!arch_normalized}. Determined using the following steps (in order):
    {ul
    {- If [uname] is available in the memo environment, [arch] is
       the lowercased token returned by [uname -m].}
    {- If the [PROCESSOR_ARCHITECTURE] environment variable is defined
       in the memo environment, its value unless it is ["x86"]. In the latter
       case the value of [PROCESSOR_ARCHITEW6432] or, if undefined, ["x86"].
       This is for Windows, see {{:https://docs.microsoft.com/en-gb/windows/win32/winprog64/wow64-implementation-details#environment-variables}here} for the
       rationale.}
    {- Otherwise, ["unknown"].}} *)

val arch_normalized : string B0_store.key
(** [arch_normalized] normalizes some of the {!arch} values to more
    general identifiers like ["arm32"], ["arm64"], ["ppc32"],
    ["ppc64"], ["x86_32"] and ["x86_64"]. *)

val arch_bits : int B0_store.key
(** [arch_bits] is number of bits in a word on the architecture of the
    operating system, derived from {!arch_normalized}. Usually either
    [32] or [64]. If unknown warns on the memo and defaults to [64]. *)
