(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00

let read_spawn_stdout m file cmd =
  Memo.spawn m ~writes:[file] ~stdout:(`File file) cmd;
  Fut.map String.trim (Memo.read m file)

let rec find_first_existing_file = function
| [] -> None
| f :: fs ->
    match Os.File.exists f |> Log.if_error ~use:false with
    | true -> Some f
    | false -> find_first_existing_file fs

(* A few tools used by this module *)

module Tool = struct
  let brew = Tool.by_name "brew"
  let cmd_exe = Tool.by_name "cmd.exe" (* windows *)
  let getprop = Tool.by_name "getprop" (* android *)
  let port = Tool.by_name "port"
  let sw_vers = Tool.by_name "sw_vers" (* macos *)
  let uname = Tool.by_name "uname"
end

(* Store keys for os-release and uname, not exposed but used below *)

let os_release =
  (* Access https://www.freedesktop.org/software/systemd/man/os-release.html *)
  let unquote s =
    let quoted s len = s.[0] = '"' && s.[len - 1] = '"' in
    match String.length s with
    | 0 | 1 -> s
    | len when quoted s len -> String.subrange s ~first:1 ~last:(len - 2)
    | len -> s
  in
  let parse_line i line acc = match String.length (String.trim line) with
  | 0 -> acc | _ when line.[0] = '#' -> acc
  | _ ->
      match String.cut_left ~sep:"=" line with
      | None ->
          B00_lines.err i "Cannot find %a char in %S" Fmt.(code char) '=' line
      | Some (k, v) ->
          String.Map.add (String.trim k) (unquote (String.trim v)) acc
  in
  let det s m =
    let files = [Fpath.v "/etc/os-release"; Fpath.v "/usr/lib/os-release"] in
    match find_first_existing_file files with
    | None -> Fut.return String.Map.empty
    | Some file ->
        Memo.file_ready m file;
        let* s = Memo.read m file in
        let map = B00_lines.fold s ~file parse_line String.Map.empty in
        let map = Memo.notify_if_error m `Warn ~use:String.Map.empty map in
        Fut.return map
  in
  Store.key ~mark:"B00_os.os_release_file" det

let uname = (* gets system name, release version, machine arch *)
  let mark = "b00_os.uname" in
  let det s m = match Memo.tool_opt m Tool.uname with
  | None -> Fut.return None
  | Some uname ->
      let file = Fpath.(Store.dir s / mark) in
      let uname = uname Cmd.(arg "-s" % "-r" % "-m") in
      let* s = read_spawn_stdout m file uname in
      match String.split_on_char ' ' s with
      | [sys; rel; mach] -> Fut.return (Some (sys, rel, mach))
      | _ ->
          Memo.notify m
            `Warn "Could not parse %a output %S." Fmt.(code string) "uname" s;
          Fut.return None
  in
  Store.key ~mark det

(* name *)

let name =
  let of_uname m = function
  | None ->
      let env = Env.env (Memo.env m) in
      if String.Map.mem "COMSPEC" env || String.Map.mem "ComSpec" env
      then "windows"
      else "unknown"
  | Some (s, _, _) ->
      match String.Ascii.lowercase s with
      | "darwin" -> "macos"
      | s -> s
  in
  let det s m = Fut.map (of_uname m) (Store.get s uname) in
  Store.key ~mark:"b00_os.name" det

(* version *)

let freebsd_version s m file =
  let file = Fpath.(Store.dir s / file) in
  let uname = (Memo.tool m Tool.uname) (Cmd.arg "-U") in
  read_spawn_stdout m file uname

let try_android_version s m file = match Memo.tool_opt m Tool.getprop with
| None -> Fut.return None
| Some getprop ->
    let file = Fpath.(Store.dir s / file) in
    let getprop = getprop (Cmd.arg "ro.build.version.release") in
    Fut.map Option.some (read_spawn_stdout m file getprop)

let linux_version s m file =
  Fut.bind (try_android_version s m file) @@ function
  | Some v -> Fut.return v
  | None ->
      Fut.bind (Store.get s os_release) @@ function info ->
      match String.Map.find_opt "VERSION_ID" info with
      | None -> Fut.return "unknown"
      | Some v -> Fut.return v

let macos_version s m file =
  let file = Fpath.(Store.dir s / file) in
  let sw_vers = (Memo.tool m Tool.sw_vers) (Cmd.arg "-productVersion") in
  read_spawn_stdout m file sw_vers

let windows_version s m file =
  let file = Fpath.(Store.dir s / file) in
  let cmd_exe = (Memo.tool m Tool.cmd_exe) Cmd.(arg "/c" % "ver") in
  let* s = read_spawn_stdout m file cmd_exe in
  (* Format is 'Product [Version XXXX]', we parse the XXX *)
  let err m s =
    Memo.notify m `Warn
      "@[<v>Could not parse %a output %S." Fmt.(code string) "ver" s;
    Fut.return "unknown"
  in
  let last = String.length s - 2 in
  if last < 0 then err m s else
  match String.rindex_opt s ' ' with
  | None -> err m s
  | Some i -> Fut.return (String.subrange ~first:(i + 1) ~last s)

let version =
  let mark = "b00_os.version" in
  let det s m = Fut.bind (Store.get s name) @@ function
  | "freebsd" -> freebsd_version s m mark
  | "linux" -> linux_version s m mark
  | "macos" -> macos_version s m mark
  | "windows" -> windows_version s m mark
  | _ ->
      Fut.bind (Store.get s uname) @@ function
      | Some (_, r, _) -> Fut.return (String.trim r)
      | None -> Fut.return "unknown"
  in
  Store.key ~mark det

(* distribution *)

let linux_distribution s m =
  let* info = Store.get s os_release in
  match String.Map.find_opt "ID" info with
  | None -> Fut.return "unknown"
  | Some v -> Fut.return v

let macos_distribution s m = match Memo.tool_opt m Tool.brew with
| Some _ -> Fut.return "homebrew"
| None ->
    match Memo.tool_opt m Tool.port with
    | Some _ -> Fut.return "macports"
    | None -> Fut.return "macos"

let distribution =
  let det s m = Fut.bind (Store.get s name) @@ function
  | "linux" -> linux_distribution s m
  | "macos" -> macos_distribution s m
  | n -> Fut.return n
  in
  Store.key ~mark:"b00_os.distribution" det

(* family *)

let linux_family s m =
  let* info = Store.get s os_release in
  match String.Map.find_opt "ID_LIKE" info with
  | None -> Store.get s distribution
  | Some v ->
      Fut.return @@
      match String.cut_left ~sep:" " v with None -> v | Some (f, _) -> f

let family =
  let det s m = Fut.bind (Store.get s name) @@ function
  | "linux" -> linux_family s m
  | "freebsd" | "openbsd" | "netbsd" | "dragonfly" -> Fut.return "bsd"
  | "windows" | "cygwin" -> Fut.return "windows"
  | n -> Fut.return n
  in
  Store.key ~mark:"b00_os.family" det

(* Executable file extension *)

let exe_ext =
  let ext_of_name = function "windows" -> ".exe" | _ -> "" in
  let det s m = Fut.map ext_of_name (Store.get s name) in
  Store.key ~mark:"b00_os.exe_ext" det

(* Machine architecture *)

let arch =
  let ret a = if a = "" then "unknown" else String.Ascii.lowercase a in
  let det s m = Fut.bind (Store.get s uname) @@ function
  | Some (_, _, a) -> Fut.return (ret a)
  | None ->
      let env = Env.env (Memo.env m) in
      match String.Map.find "PROCESSOR_ARCHITECTURE" env with
      | exception Not_found -> Fut.return "unknown"
      | "x86" as a ->
          begin match String.Map.find "PROCESSOR_ARCHITEW6432" env with
          | exception Not_found -> Fut.return a
          | a -> Fut.return (ret a)
          end
      | a -> Fut.return (ret a)
  in
  Store.key ~mark:"b00_os.arch" det

let arch_normalized =
  let normalize = function
  | "x86" | "i386" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" | "armv8b" | "armv8l" -> "arm64"
  | a when String.(starts_with "armv5" a ||
                   starts_with "armv6" a || starts_with "earmv6" a ||
                   starts_with "armv7" a || starts_with "earmv7" a) -> "arm32"
  | a -> a
  in
  let det s m = Fut.map normalize (Store.get s arch) in
  Store.key ~mark:"b00_os.arch_normalized" det

let arch_bits =
  let default = 64 in
  let det s m = Fut.bind (Store.get s arch_normalized) @@ function
    | "x86_64" | "ppc64" | "ia64" | "arm64" | "sparc64" | "mips64"->
        Fut.return 64
    | "x86_32" | "ppc32" | "arm32" -> Fut.return 32
    | a ->
        Memo.notify m
          `Warn "Unknown word size for arch %a. Using %d bits."
          Fmt.(code string) a default;
        Fut.return default
  in
  Store.key ~mark:"b00_os.arch_bits" det

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
