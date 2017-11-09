(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let loc = Def.Loc.lib "B0_care"
let group = Conf.Group.v ~loc "os" ~doc:"Operating system information"

(* FIXME needs to be fixed at the Conf api level *)

let tool env aim tool =
  Env.tool env aim [Fpath.v tool] >>| fun tool -> Cmd.(v @@ p tool)

let get_dep env aim k c =
  Conf.get_effective env aim k c >>| fun (_, a) -> a

let warn_if_error e ~use:v =
  Log.on_error_msg e ~level:Log.Warning ~use:(fun _ -> v)

(* Uname *)

let uname env aim opts =
  tool env aim "uname" >>= fun uname ->
  OS.Cmd.(run_out Cmd.(uname %% opts) |> to_string)

(* Architecture *)

let arch_normalize arch = match String.lowercase_ascii arch with
| "" -> "unknown"
| "x86" | "i386" | "i586" | "i686" -> "x86_32"
| "x86_64" | "amd64" -> "x86_64"
| "powerpc" | "ppc" | "ppcle" -> "ppc32"
| "ppc64" | "ppc64le" -> "ppc64"
| "aarch64_be" | "aarch64" | "armv8b" | "armv8l" -> "arm64"
| a when String.(is_prefix "armv5" a ||
                 is_prefix "armv6" a || is_prefix "earmv6" a ||
                 is_prefix "armv7" a || is_prefix "earmv7" a) -> "arm32"
| a -> a

let arch_windows env aim =
  let env = Env.env env aim in
  match String.Map.find "PROCESSOR_ARCHITECTURE" env with
  | exception Not_found ->
      R.error_msg "variable PROCESSOR_ARCHITECTURE undefined"
  | ("x86" as x86) ->
      (* See https://msdn.microsoft.com/en-us/library/aa384274.aspx *)
      begin match String.Map.find "PROCESSOR_ARCHITEW6432" env with
      | exception Not_found -> Ok x86
      | arch -> Ok arch
      end
  | arch -> Ok arch

let arch_discover env aim _ _ =
  let arch = match Sys.os_type (* FIXME get rid of that *) with
  | "Win32" -> arch_windows env aim
  | _ -> uname env aim (Cmd.v "-m")
  in
  Ok (arch_normalize @@ warn_if_error arch ~use:"unknown")

let arch =
  let doc = "Operating system architecture" in
  let default = Conf.discover arch_discover in
  Conf.key ~loc ~doc ~group "os.arch" Conv.string_non_empty ~default

(* Architecture bits per words *)

let arch_bits_of_arch = function
| "x86_64" | "ppc64" | "ia64" | "arm64" | "sparc64" | "mips64"-> Some 64
| "x86" | "ppc" | "arm32" -> Some 32
| _ -> None

let arch_bits_discover env aim _ c =
  (* FIXME dependent lookup is broken at the API level *)
  let default = 32 in
  let arch = get_dep env aim arch c in
  let bits = arch >>= fun a -> match arch_bits_of_arch a with
  | Some size -> Ok size
  | None -> R.error_msgf "Unknown word bit size for %s, using %d" a default
  in
  Ok (warn_if_error bits ~use:default)

let arch_bits =
  let doc = "Operating system word size in bits" in
  let default = Conf.discover arch_bits_discover in
  Conf.key ~loc ~doc ~group "os.arch_bits" Conv.int ~default

(* OS name *)

let name_normalize os = match String.lowercase_ascii os with
| "" -> "unknown"
| "darwin" -> "macos"
| os -> os

let name_discover env aim _ _ =
  let name = match Sys.os_type (* FIXME get rid of that *) with
  | "Unix" -> uname env aim @@ Cmd.v "-s"
  | os -> Ok os
  in
  Ok (name_normalize @@ warn_if_error name ~use:"unknown")

let name =
  let doc = "Operating system name" in
  let default = Conf.discover name_discover in
  Conf.key ~loc ~doc ~group "os.name" Conv.string_non_empty ~default

(* Android detection *)

let android_version =
  let cmd = Cmd.(v "getprop" % "ro.build.version.release") in
  let version = lazy (OS.Cmd.(run_out ~err:OS.File.null cmd |> to_string)) in
  fun () -> match Lazy.force version with
  | Error _ -> None
  | Ok v -> Some v

(* os-release files
   See https://www.freedesktop.org/software/systemd/man/os-release.html *)

let rec find_first_existing_file = function
| [] -> None
| f :: fs ->
    match OS.File.exists f with
    | Ok true -> Some f
    | Ok false -> find_first_existing_file fs
    | Error _ as e -> warn_if_error e ~use:(); find_first_existing_file fs

let os_release_parse file = match OS.File.read file with
| Error _ as e -> warn_if_error e ~use:None
| Ok text ->
    let unquote s =
      (* Should also unescape. Too lazy for now *)
      let enc c s len = s.[0] = c && s.[len - 1] = c in
      match String.length s with
      | 0 | 1 -> s
      | len when enc '"' s len || enc '\'' s len -> String.sub s 1 (len - 2)
      | len -> s
    in
    let parse_line acc line = match String.length line with
    | 0 -> acc
    | len when line.[0] = '#' -> acc
    | len ->
        match String.cut ~sep:"=" line with
        | None ->
            let e =
              R.error_msgf "Could not parse %a file line: %S" Fpath.pp file line
            in
            warn_if_error e ~use:acc
        | Some (var, value) ->
            (String.trim var, unquote (String.trim value)) :: acc
    in
    let lines = String.cuts ~sep:"\n" text in
    Some (List.(rev @@ fold_left parse_line [] lines))

let os_release_field =
  let files = [ Fpath.v "/etc/os-release"; Fpath.v "/usr/lib/os-release" ] in
  let assoc () = match find_first_existing_file files with
  | None -> None
  | Some file -> os_release_parse file
  in
  let assoc = lazy (assoc ()) in
  fun field -> match Lazy.force assoc with
  | None -> None
  | Some assoc ->
      match List.assoc field assoc with
      | exception Not_found -> None
      | v -> Some v

(* Operating system distribution or system package manager *)

let distribution_normalize os = match String.lowercase_ascii os with
| "" -> "unknown"
| v -> v

let distribution_linux os = match android_version () with
| Some v -> v
| None ->
    match os_release_field "ID" with
    | None -> os
    | Some v -> v

let distribution_macos os =
  OS.Cmd.exists (Cmd.v "brew") >>= function
  | true -> Ok "homebrew"
  | false ->
      OS.Cmd.exists (Cmd.v "ports") >>= function
      | true -> Ok "macports"
      | false -> Ok os

let distribution_discover env aim _ c =
  let distrib = get_dep env aim name c >>= fun os -> match os with
  | "linux" -> Ok (distribution_linux os)
  | "macos" -> Ok (warn_if_error (distribution_macos os) ~use:os)
  | os -> Ok os
  in
  Ok (distribution_normalize @@ warn_if_error distrib ~use:"unknown")

let distribution =
  let doc = "Operating system distribution or package manager" in
  let default = Conf.discover distribution_discover in
  Conf.key ~loc ~doc ~group "os.distribution" Conv.string_non_empty ~default

(* Operating system version *)

let version_normalize v = match String.lowercase_ascii v with
| "" -> "unknown"
| v -> v

let version_linux () = match android_version () with
| Some v -> Ok v
| None ->
    match os_release_field "VERSION_ID" with
    | None -> Ok "unknown"
    | Some v -> Ok v

let version_macos () =
  OS.Cmd.(run_out Cmd.(v "sw_vers" % "-productVersion") |> to_string)

let version_windows () =
  let wmic opts = OS.Cmd.(run_out Cmd.(v "wmic" %% opts) |> to_string) in
  wmic Cmd.(v "os" % "get" % "Version" % "/value") >>= fun kv ->
  match String.cut ~sep:"=" kv with
  | None -> R.error_msgf "Could not parse wmic value from %S" kv
  | Some (_, v) -> Ok (String.trim v)

let version_discover env aim _ c =
  let version = get_dep env aim name c >>= fun os -> match os with
  | "linux" -> version_linux ()
  | "macos" -> version_macos ()
  | "win32" | "cygwin" -> version_windows ()
  | "freebsd" -> uname env aim (Cmd.v "-U")
  | _ -> uname env aim (Cmd.v "-r")
  in
  Ok (version_normalize @@ warn_if_error version ~use:"unknown")

let version =
  let doc = "Operating system or distribution version" in
  let default = Conf.discover version_discover in
  Conf.key ~loc ~doc ~group "os.version" Conv.string_non_empty ~default

(* Operating system family. *)

let family_normalize f = match String.lowercase_ascii f with
| "" -> "unknown"
| v -> v

let family_linux env aim c =
  let like = match os_release_field "ID_LIKE" with
  | None -> None
  | Some likes ->
      match String.cut ~sep:" " likes with
      | None -> Some likes
      | Some (fst, _) -> Some fst
  in
  match like with
  | Some like -> Ok like
  | None -> get_dep env aim distribution c

let family_discover env aim _ c =
  let family = get_dep env aim name c >>= fun os -> match os with
  | "freebsd" | "openbsd" | "netbsd" | "dragonfly" -> Ok "bsd"
  | "win32" | "cygwin" -> Ok "windows"
  | "linux" -> family_linux env aim c
  | os -> Ok os
  in
  Ok (family_normalize @@ warn_if_error family ~use:"unknown")

let family =
  let doc = "Operating system family" in
  let default = Conf.discover family_discover in
  Conf.key ~loc ~doc ~group "os.family" Conv.string_non_empty ~default

(* Executable file extension *)

let exe_ext =
  let doc = "The file extension of executables" in
  let exe_discover env aim _ c = get_dep env aim name c >>| function
  | "win32" | "cygwin" -> ".exe"
  | _ -> ""
  in
  let default = Conf.discover exe_discover in
  Conf.key ~loc ~doc ~group "os.exe_ext" Conv.string ~default

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
