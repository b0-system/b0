(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let loc = Def.Loc.lib "B0_c"
let group = Conf.Group.v ~loc "c" ~doc:"C language support"

(* FIXME needs to be fixed at the Conf api level *)
let get_dep env aim k c =
  Conf.get_effective env aim k c >>| fun (_, a) -> a

let warn_if_error e ~use:v =
  Log.on_error_msg e ~level:Log.Warning ~use:(fun _ -> v)

let toolchain_conv =
  let ts = ["cc", `Cc; "mingw", `Mingw; "msvc", `Msvc] in
  Conv.enum ~docv:"TOOLCHAIN" ts

let toolchain =
  (* FIXME alts substitution *)
  let doc = "The C toolchain to use" in
  let toolchain_discover env aim _ c =
    get_dep env aim B0c_os.name c >>| function
    | "win32" -> `Msvc | _ -> `Cc
  in
  let default = Conf.discover toolchain_discover in
  Conf.key ~loc ~doc ~group "c.toolchain" toolchain_conv ~default

let asm_ext =
  let doc = "The file extension for assembly files" in
  let asm_discover env aim _ c = get_dep env aim toolchain c >>| function
  | `Cc | `Mingw -> ".s" | `Msvc -> ".asm"
  in
  let default = Conf.discover asm_discover in
  Conf.key ~loc ~doc ~group "c.asm_ext" Conv.string ~default

let obj_ext =
  let doc = "The file extension for C object files" in
  let obj_discover env aim _ c = get_dep env aim toolchain c >>| function
  | `Cc | `Mingw -> ".o" | `Msvc -> ".obj"
  in
  let default = Conf.discover obj_discover in
  Conf.key ~loc ~doc ~group "c.obj_ext" Conv.string ~default

let lib_ext =
  let doc = "The file extension for C static libraries" in
  let lib_discover env aim _ c = get_dep env aim toolchain c >>| function
  | `Cc | `Mingw -> ".a" | `Msvc -> ".lib"
  in
  let default = Conf.discover lib_discover in
  Conf.key ~loc ~doc ~group "c.lib_ext" Conv.string ~default

let dll_ext =
  let doc = "The file extension for C dynamic libraries" in
  let dll_discover env aim _ c = get_dep env aim toolchain c >>| function
  | `Cc -> ".so" | `Mingw | `Msvc -> ".dll"
  in
  let default = Conf.discover dll_discover in
  Conf.key ~loc ~doc ~group "c.dll_ext" Conv.string ~default

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
