(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let loc = Def.Loc.lib "B0_care"

let exe =
  let doc = "true if the file is executable" in
  Fpath.Meta.Key.v ~loc "b0.exe" Conv.bool () ~doc

type install =
  [ `Bin | `Doc | `Etc | `Lib | `Lib_root | `Libexec | `Libexec_root
  | `Man | `Misc | `Sbin | `Share | `Share_root | `Stublibs
  | `Other of Fpath.t ] * Fpath.t option

let install_dst_conv =
  let parse = function
  | "bin" -> Ok `Bin | "doc" -> Ok `Doc | "etc" -> Ok `Etc | "lib" -> Ok `Lib
  | "lib_root" -> Ok `Lib_root | "libexec" -> Ok `Libexec
  | "libexec_root" -> Ok `Libexec_root | "man" -> Ok `Man | "misc" -> Ok `Misc
  | "sbin" -> Ok `Sbin | "share" -> Ok `Share | "share_root" -> Ok `Share_root
  | "stublibs" -> Ok `Stublibs
  | name -> Fpath.of_string name >>= fun name -> Ok (`Other name)
  in
  let to_string = function
  | `Bin -> "bin" | `Doc -> "doc" | `Etc -> "etc" | `Lib -> "lib"
  | `Lib_root -> "lib_root" | `Libexec -> "libexec"
  | `Libexec_root -> "libexec_root" | `Man -> "man" | `Misc -> "misc"
  | `Sbin -> "sbin" | `Share -> "share" | `Share_root -> "share_root"
  | `Stublibs -> "stublibs" | `Other name -> Fpath.to_string name
  in
  let pp ppf v = Fmt.pf ppf "%s" (to_string v) in
  let text = Conv.text (parse, pp) in
  Conv.v text

let install_conv : install Conv.t = Conv.(pair install_dst_conv (option fpath))

let install =
  let doc = "Install location" in
  Fpath.Meta.Key.v ~loc "b0.install" install_conv () ~doc

let dist =
  let doc = "Keep built file for distribution" in
  Fpath.Meta.Key.v ~loc "b0.dist" Conv.bool () ~doc

(* Build unit metadata *)

module Unit = struct
  let exe =
    let doc = "A unit with executable outcomes" in
    Unit.Meta.Key.v ~loc "b0.exe" Conv.bool () ~doc

  let lib =
    let doc = "A unit with library outcomes" in
    Unit.Meta.Key.v ~loc "b0.lib" Conv.bool () ~doc

  let test =
    let doc = "A unit with testing outcomes" in
    Unit.Meta.Key.v ~loc "b0.test" Conv.bool () ~doc

  let bench =
    let doc = "A unit with benchmarking outcomes" in
    Unit.Meta.Key.v ~loc "b0.bench" Conv.bool () ~doc

  let doc =
    let doc = "A unit with documentation outcomes" in
    Unit.Meta.Key.v ~loc "b0.doc" Conv.bool () ~doc

  let build =
    let doc = "A unit with build system outcomes" in
    Unit.Meta.Key.v ~loc "b0.build" Conv.bool () ~doc

  let dev =
    let doc = "A unit with development time outcomes" in
    Unit.Meta.Key.v ~loc "b0.dev" Conv.bool () ~doc
end

(* Operating system information. *)

module OS = B0c_os

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
