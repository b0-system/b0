(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

val read :
  Fpath.t ->
  (string * Digest.t * String.Set.t * (string * Digest.t) list, string) result
(** [read cmi] is [Ok (name, digest, names, deps)] with
    {ul
    {- [name] the module name of the interface.}
    {- [digest] the digest of the inteface.}
    {- [names] are the unqualified module names defined in the interface,
       including [name] itself. This is slightly incomplete because it
       stops at module aliases: these would need to be resolved to
       further cmis to find the names therein.}
    {- [deps] are the digested module interfaces imported by this
       interface.}} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers

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
