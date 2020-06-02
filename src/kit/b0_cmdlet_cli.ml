(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Cmdliner

let info ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?doc ?version cmdlet =
  let doc = Option.value ~default:(B0_cmdlet.doc cmdlet) doc in
  let name = B0_cmdlet.name cmdlet in
  Term.info ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?version name ~doc

let exit = function
| `Ok c -> c
| `Help | `Version -> B0_cmdlet.Exit.ok
| `Error `Term -> B0_cmdlet.Exit.some_error
| `Error `Exn -> B0_cmdlet.Exit.Code Term.exit_status_internal_error
| `Error `Parse -> B0_cmdlet.Exit.Code Term.exit_status_cli_error

let run cmdlet ~argv t = exit @@ Term.eval ~argv (t, info cmdlet)

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
