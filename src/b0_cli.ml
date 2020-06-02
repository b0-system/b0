(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Cmdliner

module Arg = struct
  let units ?docs ?(doc = "Use unit $(docv).") () =
    Arg.(value & opt_all string [] & info ["u"; "unit"] ?docs ~doc ~docv:"UNIT")

  let x_units ?docs ?(doc = "Exclude unit $(docv). Takes over inclusion.") () =
    let docv = "UNIT" in
    Arg.(value & opt_all string [] & info ["x"; "x-unit"] ?docs ~doc ~docv)

  let packs ?docs ?(doc = "Use pack $(docv).")  () =
    Arg.(value & opt_all string [] & info ["p"; "pack"] ?docs ~doc ~docv:"PACK")

  let x_packs ?docs ?(doc = "Exclude pack $(docv). Takes over inclusion.") () =
    let docv = "PACK" in
    Arg.(value & opt_all string [] & info ["X"; "x-pack"] ?docs ~doc ~docv)
end

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
