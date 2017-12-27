(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

let delete dir =
  OS.Dir.exists dir >>= function
  | true -> OS.Dir.delete ~contents:true dir >>= fun () -> Ok `Ok
  | false -> Ok `Ok

let reset_full ~b0_dir = delete (B0_dir.dir b0_dir)
let reset_cache ~b0_dir ~cache_dir =
  let b0_dir = B0_dir.dir b0_dir in
  let cache = B0b_cli.get_cache_dir ~b0_dir ~cache_dir in
  delete cache

let reset_variant ~b0_dir variant =
  let log = Log.Error in
  let dir = B0_dir.variant_dir b0_dir in
  match B0b_cli.load_variant ~log ~dir variant with
  | Error exit -> Ok exit
  | Ok load ->
      let variant = Variant.of_load load in
      Variant.reset variant >>= fun () ->
      Log.app (fun m -> m "Reset variant %a." Variant.pp_name variant);
      Ok `Ok

let reset_default_variant ~b0_dir =
  let log = Log.Error in
  let variant = B0b_cli.find_variant_name ~cli:None b0_dir in
  match B0b_cli.need_variant_name ~log variant with
  | Error exit -> Ok exit
  | Ok variant -> reset_variant ~b0_dir variant

let reset_all_variants ~b0_dir =
  let dir = B0_dir.variant_dir b0_dir in
  Variant.list ~dir >>= fun variants ->
  let rec loop = function
  | [] -> Ok `Ok
  | l :: ls ->
      match Variant.reset (Variant.of_load l) with
      | Error _ as e -> e
      | Ok () -> loop ls
  in
  loop variants

let reset full cache all variant cache_dir setup =
  let b0_dir = Driver.b0_dir setup in
  let maybe_cache r = if cache then reset_cache ~b0_dir ~cache_dir else Ok r in
  begin match full with
  | true -> reset_full ~b0_dir >>= fun ret -> maybe_cache ret
  | false ->
      match all with
      | true -> reset_all_variants ~b0_dir >>= fun ret -> maybe_cache ret
      | false ->
          match variant with
          | None when not cache -> reset_default_variant ~b0_dir
          | None -> maybe_cache `Ok
          | Some variant ->
              reset_variant ~b0_dir variant >>= fun ret -> maybe_cache ret
  end
  |> B0b_cli.to_exit_code |> B0_driver.Cli.handle_error

(* Command line interface *)

open Cmdliner

let full =
  let doc = "Delete the $(b,_b0) directory." in
  Arg.(value & flag & info ["f"; "full"] ~doc)

let cache =
  let doc = "Delete the build cache." in
  Arg.(value & flag & info ["c"; "cache"] ~doc)

let all =
  let doc = "Reset all variants to their creation state." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let doc = "Reset the build"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Cli.driver_default_exits

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command resets the build. Without options resets
        the current variant to its creation state.";
    `S Manpage.s_options;
    `S B0_driver.Cli.s_driver_opts ]

let cmd =
  Term.(pure reset $ full $ cache $ all $ Cli.variant $
        B0_driver.Cli.cache_dir),
  Term.info "reset" ~doc ~sdocs ~exits ~man ~man_xrefs,
  `Driver

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
